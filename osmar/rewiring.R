rewiring_plan <- drake_plan(
  pgh_termini = way_termini(pgh_raw),
  pgh_needs_rewiring = bridges_to_rewire(pgh_tidy_graph),
  rewired_pgh_graph = rewire_bridges(pgh_tidy_graph, bridges = pgh_needs_rewiring, termini = pgh_termini),
  
  # Finalize output plot
  final_pgh_graph = rewired_pgh_graph %>% weight_by_distance() %>% select_main_component() %>% mark_required_edges(),
)

# Rewiring ----

bridges_to_rewire <- function(graph) {
  graph %>%
    as_tibble("edges") %>%
    filter(!is.na(bridge_relation)) %>%
    pull(bridge_relation) %>%
    unique() %>%
    na.omit()
}

# Replace each complex bridge edgeset in the graph with a single synthetic edge
# with new endpoints
rewire_bridges <- function(osm_graph, bridges, termini) {
  # Loop through bridge ids to be rewired
  reduce(bridges, rewire_bridge, termini = termini, .init = osm_graph)
}

# From a table of nodes that we know to be the same bridge, cluster into two groups to form
cluster_points <- function(g, bridge_identifier) {
  message(bridge_identifier, "...", appendLF = FALSE)
  
  node_tbl <- g %>%
    activate(edges) %>%
    filter(bridge_id == bridge_identifier) %>%
    remove_unreachable_nodes() %>%
    as_tibble(active = "nodes")
  
  message(nrow(node_tbl), "...", appendLF = FALSE)
  
  # If only two nodes are present, then immediately return the table because no
  # clustering needs to be done
  if (nrow(node_tbl) <= 2) {
    message("skipped")
    return(NULL)
  }
  
  # Use kmeans to find the points most likely to be on either side of a bridge
  # TODO this WILL cause problems when the bridge as defined in OSM is wider
  # than it is long, but we'll have to cross that bridge when we come to it.
  suppressWarnings({
    kmnodes <- node_tbl %>%
      select(name, lat, lon) %>%
      column_to_rownames(var = "name") %>%
      as.matrix() %>%
      kmeans(2)
  })
  
  new_nodes <- kmnodes$centers %>%
    as_tibble() %>%
    mutate(
      name = paste(1:2, bridge_identifier, sep = "-"),
      label = NA_character_,
      synthetic = TRUE)
  
  new_edges <- kmnodes$cluster %>%
    # Use the cluster memberships to synthesize new from-to edge list
    enframe(name = "from_name", value = "to_name") %>%
    mutate(to_name = paste(to_name, bridge_identifier, sep = "-")) %>%
    # Add new edge between the new nodes
    bind_rows(tibble(from_name = new_nodes[[1, "name"]],
                     to_name = new_nodes[[2, "name"]],
                     bridge_id = bridge_identifier)) %>%
    left_join(node_tbl, by = c("from_name" = "name")) %>%
    left_join(new_nodes, by = c("to_name" = "name")) %>%
    # Make sure the new nodes also get a lat & lon
    mutate(
      lat.x = coalesce(lat.x, new_nodes[[1, "lat"]]),
      lon.x = coalesce(lon.x, new_nodes[[1, "lon"]])) %>%
    mutate(
      distance = sqrt((lat.x - lat.y)^2 + (lon.x - lon.y)^2),
      synthetic = TRUE,
      associated_bridge = bridge_identifier) %>%
    select(from_name, to_name, bridge_id, distance, synthetic, associated_bridge)
  
  message("done")
  
  list(
    nodes = new_nodes,
    edges = new_edges
  )
}

# For a given bridge ID, create two new endpoints, connect the original 2+
# terminal points to these new endpoints, and remove the original edges/nodes
rewire_bridge <- function(osm_graph, b, termini) {
  
  # First, simplify the Ways belonging to the bridge
  # Get all Ways belonging to the bridge
  waylist <- osm_graph %>%
    as_tibble("edges") %>%
    filter(bridge_id == b) %>%
    pull(name) %>%
    unique()
  
  cluster_results <- cluster_points(osm_graph, b)
  
  # If cluster_results returned null, then exit early
  if (is.null(cluster_results)) return(osm_graph)
  
  # Remove unwanted edges
  new_graph <- osm_graph %>%
    activate(edges) %>%
    filter(is.na(bridge_id) | bridge_id != b) %>%
    # Bind the newly-created nodes
    activate(nodes) %>%
    bind_nodes(cluster_results$nodes)
  
  # Get proper node indices now that the new synthetic nodes have been added
  indexed_edges <- cluster_results$edges %>%
    mutate(
      from = node_number(new_graph, from_name),
      to = node_number(new_graph, to_name))
  
  new_graph %>%
    # And wire them up to the old nodes
    bind_edges(indexed_edges) %>%
    activate(nodes) %>%
    # Remove nodes of degree 1 to get rid of extra edges that go nowhere
    mutate(degree = centrality_degree(mode = "all")) %>%
    filter(degree > 1)
}

# Rewire a simple multi-Node Way with an explicit start and end node
singleton_rewire_handler <- function(graph, way_id, start_node, end_node) {
  # Collect metadata from the original way, then create a new edge connecting
  # the start and end nodes
  new_edge <- graph %>%
    as_tibble(active = "edges") %>%
    filter(name == way_id) %>%
    slice(1) %>%
    select(name, bridge_id, label, bridge_relation, is_bridge) %>%
    mutate(straightened = TRUE) %>%
    mutate(
      from = node_number(graph, start_node),
      to = node_number(graph, end_node)) %>%
    assert(is_uniq, name)
  
  graph %>%
    # Remove original edges representing the way
    activate(edges) %>%
    filter(is.na(name) | name != way_id) %>%
    # Add the new edge and return the graph
    bind_edges(new_edge)
}

straighten_graph <- function(graph) {
  node_sequence <- seq_len(igraph::vcount(graph))
  reduce(node_sequence, straighten_neighborhood, .init = graph)
}

straighten_neighborhood <- function(graph, node_id) {
  sel_degree <- igraph::degree(graph, node_id)
  if (sel_degree != 2) return(graph)
  
  res <- graph %>%
    morph(to_local_neighborhood, node = node_id)
  
  old_edges <- res$neighborhood %>%
    as_tibble("edges")
  
  new_neighbors <- res$neighborhood %>%
    as_tibble("nodes") %>%
    filter(.tidygraph_node_index != node_id)
  
  new_links <- data_frame(from = new_neighbors$.tidygraph_node_index[1], to = new_neighbors$.tidygraph_node_index[2]) %>%
    bind_cols(select(old_edges, -from, -to, -weight, -.tidygraph_edge_index)[1,]) %>%
    mutate(rewired = TRUE)
  
  graph %>%
    # Remove old edges
    activate(edges) %>%
    filter(!(row_number() %in% old_edges$.tidygraph_edge_index)) %>%
    # Add new synthetic edge
    bind_edges(new_links)
}

mark_required_edges <- function(graph) {
  graph %>%
    activate(edges) %>%
    mutate(
      required = case_when(
        !is.na(bridge_id) & is_bridge ~ TRUE,
        !is.na(bridge_id) & synthetic ~ TRUE,
        TRUE ~ FALSE
      )
    )
}
