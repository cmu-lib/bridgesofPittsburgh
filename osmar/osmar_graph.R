# Drake ----

pkgconfig::set_config(
  "drake::strings_in_dots" = "literals",
  "drake::verbose" = 4)
library(drake)
library(osmar)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(rgdal)
library(assertr)
library(httr)

tiny_limits <- list(xlim = c(-80.0106, -79.9872), ylim = c(40.4429, 40.4551))

# Sample Graph
tiny_plan <- drake_plan(
  tiny_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/input_data/tiny.xml"))),
  tiny_graph = as_igraph(tiny_raw),
  tiny_tidy_graph = enrich_osmar_graph(tiny_raw, tiny_graph, limits = tiny_limits),
  tiny_termini = way_termini(tiny_raw),
  tiny_needs_rewiring = bridges_to_rewire(tiny_tidy_graph),
  tiny_rewired_graph = rewire_bridges(tiny_tidy_graph, 
                                      bridges = tiny_needs_rewiring,
                                      termini = tiny_termini),
  final_tiny_graph = tiny_rewired_graph %>% weight_by_distance() %>% mark_required_edges()
)

plot_plan <- drake_plan(
  tiny_plot = bridge_plot(tiny_tidy_graph),
  tiny_plot_image = ggsave(tiny_plot, filename = file_out("osmar/output_data/tiny_image.svg"), width = 10, height = 10),
  final_tiny_plot = bridge_plot(final_tiny_graph),
  final_tiny_plot_image = ggsave(final_tiny_plot, filename = file_out("osmar/output_data/final_tiny_plot_image.png"), width = 10, height = 10),
  pgh_plot = bridge_plot(pgh_tidy_graph),
  pgh_plot_image = ggsave(pgh_plot, filename = file_out("osmar/output_data/pgh_image.svg"), width = 50, height = 40, limitsize = FALSE),
  rewired_pgh_plot = bridge_plot(rewired_pgh_graph),
  rewired_pgh_plot_image = ggsave(rewired_pgh_plot, filename = file_out("osmar/output_data/rewired_pgh_image.png"), width = 50, height = 40, limitsize = FALSE),
  final_pgh_plot = bridge_plot(final_pgh_graph),
  final_pgh_plot_image = ggsave(final_pgh_plot, filename = file_out("osmar/output_data/final_pgh_image.png"), width = 50, height = 40, limitsize = FALSE)
)

merged_plot_plan <- gather_plan(plot_plan, target = "plots")

large_plan <- drake_plan(
  download_osm = target(
    command = get_osm_bbox("-80.1257,40.3405,-79.7978,40.5407"),
    # Must manually trigger a new data download
    trigger = trigger(command = FALSE, depend = FALSE, file = FALSE)),
  pgh_raw = read_osm_response(download_osm),
  # Shapefile for PGH boundaries
  pgh_boundary_shp = as(readOGR(file_in("osmar/input_data/Pittsburgh_Buffered_Boundary/")), "SpatialPolygons"),
  pgh_points_sp = as_sp(pgh_raw, "points"),
  point_overlap = over(pgh_points_sp, pgh_boundary_shp),
  in_bound_points = names(na.omit(point_overlap)),
  
  # Full Graph
  pgh_graph = as_igraph(pgh_raw),
  pgh_tidy_graph = enrich_osmar_graph(pgh_raw, pgh_graph, in_pgh_nodes = in_bound_points),
  pgh_termini = way_termini(pgh_raw),
  pgh_needs_rewiring = bridges_to_rewire(pgh_tidy_graph),
  rewired_pgh_graph = rewire_bridges(pgh_tidy_graph, bridges = pgh_needs_rewiring, termini = pgh_termini),
  
  # Finalize output plot
  final_pgh_graph = rewired_pgh_graph %>% weight_by_distance() %>% select_main_component() %>% mark_required_edges(),
  final_pgh_nodes = write_csv(as_tibble(final_pgh_graph, "nodes"), path = file_out("osmar/output_data/rewired_pgh_nodes.csv"), na = ""),
  final_pgh_edges = write_csv(as_tibble(final_pgh_graph, "edges") %>% select(-weight), path = file_out("osmar/output_data/rewired_pgh_edges.csv"), na = "")
)

pgh_plan <- bind_plans(
  tiny_plan,
  plot_plan,
  large_plan,
  merged_plot_plan
)

# Data utilities ----

get_osm_bbox <- function(bbox_string) {
  content(GET(paste0("https://overpass-api.de/api/map?bbox=", bbox_string)), as = "text")
}

read_osm_response <- function(raw_response) {
  tfile <- tempfile()
  write_lines(raw_response, path = tfile)
  get_osm(complete_file(), source = osmsource_file(tfile))
}

# Graph utilities ----

remove_unreachable_nodes <- function(graph) {
  graph %>%
    activate(nodes) %>% 
    filter(!(node_is_isolated()))
}

# Weights by carteisan distance of from and to nodes
# the "conversion" factor is 
weight_by_distance <- function(graph, conversion = 91805.38) {
  graph %>% 
    activate(edges) %>% 
    mutate(distance = sqrt((.N()$lat[from] - .N()$lat[to])^2 + 
                           (.N()$lon[from] - .N()$lon[to])^2) * conversion)
}

# Match UIDs for nodes even after a graph has been modified and the node index
# in a given graph has changed.
node_number <- function(graph, name) {
  match(name, as_tibble(graph, active = "nodes")[["name"]])
}

# Keep only the biggest connected component of a graph
select_main_component <- function(graph) {
  graph %>% 
    activate(nodes) %>% 
    mutate(component = group_components()) %>% 
    filter(component == 1)
}

# Identifying bridges ----

osm_node_attributes <- function(src) {
  node_keys <- c("name")
  
  base_attrs <- src$nodes$attrs %>% 
    select(id, lat, lon)
  
  node_tags <- src$nodes$tags %>% 
    filter(k %in% node_keys) %>% 
    mutate_at(vars(v), as.character) %>%
    spread(k, v, drop = TRUE) %>% 
    # To avoid collision with the "name" id used by igraph/tidygraph, use the
    # term "label" for OSM name
    rename(label = name)

  base_attrs %>% 
    left_join(node_tags, by = "id") %>% 
    mutate_at(vars(id), as.character)
}

osm_edge_attributes <- function(src) {
  # Pull basic way tags
  
  edge_keys <- c("name", "access", "highway", "bridge", "bridge_name")
  
  way_tags <- src$ways$tags %>% 
    filter(k %in% edge_keys) %>% 
    mutate_at(vars(v), as.character) %>%
    spread(k, v, drop = TRUE) %>% 
    # To avoid collision with the "name" id used by igraph/tidygraph, use the
    # term "label" for OSM name
    rename(label = name)
  
  way_tags %>% 
    mutate_at(vars(id), as.character)
}

way_termini <- function(src) {
  long_termini <- src$ways$refs %>% 
    group_by(id) %>% 
    mutate(
      index = row_number(),
      first = row_number(index) == 1,
      last = row_number(desc(index)) == 1) %>% 
    filter(first | last) %>% 
    ungroup()
  
  firsts <- long_termini %>% 
    filter(first) %>% 
    select(way_id = id, start_node = ref)
  
  lasts <- long_termini %>% 
    filter(last) %>% 
    select(way_id = id, end_node = ref)
  
  left_join(firsts, lasts, by = "way_id") %>% 
    mutate_all(as.character)
}

# Only keep those nodes in the graph that are within a specified bounding box
filter_to_limits <- function(graph, limits) {
  graph %>%
    activate(nodes) %>% 
    filter(
      between(lon, left = limits$xlim[1], right = limits$xlim[2]),
      between(lat, left = limits$ylim[1], right = limits$ylim[2]))
}

# Only keep edges that contain at least one node within the spedified set
filter_to_nodes <- function(graph, node_ids) {
  node_indices <- node_number(graph, node_ids) %>% na.omit()
  
  graph %>% 
    activate(edges) %>% 
    filter(from %in% node_indices | to %in% node_indices) %>% 
    remove_unreachable_nodes()
}

# Filter edges down to the desired paths based on OSM tags (e.g. no sidewalks,
# no private roads, etc). This is for removing edges entirely from the network,
# not to be confused with mark_required_edges() which only makrs those that will
# eventually be required for RPP
filter_to_allowed_paths <- function(graph) {
  excluded_highways <- c("pedestrian", "footway")
  
  graph %>% 
    activate(edges) %>% 
    filter(
      (is.na(access) | access != "no") &
      (!is.na(highway) & !(highway %in% excluded_highways))
    )
}

# For those edges that have earlier been marked as bridges, designate which are required
mark_bridges <- function(graph) {
  allowed_bridge_attributes <- c("motorway", "primary", "secondary", "tertiary", "trunk")
  
  graph %>% 
    activate(edges) %>% 
    mutate(
      is_bridge = case_when(
        bridge == "yes" & highway %in% allowed_bridge_attributes ~ TRUE,
        # When in doubt, it's not a bridge
        TRUE ~ FALSE),
      bridge_id = case_when(
        is_bridge & !is.na(bridge_relation) ~ bridge_relation,
        is_bridge ~ name,
        TRUE ~ NA_character_
      ),
      straightened = FALSE,
      synthetic = FALSE,
      required = FALSE
    )
}

# Find relations that are bridges, and join their IDs to the edges
add_parent_bridge_relations <- function(graph, raw_osm) {
  bridge_relations <- raw_osm$relations$tags %>% 
    filter(k == "type", v == "bridge") %>% 
    left_join(raw_osm$relations$refs, by = "id") %>% 
    filter(type == "way") %>% 
    mutate_at(vars(id, ref), as.character) %>% 
    select(name = ref, bridge_relation = id)
  
  graph %>% 
    activate(edges) %>% 
    left_join(bridge_relations, by = "name")
}

enrich_osmar_graph <- function(raw_osmar, graph_osmar, in_pgh_nodes = NULL, limits = NULL) {
  osmar_nodes <- osm_node_attributes(raw_osmar)
  osmar_edges <- osm_edge_attributes(raw_osmar)
  
  graph <- as_tbl_graph(graph_osmar, directed = FALSE) %>% 
    activate(nodes) %>% 
    left_join(osmar_nodes, by = c("name" = "id")) %>% 
    activate(edges) %>% 
    mutate_at(vars(name), as.character) %>% 
    left_join(osmar_edges, by = c("name" = "id")) %>% 
    filter_to_allowed_paths() %>% 
    add_parent_bridge_relations(raw_osmar) %>% 
    mark_bridges() %>% 
    remove_unreachable_nodes()
  
  # Finally, trim graph down to specified nodes or specified geographic limits
  if (!is.null(in_pgh_nodes)) graph <- filter_to_nodes(graph, in_pgh_nodes)
  if (!is.null(limits)) graph <- filter_to_limits(graph, limits)
  
  graph
}

# Plotting ----

lat_lon_layout <- function(graph) {
  node_positions <- graph %>% activate(nodes) %>% as_tibble() %>% select(x = lon, y = lat) %>% mutate_at(vars(x, y), scales::rescale)
  create_layout(graph, layout = "manual", node.positions = node_positions)
}

bridge_plot <- function(graph) {
  graph %>% 
    activate(edges) %>% 
    mutate(
      edge_flag = case_when(
        required == TRUE ~ "purple",
        synthetic == TRUE ~ "green",
        straightened == TRUE ~ "red",
        is_bridge == TRUE ~ "blue",
        TRUE ~ "gray"
      )
    ) %>% 
    lat_lon_layout() %>% 
    ggraph(layout = "manual") + 
    geom_edge_link(aes(color = edge_flag)) +
    scale_edge_color_identity() +    
    theme_graph() +
    coord_map()
}

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

select_main_component <- function(graph) {
  graph %>% 
    activate(nodes) %>% 
    mutate(component = group_components()) %>% 
    filter(component == 1)
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
