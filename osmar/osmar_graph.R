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

tiny_limits <- list(xlim = c(-80.0106, -79.9872), ylim = c(40.4429, 40.4551))

pgh_plan <- drake_plan(
  
  # Shapefile for PGH boundaries
  pgh_boundary_shp = as(readOGR("osmar/input_data/Pittsburgh_Buffered_Boundary/"), "SpatialPolygons"),
  pgh_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/input_data/pgh_osm.xml"))),
  pgh_points_sp = as_sp(pgh_raw, "points"),
  point_overlap = over(pgh_points_sp, pgh_boundary_shp),
  in_bound_points = names(na.omit(point_overlap)),
  in_bound_pgh = subset(pgh_raw, ids = find_up(pgh_raw, node(in_bound_points))),
  
  # Sample Graph
  tiny_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/input_data/tiny.xml"))),
  tiny_graph = as_igraph(tiny_raw),
  tiny_bridges = find_bridge_waysets(tiny_raw),
  tiny_tidy_graph = enrich_osmar_graph(tiny_raw, tiny_graph, tiny_bridges, limits = tiny_limits),
  tiny_plot = bridge_plot(tiny_tidy_graph),
  tiny_termini = way_termini(tiny_raw),
  tiny_plot_image = ggsave(tiny_plot, filename = file_out("osmar/output_data/tiny_image.png"), width = 20, height = 20),
  tiny_needs_rewiring = bridges_to_rewire(tiny_tidy_graph),
  tiny_rewired_graph = rewire_bridges(tiny_tidy_graph, 
                                      bridges = tiny_needs_rewiring,
                                      termini = tiny_termini),
  final_tiny_graph = tiny_rewired_graph %>% weight_by_distance(),
  final_tiny_plot = bridge_plot(final_tiny_graph),
  final_tiny_plot_image = ggsave(final_tiny_plot, filename = file_out("osmar/output_data/final_tiny_plot_image.png"), width = 40, height = 30),
  
  
  # Full Graph
  pgh_graph = as_igraph(in_bound_pgh),
  pgh_bridges = find_bridge_waysets(in_bound_pgh),
  pgh_tidy_graph = enrich_osmar_graph(in_bound_pgh, pgh_graph, pgh_bridges),
  pgh_termini = way_termini(in_bound_pgh),
  pgh_plot = bridge_plot(pgh_tidy_graph),
  pgh_plot_image = ggsave(pgh_plot, filename = file_out("osmar/output_data/pgh_image.png"), width = 40, height = 30),
  
  pgh_needs_rewiring = bridges_to_rewire(pgh_tidy_graph),
  rewired_pgh_graph = rewire_bridges(pgh_tidy_graph, bridges = pgh_needs_rewiring, termini = pgh_termini),
  rewired_pgh_plot = bridge_plot(rewired_pgh_graph),
  rewired_pgh_plot_image = ggsave(rewired_pgh_plot, filename = file_out("osmar/output_data/rewired_pgh_image.png"), width = 40, height = 30),
  
  # Finalize output plot
  final_pgh_graph = rewired_pgh_graph %>% weight_by_distance() %>% select_main_component(),
  final_pgh_nodes = write_csv(as_tibble(final_pgh_graph, "nodes"), path = file_out("osmar/output_data/rewired_pgh_nodes.csv"), na = ""),
  final_pgh_edges = write_csv(as_tibble(final_pgh_graph, "edges") %>% select(-weight), path = file_out("osmar/output_data/rewired_pgh_edges.csv"), na = ""),
  final_pgh_plot = bridge_plot(final_pgh_graph),
  final_pgh_plot_image = ggsave(final_pgh_plot, filename = file_out("osmar/output_data/final_pgh_image.png"), width = 40, height = 30)
)

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

tangent_ways <- function(src, osm_nodes) {
  upsearch <- find_up(src, node(osm_nodes))
  upsearch[["way_ids"]]
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
  edge_keys <- c("name", "highway", "bridge", "bridge_name", "bridge:name", "wikipedia")
  
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

only_roadways <- function(osmar_graph) {
  osmar_graph %>% 
    activate(edges) %>% 
    filter(!is.na(highway))
}

enrich_osmar_graph <- function(raw_osmar, graph_osmar, bridges, in_pgh_nodes = NULL, in_pgh_edges = NULL, limits = NULL) {
  osmar_nodes <- osm_node_attributes(raw_osmar)
  osmar_edges <- osm_edge_attributes(raw_osmar)
  
  res <- as_tbl_graph(graph_osmar, directed = FALSE) %>% 
    activate(nodes) %>% 
    left_join(osmar_nodes, by = c("name" = "id")) %>% 
    activate(edges) %>% 
    mutate_at(vars(name), as.character) %>% 
    left_join(osmar_edges, by = c("name" = "id")) %>% 
    left_join(bridges, by = c("name" = "way_id")) %>% 
    mutate(
      is_bridge = !is.na(bridge_id),
      rewired = if_else(is_bridge, FALSE, NA),
      synthetic = NA)
  
  if (!is.null(in_pgh_nodes)) {
    # Only keep nodes that are inside a defined boundary near the Pittsburgh
    # administrative border
    res <- res %>% 
      activate(nodes) %>% 
      filter(name %in% in_pgh_nodes)
  }
  
  if (!is.null(in_pgh_edges)) {
    res <- res %>% 
      activate(edges) %>% 
      filter(name %in% in_pgh_edges)
  } 
  
  if (!is.null(limits)) {
    res <- res %>%
      activate(nodes) %>% 
      filter(
        between(lon, left = limits$xlim[1], right = limits$xlim[2]),
        between(lat, left = limits$ylim[1], right = limits$ylim[2]))
  }
  
  # Finally, filter only down to the roadways
  only_roadways(res)
}

find_bridge_waysets <- function(raw_osm) {
  bridge_ways <- raw_osm$ways$tags %>% 
    filter(k == "bridge", v == "yes")
  
  # Pull relations that are classed as bridges
  bridge_relations <- raw_osm$relations$tags %>% 
    filter(k == "type", v == "bridge") %>% 
    left_join(raw_osm$relations$refs, by = "id") %>% 
    filter(type == "way") %>% 
    mutate(
      id_type = "osm",
      bridge_id = paste(id_type, id, sep = "-")) %>% 
    select(bridge_id, way_id = ref, id_type) %>% 
    # Only keep those ways that are ALSO classed as bridges (to avoid pulling in pylons etc)
    semi_join(bridge_ways, by = c("way_id" = "id"))
  
  # Pull ways that are themselves bridges with no parent relation
  bridge_singleton_ways <- bridge_ways %>% 
    # If the way is in an identified relation, don't give it a new ID
    anti_join(bridge_relations, by = c("id" = "way_id")) %>% 
    mutate(
      id_type = "pgh",
      bridge_id = paste(id_type, row_number(), sep = "-")) %>% 
    select(bridge_id, way_id = id, id_type)
  
  union_bridge_ways <- bind_rows(bridge_singleton_ways, bridge_relations) %>% 
    mutate_at(vars(way_id), as.character)
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
        synthetic == TRUE ~ "green",
        rewired == TRUE ~ "red",
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
    filter(id_type == "osm") %>% 
    pull(bridge_id) %>% 
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
      label = NA_character_)
  
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
  
  presimplified_graph <- reduce(waylist, function(x, y) {
    terminals <- termini %>% filter(way_id == y)
    
    singleton_rewire_handler(x, way_id = y, 
                             start_node = terminals[["start_node"]], 
                             end_node = terminals[["end_node"]])
  }, .init = osm_graph)
  bridge_plot(presimplified_graph)
  cluster_results <- cluster_points(presimplified_graph, b)
  
  # If cluster_results returned null, then exit early
  if (is.null(cluster_results)) return(presimplified_graph)
  
  # Remove unwanted edges
  new_graph <- presimplified_graph %>% 
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
    bind_edges(indexed_edges)
}

node_number <- function(graph, name) {
  match(name, as_tibble(graph, active = "nodes")[["name"]])
}

# Rewire a simple multi-Node Way with an explicit start and end node
singleton_rewire_handler <- function(graph, way_id, start_node, end_node) {
  # Collect metadata from the original way, then create a new edge connecting
  # the start and end nodes
  new_edge <- graph %>% 
    as_tibble(active = "edges") %>% 
    filter(name == way_id) %>% 
    slice(1) %>% 
    select(name, bridge_id, label, wikipedia, bridge_id, id_type, is_bridge) %>% 
    mutate(rewired = TRUE) %>% 
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


select_main_component <- function(graph) {
  graph %>% 
    activate(nodes) %>% 
    mutate(component = group_components()) %>% 
    filter(component == 1)
}
