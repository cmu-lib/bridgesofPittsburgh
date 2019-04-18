# Convert OSM nodes to sf so that we can determine which ones are withi the PGH
# administrative boundary
osm_nodes_to_sf <- function(osm) {
  nodes_sf <- cbind(osm$nodes$attrs$lon, osm$nodes$attrs$lat) %>% 
    st_multipoint() %>% 
    st_sfc(crs = 4326) %>%
    st_cast("POINT")
  
  res <- select(osm$nodes$attrs, id)
  st_geometry(res) <- nodes_sf
  res
}

nodes_within_boundaries <- function(nodes, boundaries) {
  res <- st_intersects(nodes, boundaries, sparse = FALSE) %>% 
    apply(1, any)
  
  nodes$id[which(res)]
}


# Match UIDs for nodes even after a graph has been modified and the node index
# in a given graph has changed.
node_number <- function(graph, name) {
  match(name, as_tibble(graph, active = "nodes")[["name"]])
}

# Only keep edges that contain at least one node within the spedified set
filter_to_nodes <- function(graph, node_ids) {
  node_indices <- node_number(graph, node_ids) %>% na.omit()
  
  graph %>%
    activate(edges) %>%
    filter(from %in% node_indices | to %in% node_indices) %>%
    remove_unreachable_nodes()
}

# Keep all edges and nodes, but only mark bridges that are within node boundaries
filter_bridges_to_nodes <- function(graph, node_ids) {
  node_indices <- na.omit(match(node_ids, vertex_attr(graph, "id")))
  
  res <- graph %>%
    activate(edges) %>%
    mutate(
      within_boundaries = from %in% node_indices | to %in% node_indices,
      is_bridge = if_else(within_boundaries, is_bridge, FALSE),
      bridge_id = if_else(within_boundaries, bridge_id, NA_real_)
    )
}

write_edgelist <- function(graph) {
  
  if (all(!(edge_attr_names(graph) %in% c("bridge_relation", "bridge", "highway")))) {
    E(graph)$bridge_relation <- NA_real_
    E(graph)$bridge <- NA_character_
    E(graph)$highway <- NA_character_
  }
  
  as_tibble(graph, "edges") %>% 
    mutate(edge_id = row_number()) %>% 
    select(from, to, id = edge_id, 
           osm_way_id = id, 
           osm_bridge_relation_id = bridge_relation, 
           osm_bridge = bridge, 
           osm_highway = highway, 
           osm_label = label, 
           bridge_id, distance, 
           within_boundaries)
}

write_nodelist <- function(graph) {
  as_tibble(graph, "nodes") %>% 
    mutate(node_id = row_number()) %>% 
    select(id = node_id, osm_node_id = id, lat, lon, osm_label = label, neighborhood)
}

bridge_node_correspondence <- function(graph, edge_bundles) {
  decorated_graph <- decorate_graph(graph, edge_bundles, E(graph)$distance)
  interface_points <- which(V(decorated_graph)$pathfinder.interface)
  
  as_tibble(decorated_graph, "edges") %>% 
    filter(!is.na(bridge_id)) %>% 
    select(from, to, bridge_id) %>% 
    gather(direction, node_index, from:to) %>% 
    select(node_index, bridge_id, direction) %>% 
    filter(node_index %in% interface_points) %>% 
    arrange(node_index)
}

full_matrix <- function(graph) {
  general_distance_matrix(graph)
}

inter_bridge_matrix <- function(graph, node_sibling_lookup) {
  bridgeless_graph <- delete_edges(graph, edges = which(!is.na(E(graph)$pathfinder.bundle_id)))
  general_distance_matrix(bridgeless_graph, node_sibling_lookup)
}

intra_bridge_matrix <- function(graph) {
  bridge_only_graph <- delete_edges(graph, edges = which(is.na(E(graph)$pathfinder.bundle_id)))
  general_distance_matrix(bridge_only_graph)
}

general_distance_matrix <- function(graph, node_sibling_lookup = NULL) {
  interface_points <- which(V(graph)$pathfinder.interface)
  orig_edge_ids <- edge_attr(graph, "pathfinder.edge_id")
  distance_matrix <- distances(graph, 
                               v = interface_points,
                               to = interface_points,
                               mode = "out",
                               weights = E(graph)$pathfinder.distance)
  
  diag(distance_matrix) <- NA_real_
  colnames(distance_matrix) <- interface_points
  rownames(distance_matrix) <- interface_points
  
  if (!is.null(node_sibling_lookup)) {
    for (i in as.character(interface_points)) {
      exclusion_points <- node_sibling_lookup[[i]]
      distance_matrix[exclusion_points, exclusion_points] <- NA_real_
    }
  }
  
  suppressWarnings({
    paths <- parallel::mclapply(interface_points, function(x) {
      shortest_paths(graph,
                     from = x,
                     to = interface_points, 
                     mode = "out", output = "epath", 
                     weights = E(graph)$pathfinder.distance)
    }, mc.preschedule = TRUE, mc.cores = 8)
  })
    
    named_all_paths <- lapply(paths, function(x) {
      lapply(x$epath, function(y) {
        orig_edge_ids[as.integer(y)]
      }) %>% 
        set_names(interface_points)
    }) %>% 
      set_names(interface_points)
  
    list(
      distance_matrix = distance_matrix,
      path_list = named_all_paths
    )
}
