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

simplify_topology <- function(graph, edge_bundles) {
  undirected_network <- graph %>% 
    decorate_graph(edge_bundles, E(graph)$distance) %>% 
    as.undirected(edge.attr.comb = list(id = "first", 
                                        label = "first", 
                                        is_bridge = "first", 
                                        distance = "first", 
                                        within_boundaries = "first",
                                        bridge_id = "first", "ignore")) %>% 
    as_tbl_graph()
  
  two_degree <- get_prunable_nodes(undirected_network)
  
  while (length(two_degree > 0)) {
    s1 <- two_degree[1]
    endpoints <- adjacent_vertices(undirected_network, v = s1)[[1]]
    edges_to_remove <- incident_edges(undirected_network, v = s1)[[1]]
    
    new_distance <- sum(edge_attr(undirected_network, "distance", index = edges_to_remove))
    new_bridge_id <- first(na.omit(edge_attr(undirected_network, "bridge_id", index = edges_to_remove)))
    new_bridge_label <- first(na.omit(edge_attr(undirected_network, "label", index = edges_to_remove)))
    new_bridge_in_bounds <- any(edge_attr(undirected_network, "within_boundaries", index = edges_to_remove))
    new_bridge_is_bridge <- any(edge_attr(undirected_network, "is_bridge", index = edges_to_remove))
    
    undirected_network <- add_edges(undirected_network, endpoints, 
                                    attr = list(distance = new_distance, 
                                                bridge_id = new_bridge_id,
                                                label = new_bridge_label,
                                                within_boundaries = new_bridge_in_bounds,
                                                is_bridge = new_bridge_is_bridge))
    undirected_network <- delete_vertices(undirected_network, v = s1)
    
    two_degree <- get_prunable_nodes(undirected_network)
    message("Remaining nodes: ", length(two_degree))
  }
  
  as_tbl_graph(undirected_network)
}

get_prunable_nodes <- function(graph) {
  which(!V(graph)$pathfinder.interface & degree(graph) == 2)
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
    arrange(node_index)
}

interface_distance_matrix <- function(graph, bridge_point_map, keep_paths = c("all", "inter", "intra")) {
  stopifnot(inherits(graph, "pathfinder_graph"))
  
  interface_points <- which(V(graph)$pathfinder.interface)
  
  # Calculate full distance matrices between all interface points
  all_distances <- distances(graph, v = interface_points,
                                    to = interface_points, mode = "out", 
                                    weights = E(graph)$pathfinder.distance)
  rownames(all_distances) <- interface_points
  colnames(all_distances) <- interface_points
  
  plan(multiprocess)
  suppressWarnings({
    all_paths <- future_map(interface_points, ~shortest_paths(graph,
                                                              from = .x,
                                                              to = interface_points, 
                                                              mode = "out", output = "epath", 
                                                              weights = E(graph)$pathfinder.distance),
                            .progress = TRUE)
  })
  
  orig_edge_ids <- edge_attr(graph, "pathfinder.edge_id")
  
  named_all_paths <- map(all_paths, function(x) {
    map(x$epath, function(y) {
      orig_edge_ids[as.integer(y)]
    }) %>% 
      set_names(interface_points)
  }) %>% 
    set_names(interface_points)
  
  # Null out all intra-bridge distances
  diag(all_distances) <- NA_real_
  
  if (keep_paths == "all") {
    # Do nothing
  } else if (keep_paths == "inter") {
    # Remove all distances for intra-bridge points
    for (bi in unique(bridge_point_map$bridge_id)) {
      mutually_exclusive_nodes <- as.character(intersect(bridge_point_map$node_index[bridge_point_map$bridge_id == bi], interface_points))
      all_distances[mutually_exclusive_nodes, mutually_exclusive_nodes] <- NA_real_
    }
  } else if (keep_paths == "intra") {
    # Remove all distances for inter-bridge poitns
    matrix_positions <- expand.grid(as.integer(rownames(all_distances)), as.integer(colnames(all_distances)))
    is_coincident <- mapply(function(v1, v2) {
      any(bridge_point_map$bridge_id[bridge_point_map$node_index == v1] %in% bridge_point_map$bridge_id[bridge_point_map$node_index == v2])
    }, matrix_positions$Var1, matrix_positions$Var2, SIMPLIFY = TRUE)
    stopifnot(inherits(is_coincident, "logical"))
    all_distances[!is_coincident] <- NA_real_
  } else {
    stop(paste0(keep_paths, " is not a valid value for keep_paths. Use 'all', 'inter', or 'intra'."))
  }
  
  list(
    distance_matrix = all_distances,
    path_list = named_all_paths
  )
}
