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
                                        bridge_id = "first", "ignore"))
  
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