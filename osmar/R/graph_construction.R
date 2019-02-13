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
