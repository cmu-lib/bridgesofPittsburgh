# Data utilities ----

get_osm_bbox <- function(xlim, ylim) {
  content(GET(str_glue("https://overpass-api.de/api/map?bbox={xlim[1]},{ylim[1]},{xlim[2]},{ylim[2]}")), as = "text", encoding = "UTF-8")
}

read_osm_response <- function(raw_response) {
  tfile <- tempfile()
  write_lines(raw_response, path = tfile)
  get_osm(complete_file(), source = osmsource_file(tfile))
}

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

# Graph utilities ----

remove_unreachable_nodes <- function(graph) {
  graph %>%
    activate(nodes) %>%
    filter(!(node_is_isolated()))
}

# Weights by carteisan distance of from and to nodes
# the "conversion" factor is
weight_by_distance <- function(graph) {
  graph %>%
    activate(edges) %>%
    mutate(distance = geosphere::distGeo(
      p1 = cbind(.N()$lon[from], .N()$lat[from]),
      p2 = cbind(.N()$lon[to], .N()$lat[to])))
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

get_interface_points <- function(graph) {
  which(V(graph)$is_interface)
}

get_starting_points <- function(graph, interface_points) {
  # Pick one interface point per unique bridge
  point_bridges <- map_df(set_names(interface_points), function(p) {
    ei <- E(graph)[.from(p)]
    unique_bridges <- unique(edge_attr(graph, "bridge_id", ei))
    data_frame(
      bridge_id = unique_bridges
    )
  }, .id = "point")
  
  # Take the first point for each bridge
  point_bridges %>% 
    filter(!is.na(bridge_id)) %>% 
    distinct(bridge_id, .keep_all = TRUE) %>% 
    pull(point) %>% 
    as.integer()
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

  edge_keys <- c("name", "access", "highway", "bridge", "bridge_name", "oneway")

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

# Keep all edges and nodes, but only mark bridges that are within node boundaries
filter_bridges_to_nodes <- function(graph, node_ids) {
  node_indices <- node_number(graph, node_ids) %>% na.omit()
  
  graph %>%
    activate(edges) %>%
    mutate(
      within_boundaries = from %in% node_indices | to %in% node_indices,
      is_bridge = if_else(within_boundaries, is_bridge, FALSE),
      bridge_id = if_else(within_boundaries, bridge_id, NA_character_)
    )
}

# Filter edges down to the desired paths based on OSM tags (e.g. no sidewalks,
# no private roads, etc). This is for removing edges entirely from the network,
# not to be confused with mark_required_edges() which only makrs those that will
# eventually be required for RPP
filter_to_allowed_paths <- function(graph) {
  excluded_highways <- c("pedestrian", "footway", "cycleway", "steps", "track", "elevator", "bus_stop", "construction", "no", "escape", "proposed", "raceway", "services")

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
      )
    )
}

# Find relations that are bridges, and join their IDs to the edges
add_parent_bridge_relations <- function(graph, raw_osm) {
  bridge_relations <- raw_osm$relations$tags %>%
    filter(k == "type", v == "bridge") %>%
    left_join(raw_osm$relations$refs, by = "id") %>%
    filter(type == "way") %>%
    mutate_at(vars(id, ref), as.character) %>%
    select(name = ref, bridge_relation = id) %>% 
    distinct()

  graph %>%
    activate(edges) %>%
    left_join(bridge_relations, by = "name")
}

# After marking bridges, mark all nodes that are interface nodes for bridges, having at least one edge that is a bridge, and at least one edge that is NOT a bridge
mark_interface_nodes <- function(graph) {
  plan(multiprocess)
  interface_results <- future_map(seq_len(vcount(graph)), is_node_interface, edges = as_tibble(graph, "edges"))
  V(graph)$is_interface <- map_lgl(interface_results, "is_interface")
  
  graph
}

is_node_interface <- function(i, edges) {
  message(i)
  tangent_edges <- which(edges[["from"]] == i | edges[["to"]] == i)
  outbound_edges <-  which(edges[["from"]] == i)
  
  has_bridge <- any(edges[["is_bridge"]][outbound_edges])
  has_non_bridge <- any(!(edges[["is_bridge"]][tangent_edges]))
  has_multi_bridge <- n_distinct(edges[["bridge_id"]][outbound_edges], na.rm = TRUE) >= 2
  is_interface <- (has_bridge & has_non_bridge) | has_multi_bridge
  
  list(is_interface = is_interface)
}

enrich_osmar_graph <- function(raw_osmar, graph_osmar, in_pgh_nodes = NULL, limits = NULL, keep_full = TRUE) {
  osmar_nodes <- osm_node_attributes(raw_osmar)
  osmar_edges <- osm_edge_attributes(raw_osmar)

  graph <- as_tbl_graph(graph_osmar, directed = TRUE) %>%
    activate(nodes) %>%
    left_join(osmar_nodes, by = c("name" = "id")) %>%
    activate(edges) %>%
    mutate_at(vars(name), as.character) %>%
    left_join(osmar_edges, by = c("name" = "id")) %>%
    select(-weight) %>% 
    filter_to_allowed_paths() %>%
    add_parent_bridge_relations(raw_osmar) %>%
    mark_bridges() %>%
    remove_unreachable_nodes() %>% 
    weight_by_distance()
    
  # Trim graph down to specified nodes or specified geographic limits
  if (!is.null(in_pgh_nodes) & keep_full) graph <- filter_bridges_to_nodes(graph, in_pgh_nodes)
  if (!is.null(in_pgh_nodes) & !keep_full) graph <- filter_to_nodes(graph, in_pgh_nodes)
  if (!is.null(limits)) graph <- filter_to_limits(graph, limits)
  
  # Add reverse edges of non-one-way streets
  reversed_edges <- graph %>% 
    as_tibble("edges") %>% 
    filter(is.na(oneway) | oneway != "yes") %>% 
    select(from = to, to = from, everything())
  graph <- bind_edges(graph, reversed_edges)

  # Give a final persistent ID for edges
  graph %>% 
    select_main_component() %>% 
    activate(edges) %>% 
    mutate(.id = row_number())
}
