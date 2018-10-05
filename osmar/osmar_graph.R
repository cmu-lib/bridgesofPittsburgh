# Drake ----

pkgconfig::set_config("drake::strings_in_dots" = "literals")
library(drake)
library(osmar)
library(tidyverse)
library(tidygraph)
library(ggraph)

tiny_limits <- list(xlim = c(-80.0106, -79.9872), ylim = c(40.4429, 40.4551))

pgh_plan <- drake_plan(
  pgh_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/pgh_osm.xml"))),
  pgh_graph = as_igraph(pgh_raw),
  pgh_bridges = find_bridge_waysets(pgh_raw),
  tidy_pgh_graph = enrich_osmar_graph(pgh_raw, pgh_graph, pgh_bridges),
  tiny_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/tiny.xml"))),
  tiny_graph = as_igraph(tiny_raw),
  tiny_bridges = find_bridge_waysets(tiny_raw),
  tiny_tidy_graph = enrich_osmar_graph(tiny_raw, tiny_graph, tiny_bridges, tiny_limits),
  tiny_layout = lat_lon_layout(tiny_tidy_graph),
  tiny_plot = bridge_plot(tiny_layout),
  tiny_termini = way_termini(tiny_raw),
  pgh_termini = way_termini(pgh_raw),
  #tiny_plot_image = target(
   # command = ggsave(tiny_plot, filename = file_out("tiny_image.png"), width = 20, height = 20),
    #trigger = trigger(command = FALSE, depend = FALSE, file = FALSE, missing = TRUE)),
  pgh_layout = lat_lon_layout(tidy_pgh_graph),
  #pgh_plot_image = target(
   # command = ggsave(pgh_plot, filename = file_out("pgh_image.png"), width = 40, height = 30),
    #trigger = trigger(command = FALSE, depend = FALSE, file = FALSE, missing = TRUE))
  pgh_plot = bridge_plot(pgh_layout),
  tiny_unique_bridges = unique_bridges(tiny_tidy_graph),
  pgh_unique_bridges = unique_bridges(tidy_pgh_graph),
  tiny_needs_rewiring = map_lgl(tiny_unique_bridges, needs_rewire, graph = tiny_tidy_graph),
  pgh_needs_rewiring = map_lgl(pgh_unique_bridges, needs_rewire, graph = tidy_pgh_graph)
)

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

enrich_osmar_graph <- function(raw_osmar, graph_osmar, bridges, limits = NULL) {
  osmar_nodes <- osm_node_attributes(raw_osmar)
  osmar_edges <- osm_edge_attributes(raw_osmar)
  
  res <- as_tbl_graph(graph_osmar) %>% 
    activate(nodes) %>% 
    left_join(osmar_nodes, by = c("name" = "id")) %>% 
    activate(edges) %>% 
    mutate_at(vars(name), as.character) %>% 
    left_join(osmar_edges, by = c("name" = "id")) %>% 
    left_join(bridges, by = c("name" = "way_id")) %>% 
    mutate(is_bridge = !is.na(bridge_id))
  
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

bridge_plot <- function(layout) {
  ggraph(layout, layout = "manual") + 
    geom_edge_link(aes(color = bridge_id == "pgh-47")) +
    scale_edge_color_manual(values = c("TRUE" = "red", "FALSE" = "red"), na.value = "gray", guide = FALSE) +
    theme_graph() +
    coord_map()
}

# Rewiring ----

unique_bridges <- function(graph) {
  graph %>% 
    activate(edges) %>% 
    as_tibble() %>% 
    pull(bridge_id) %>% 
    unique() %>% 
    na.omit()
}

# Which Bridge relations _need_ rewiring?
# Find this by getting the subgraphs for each bridge relation and checking for connectedness
needs_rewire <- function(x, graph) {
  message("Testing ", x, "...", appendLF = FALSE)
  res <- graph %>% 
    activate(edges) %>% 
    # Keep only edges for this specific bridge
    filter(bridge_id == x) %>% 
    activate(nodes) %>% 
    # Remove the remaining unconnected nodes
    filter(centrality_degree(mode = "all") > 0) %>% 
    # IF the graph is not connected, return TRUE - we DO need to rewuire
    with_graph(!graph_is_connected())
  message(res)
  res
}

# Replace each bridge edgeset in the graph with a single synthetic edge with new
# endpoints
rewire_bridges <- function(osm_graph) {
  
}

# For a given bridge ID, create two new endpoints, connect the original 2+
# terminal points to these new endpoints, and remove the original edges/nodes
rewire_bridge <- function(osm_graph, bridge_id) {
  
}
