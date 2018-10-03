pkgconfig::set_config("drake::strings_in_dots" = "literals")
library(drake)
library(osmar)
library(tidyverse)
library(tidygraph)
library(ggraph)

edge_tags <- c("bridge", "bridge_name", "bridge:name", "wikipedia")

pgh_plan <- drake_plan(
  pgh_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/pgh_osm.xml"))),
  pgh_graph = as_igraph(pgh_raw),
  tiny_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/tiny.xml"))),
  tiny_graph = as_igraph(tiny_raw),
  tiny_bridges = find_bridge_waysets(tiny_raw),
  tiny_tidy_graph = enrich_osmar_graph(tiny_raw, tiny_graph, tiny_bridges),
  tiny_layout = lat_lon_layout(tiny_tidy_graph)
)

osm_node_attributes <- function(src) {
  base_attrs <- src$nodes$attrs %>% 
    select(id, lat, lon)
  
  tags <- src$nodes$tags %>% 
    mutate_at(vars(v), as.character) %>% 
    spread(k, v)
  
  left_join(base_attrs, tags, by = "id") %>% 
    mutate_at(vars(id), as.character)
}

osm_edge_attributes <- function(src) {
  way_tags <- src$ways$tags %>% 
    filter(k %in% bridge_keys) %>% 
    mutate_at(vars(v), as.character) %>%
    spread(k, v, drop = TRUE)
  
  way_tags %>% 
    mutate_at(vars(id), as.character)
}

enrich_osmar_graph <- function(raw_osmar, graph_osmar, bridges) {
  osmar_nodes <- osm_node_attributes(raw_osmar)
  osmar_edges <- osm_edge_attributes(raw_osmar)
  
  as_tbl_graph(graph_osmar) %>% 
    activate(nodes) %>% 
    left_join(osmar_nodes, by = c("name" = "id")) %>% 
    activate(edges) %>% 
    mutate_at(vars(name), as.character) %>% 
    left_join(osmar_edges, by = c("name" = "id")) %>% 
    left_join(bridges, by = c("name" = "way_id")) %>% 
    mutate(is_bridge = !is.na(bridge_id))
}

lat_lon_layout <- function(graph) {
  node_positions <- graph %>% activate(nodes) %>% as_tibble() %>% select(x = lon, y = lat)
  create_layout(graph, layout = "manual", node.positions = node_positions)
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

make(pgh_plan)
loadd(lazy = TRUE)

library(ggrepel)

ggraph(tiny_layout, layout = "manual") + 
  geom_edge_link(aes(color = bridge_id)) +
  #scale_edge_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  theme_graph() +
  coord_map(xlim = c(-80.0106, -79.9872), ylim = c(40.4429, 40.4551))

