pkgconfig::set_config("drake::strings_in_dots" = "literals")
library(drake)
library(osmar)
library(tidyverse)
library(tidygraph)
library(ggraph)

bridge_keys <- c("bridge", "bridge_name", "bridge:name", "bridge:structure", 
                 "bridge:support", "demolished:bridge", "demolished:bridge:material", 
                 "historic:bridge", "removed:bridge", "source:bridge", "source:removed:bridge")

pgh_plan <- drake_plan(
  pgh_raw = get_osm(complete_file(), source = osmsource_file(file_in("pgh_osm.xml"))),
  pgh_graph = as_igraph(pgh_raw),
  tidy_pgh_graph = enrich_osmar_graph(pgh_raw, pgh_graph),
  tiny_raw = get_osm(complete_file(), source = osmsource_file(file_in("tiny.xml"))),
  tiny_graph = as_igraph(tiny_raw),
  tiny_tidy_graph = enrich_osmar_graph(tiny_raw, tiny_graph),
  tiny_layout = lat_lon_layout(tiny_tidy_graph),
  bridge_layout = lat_lon_layout(bridges_only),
  bridges_only = tidy_pgh_graph %>% 
    activate(edges) %>% 
    filter(bridge) %>% 
    activate(nodes) %>% 
    mutate(degree = centrality_degree()) %>% 
    filter(degree >= 1)
)

osm_node_attributes <- function(src) {
  base_attrs <- src$nodes$attrs
  base_attrs$timestamp <- as.character(base_attrs$timestamp)
  base_attrs <- base_attrs %>% 
    mutate_at(vars(visible), as.logical) %>% 
    mutate_at(vars(uid, version), funs(as.integer(as.character(.))))
  
  tags <- src$nodes$tags %>% 
    mutate_at(vars(v), as.character) %>% 
    spread(k, v)
  
  left_join(base_attrs, tags, by = "id") %>% 
    mutate_at(vars(id), as.character)
}

osm_edge_attributes <- function(src) {
  way_attrs <- src$ways$attrs
  way_attrs$timestamp <- as.character(way_attrs$timestamp)
  way_attrs <- way_attrs %>% 
    mutate_at(vars(visible), as.logical) %>% 
    mutate_at(vars(uid, version), funs(as.integer(as.character(.))))
  
  way_tags <- src$ways$tags %>% 
    mutate_at(vars(v), as.character) %>%
    spread(k, v, drop = TRUE)
  
  left_join(way_attrs, way_tags, by = "id") %>% 
    mutate_at(vars(id), as.character)
}

enrich_osmar_graph <- function(raw_osmar, graph_osmar) {
  osmar_edges <- osm_edge_attributes(raw_osmar)
  
  as_tbl_graph(graph_osmar) %>% 
    activate(edges) %>% 
    mutate_at(vars(name), as.character) %>% 
    left_join(osmar_edges, by = c("name" = "id")) %>% 
    mutate_at(vars(bridge), funs(!is.na(.))) %>% 
    activate(nodes) %>% 
    left_join(osmar_nodes, by = c("name" = "id"))
}

lat_lon_layout <- function(graph) {
  node_positions <- graph %>% activate(nodes) %>% as_tibble() %>% select(x = lon, y = lat)
  create_layout(graph, layout = "manual", node.positions = node_positions)
}
