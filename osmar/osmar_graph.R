library(osmar)
library(tidyverse)
library(tidygraph)
library(ggraph)

bridge_keys <- c("bridge", "bridge_name", "bridge:name", "bridge:structure", 
                 "bridge:support", "demolished:bridge", "demolished:bridge:material", 
                 "historic:bridge", "removed:bridge", "source:bridge", "source:removed:bridge")


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
  osmar_nodes <- osm_node_attributes(raw_osmar)
  osmar_edges <- osm_edge_attributes(raw_osmar)
  
  as_tbl_graph(graph_osmar) %>% 
    activate(edges) %>% 
    mutate_at(vars(name), as.character) %>% 
    left_join(osmar_edges, by = c("name" = "id")) %>% 
    activate(nodes) %>% 
    left_join(osmar_nodes, by = c("name" = "id"))
}
