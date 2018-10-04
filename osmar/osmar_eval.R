source("osmar/osmar_graph.R")
make(pgh_plan, verbose = 4)
loadd(lazy = TRUE)


tiny_layout %>% 
  ggraph(layout = "manual") + 
  geom_edge_link(aes(color = bridge_id == "pgh-47")) +
  scale_edge_color_manual(values = c("TRUE" = "red", "FALSE" = "red"), na.value = "gray", guide = FALSE) +
  theme_graph() +
  coord_map()

library(sigmajs)
sigmajs(height = 900, width = 1600) %>% 
  sg_from_igraph(tidy_pgh_graph, pgh_layout) %>% 
  sg_settings(drawNodes = FALSE)

is_internal_bridge_node <- function(neighborhood, graph, node, ...) {
  as_tibble(neighborhood, active = "edges") %>% 
    pull(bridge_id) %>% 
    na.omit() %>% 
    n_distinct() == 1
}

is_graph_bridge_adjacent <- function(neighborhood, graph, node, ...) {
  as_tibble(neighborhood, active = "edges") %>% 
    pull(is_bridge) %>% 
    any()
}

adjacent_bridges <- function(neighborhood, graph, node, ...) {
  res <- as_tibble(neighborhood, active = "edges") %>% 
    pull(bridge_id) %>% 
    unique() %>% 
    na.omit() %>% 
    paste(collapse = ";")
  
  if (nzchar(res))
    return(res)
  NA_character_
}

node_bridges <- tiny_tidy_graph %>% 
  activate(nodes) %>% 
  mutate(
    bridge_inherent = map_local_lgl(.f = is_internal_bridge_node),
    bridge_adjacent = map_local_lgl(.f = is_graph_bridge_adjacent),
    adjacent_bridges = map_local_chr(.f = adjacent_bridges)
  )

get_terminal_nodes <- function(x, graph) {
  
}

split_terminal_nodes <- function(x, graph) {
  
}
