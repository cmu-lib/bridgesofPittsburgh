source("osmar/osmar_graph.R")
make(pgh_plan, verbose = 4)
loadd(lazy = TRUE)


pgh_layout <- lat_lon_layout(final_pgh_graph) %>% 
  mutate(y = abs(y - 1))

colored_graph <- final_pgh_graph %>% 
  activate(edges) %>% 
  mutate(color = case_when(
    synthetic ~ "#FB0207",
    rewired ~ "#21FF06",
    is_bridge ~ "#0F80FF",
    TRUE ~ "#7F7F7F"
  ))

library(sigmajs)
sigmajs(height = 900, width = 1600) %>% 
  sg_from_igraph(colored_graph, pgh_layout) %>% 
  sg_settings(
    drawNodes = FALSE, 
    drawEdgeLabels = FALSE,
    drawNodeLabels = FALSE)

lois_internal_bridge_node <- function(neighborhood, graph, node, ...) {
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
