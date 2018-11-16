library(sf)
source("osmar/osmar_graph.R")
source("osmar/pathfinding_build.R")
loadd(c("tidy_tiny_graph", "pgh_tidy_graph"), cache = graph_cache)

#res1027 <- greedy_search(1027, pgh_tidy_graph)
load("res1027.rda")

path_ids <- res1027$epath %>% 
  flatten_int()

graph <- pgh_tidy_graph %>% 
  activate(edges) %>% 
  mutate(
    edge_order = match(row_number(), path_ids),
    flagged_edge = !is.na(edge_order),
    edge_label = if_else(is.na(edge_order), "", as.character(edge_order)),
    edge_category = as.factor(case_when(
      is_bridge & flagged_edge ~ "crossed bridge",
      !is_bridge & flagged_edge ~ "crossed road",
      is_bridge & !flagged_edge ~ "uncrossed bridge",
      TRUE ~ "uncrossed road"
    ))
  ) %>% 
  filter(within_boundaries | flagged_edge) %>% 
  select_main_component()

nodes <- as_tibble(graph, "nodes") %>% 
  mutate(index = row_number())
edges <- as_tibble(graph, "edges")
st_nodes <- st_multipoint(cbind(nodes$lon, nodes$lat), dim = "XY")
st_edges <- edges %>% 
  select(.id, from, to) %>% 
  gather(key = "dim", value = "index", from, to) %>%
  arrange(.id, dim) %>% 
  left_join(select(nodes, lat, lon, index), by = "index") %>% 
  split(as.factor(.$.id)) %>% 
  map(function(x) {
    cbind(x$lon, x$lat)
  }) %>% 
  map(st_linestring)

pgh_sf <- st_sfc(st_edges, crs = 4326)

st_geometry(edges) <- pgh_sf


pdf("big.pdf", width = 36, height = 48)
plot(edges["edge_category"], pal = c(
  "crossed bridge" = "#d95f02",
  "crossed road" = "#1b9e77",
  "uncrossed bridge" = "#e6ab02",
  "uncrossed road" = "gray"
), lwd = if_else(edges$flagged_edge, 5, 1))
dev.off()

# 
# %>% 
#   lat_lon_layout() %>%
#   ggraph(layout = "manual") +
#   geom_edge_link(aes(color = edge_category, width = flagged_edge)) +
#   scale_edge_color_manual(values = c(
#     "crossed bridge" = "#d95f02",
#     "crossed road" = "#1b9e77",
#     "uncrossed bridge" = "#e6ab02",
#     "uncrossed road" = "gray"
#   )) +
#   scale_edge_width_manual(values = c("TRUE" = 1.2, "FALSE" = 0.5)) +
#   theme_graph() +
#   coord_map()
# }