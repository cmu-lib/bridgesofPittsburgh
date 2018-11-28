library(sf)
library(fs)
library(mapview)
source("osmar/pathfinding_build.R")

plot_plan <- drake_plan(
  filtered_graph = filter_graph_to_pathway(graph = pgh_tidy_graph, pathway = pgh_pathway_2469),
  pathway_sf = graph_as_sf(filtered_graph),
  modular_graph = add_modularity(filtered_graph, group_walktrap, steps = 10),
  
  # Layers
  pathway_layer = produce_pathway_sf(graph = pgh_tidy_graph, pathway = pgh_pathway_2469, linefun = produce_step_linestring),
  pathway_multiline_layer = produce_pathway_sf(graph = pgh_tidy_graph, pathway = pgh_pathway_2469, linefun = produce_step_multiline),
  bridges_layer = filter(pathway_sf, edge_category == "crossed bridge"),
  crossed_roads_layer = filter(pathway_sf, edge_category == "crossed road"),
  uncrossed_roads_layer = filter(pathway_sf, edge_category == "uncrossed road"),
  
  bridge_map = bridges_only_plot(pathway_sf, file_out("osmar/output_data/bridges_map.pdf")),
  pathway_map = pathway_plot(pathway_sf, file_out("osmar/output_data/pathway_map.pdf")),
  labeled_map = labeled_plot(pathway_sf, file_out("osmar/output_data/labeled_map.pdf")),
  leaflet_map = mapview_plot(list(
    "Bridges" = bridges_layer, 
    "Crossed roads" = crossed_roads_layer, 
    "Pittsburgh boudary" = pgh_boundary_layer)),
  output_leaflet = mapshot(leaflet_map, url = file_out(fs::path(getwd(), "osmar/output_data/pgh_leaflet.html")))
)

shp_plan <- drake_plan(
  pathway_shp = write_sf(pathway_layer, file_out("osmar/output_data/shapefiles/pathway_linestring"), driver = "ESRI Shapefile"),
  pathway_multiline_shp = write_sf(pathway_multiline_layer, file_out("osmar/output_data/shapefiles/pathway_multilinestring"), driver = "ESRI Shapefile"),
  bridge_shp = write_sf(bridges_layer, file_out("osmar/output_data/shapefiles/bridges_shp"), driver = "ESRI Shapefile"),
  crossed_roads_shp = write_sf(crossed_roads_layer, file_out("osmar/output_data/shapefiles/pathway_shp"), driver = "ESRI Shapefile")
)

all_plots <- bind_plans(
  all_pathways,
  shp_plan,
  plot_plan
)

# Graph transforms ----

filter_graph_to_pathway <- function(graph, pathway) {
  path_ids <- unique(flatten_int(pathway$epath))
  
  graph %>% 
    activate(edges) %>% 
    mutate(
      edge_order = match(row_number(), path_ids),
      flagged_edge = !is.na(edge_order),
      edge_label = if_else(is.na(edge_order), "", as.character(edge_order)),
      edge_category = factor(case_when(
        is_bridge & flagged_edge ~ "crossed bridge",
        !is_bridge & flagged_edge ~ "crossed road",
        is_bridge & !flagged_edge ~ "uncrossed bridge",
        TRUE ~ "uncrossed road"
      ), levels = c("crossed bridge", "crossed road", "uncrossed bridge", "uncrossed road"),
      ordered = TRUE)
    ) %>% 
    filter(within_boundaries | flagged_edge) %>% 
    select_main_component() %>% 
    activate(edges) %>% 
    mutate_at(vars(flagged_edge), funs(as.factor(as.logical(.))))
}

add_modularity <- function(graph, modfun, ...) {
  graph %>% 
    activate(nodes) %>% 
    mutate(modularity = modfun(weights = distance, ...))
}

# SF transforms ----

produce_pathway_sf <- function(graph, pathway, linefun) {
  edges <- as_tibble(graph, "edges") %>% arrange(.id)
  nodes <- as_tibble(graph, "nodes") %>% mutate(index = row_number())
  
  res <- imap(pathway$epath, linefun, edges = edges, nodes = nodes)
  do.call(rbind, args = res)
}

produce_step_multiline <- function(eids, i, edges, nodes) {
  select_edges <- edges[eids,]
  
  poly_attr <- data.frame(step = i)
  
  st_edges <- select_edges %>% 
    select(.id, from, to) %>% 
    gather(key = "dim", value = "index", from, to) %>%
    arrange(.id, dim) %>% 
    left_join(select(nodes, lat, lon, index), by = "index") %>% 
    split(as.factor(.$.id)) %>% 
    map(function(x) {
      cbind(x$lon, x$lat)
    }) %>% 
    st_multilinestring()
  
  sf_col <- st_sfc(st_edges, crs = 4326)
  st_geometry(poly_attr) <- sf_col
  poly_attr
}

produce_step_linestring <- function(eids, i, edges, nodes) {
  select_edges <- edges[eids,]
  
  st_edges <- select_edges %>% 
    select(.id, from, to) %>% 
    gather(key = "dim", value = "index", from, to) %>%
    arrange(.id, dim) %>% 
    left_join(select(nodes, lat, lon, index), by = "index") %>% 
    split(as.factor(.$.id)) %>% 
    map(function(x) {
      cbind(x$lon, x$lat)
    }) %>% 
    map(st_linestring)
  
  sf_col <- st_sfc(st_edges, crs = 4326)
  st_geometry(select_edges) <- sf_col
  select_edges$step <- i
  select_edges
}

graph_as_sf <- function(marked_graph) {
  edges <- as_tibble(marked_graph, "edges") %>% arrange(.id)
  nodes <- as_tibble(marked_graph, "nodes") %>% mutate(index = row_number())
  
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
  
  sf_col <- st_sfc(st_edges, crs = 4326)
  st_geometry(edges) <- sf_col
  edges
}

# Plotting ----

bridges_only_plot <- function(edges, filepath) {
  pdf(filepath, width = 20, height = 20)
  plot(edges["is_bridge"], pal = c(
    "TRUE" = "gray",
    "FALSE" = "red"
  ), lwd = if_else(edges$is_bridge == "TRUE", 2, 0.5))
  dev.off()
}

pathway_plot <- function(edges, filepath) {
  pdf(filepath, width = 20, height = 20)
  plot(edges["edge_category"], pal = c(
    "crossed bridge" = "#d95f02",
    "crossed road" = "#1b9e77",
    "uncrossed bridge" = "#e6ab02",
    "uncrossed road" = "gray"
  ), lwd = if_else(edges$flagged_edge == "TRUE", 2, 0.5))
  dev.off()
}

labeled_plot <- function(edges, filepath) {
  p <- ggplot(edges) +
    geom_sf(aes(fill = edge_category)) +
    geom_sf_text(aes(label = edge_label), check_overlap = TRUE)
  
  ggsave(filename = filepath, plot = p, width = 20, height = 20)
}

mapview_plot <- function(l) {
  mapview(l)
}

# Animation ----

animation_frame <- function(e, i, edges, dirpath) {
  if (length(e) == 0) {
    edges$highlight <- FALSE
  } else {
    edges$highlight <- edges$.id %in% e
  }
  png(fs::path(dirpath, paste0(str_pad(i, width = 3, pad = "0", side = "left"), "map"), ext = "png"), height = 768, width = 1024)
  plot(edges["highlight"], pal = c("gray", "red"), lwd = if_else(edges$highlight, 3, 1), key.pos = NULL)
  dev.off()
}

animation_plot <- function(pathway, edges, dirpath) {
  fs::dir_create(dirpath)
  iwalk(pathway$epath, animation_frame, edges = edges, dirpath = dirpath)
}
