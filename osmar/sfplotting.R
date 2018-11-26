library(sf)
library(fs)
source("osmar/pathfinding_build.R")
loadd(c("tidy_tiny_graph", "pgh_tidy_graph"), cache = path_cache)
loadd("pgh_pathway_2489", cache = path_cache)

mark_graph <- function(graph, pathway) {
  path_ids <- unique(flatten_int(pathway$epath))
  
  marked_graph <- graph %>% 
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
    # Simplify multiple edges on all two-way roads so that we only plot one line
    # per road. The edge category is a ranked factor, so it will plot as a
    # crossed bridge before being plotted as anything else, etc.
    simplify(remove.multiple = TRUE, edge.attr.comb = list(edge_cateogry = "max", flagged_edge = "max", "first")) %>% 
    as_tbl_graph() %>% 
    activate(edges) %>% 
    mutate(flagged_edge = as.factor(as.logical(flagged_edge)))
  
  marked_graph
}

flagged_graph_as_sf <- function(marked_graph) {
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

graph_to_sf <- function(graph, pathway) {
  flagged_graph <- mark_graph(graph, pathway)
  flagged_graph_as_sf(flagged_graph)
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

animation_frame <- function(e, i, edges, dirpath) {
  if (length(e) == 0) {
    edges$highlight <- FALSE
  } else {
    edges$highlight <- edges$.id %in% e
  }
  png(fs::path(dirpath, paste0(str_pad(i, width = 3, pad = "0", side = "left"), "map"), ext = "png"), height = 768, width = 1024)
  plot(edges["highlight"], pal = c("gray", "red"), lwd = if_else(edges$highlight, 3, 1))
  dev.off()
}

animation_plot <- function(pathway, edges, dirpath) {
  fs::dir_create(dirpath)
  iwalk(pathway$epath, animation_frame, edges = edges, dirpath = dirpath)
}
