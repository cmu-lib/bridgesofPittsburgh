# Graph transforms ----

# We run the pathway traversal on a graph much larger than Pittsburgh, so that
# the path can traverse some non-Pittsburgh roads in order to move about
# different parts of the city. However we only want to visualize Pittsburgh. This
# filters down the graph to just the city + any additional paths crossed, and
# adds edge attributes marking which were passed by a pathway.

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
      ordered = TRUE),
      osm_url = str_glue("https://openstreetmap.org/way/{id}")
    ) %>%
    filter(within_boundaries | flagged_edge) %>%
    # Remove any orphaned nodes
    select_main_component() %>%
    activate(edges) %>%
    mutate_at(vars(flagged_edge), funs(as.factor(as.logical(.))))
}

# SF transforms ----

# Functions to take a graph and render it as sf object, including as linestrings and multilinestrings

produce_pathway_sf <- function(graph, pathway) {
  edges_sf <- edges_to_sf(graph, V(graph)$lat, V(graph)$lon) %>% 
    mutate(osm_url = str_glue("https://openstreetmap.org/way/{id}"))
  
  augmented_pathway <- augment(pathway) %>% 
    group_by(bundle_id) %>% 
    mutate(total_times_bridge_crossed = max(times_bundle_crossed)) %>% 
    ungroup()
  
  bind_cols(edges_sf[augmented_pathway$edge_id,], augmented_pathway)
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

mapview_plot <- function(l, ...) {
  mapview(l, ...)
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
