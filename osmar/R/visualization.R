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

bridge_text_table <- function(graph, pathway) {
  road_labels <- as_tibble(graph, "edges")[unlist(pathway$epath), ] %>% 
    mutate(
      label = coalesce(label, "<unnamed road>"),
      label_dupe = duplicated(label),
      label_number = cumsum(!label_dupe)) %>% 
    group_by(label_number, label) %>% 
    summarize(dist = sum(distance), 
              osm_url = first(str_glue("https://openstreetmap.org/way/{id}")), 
              is_bridge = any(!is.na(bridge_id)))
}

bridge_text_html <- function(text_table) {
  tags$ol(pmap(text_table, function(label, dist, osm_url, ...) tags$li(a(str_glue("{label} - {format(dist, digits = 0)} meters"), href = osm_url))))
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

# Creat POINT sf with centroids for all bridges and attributes for a leaflet map popup
bridge_centroids <- function(graph, pathway) {
  # Get point sf from graph
  graph_pt <- nodes_to_sf(pathway$graph_state)
  
  # Get the unique vertex indices for each bridge
  bridge_points <- map(pathway$edge_bundles, function(es) {
    point_indices <- unique(as.integer(ends(graph, es = es, names = FALSE)))
    st_union(graph_pt[point_indices,])
  })
  
  # Use sf to find the centroid for each node and then bind into one sf
  # collection (have to use do.call() because apparently c.sfc doesn't work in
  # bulk, as sf doesn't want to check they all have the same CRS...)
  bridge_centroids <- map(bridge_points, st_centroid) %>% 
    do.call(c, .)
  
  bridge_attributes <- pathway$graph_state %>% 
    as_tibble("edges") %>% 
    filter(!is.na(pathfinder.bundle_id)) %>% 
    group_by(pathfinder.bundle_id) %>% 
    summarize_all(funs(last)) %>% 
    mutate(
      bridge_label = case_when(
        # Yank the text "relation" from a few poorly-named bridge labels
        !is.na(relation_label) ~ str_replace(as.character(relation_label), " ?relation", ""),
        !is.na(label) ~ label,
        TRUE ~ "unnamed bridge"
      ),
      osm_url = case_when(
        is.na(bridge_relation) ~ str_glue("https://openstreetmap.org/way/{bridge_id}"),
        !is.na(bridge_relation) ~ str_glue("https://openstreetmap.org/relation/{bridge_id}")
      )
    ) %>% 
    select(
      bridge_label,
      road_type = highway,
      osm_url
    )
  
  st_geometry(bridge_attributes) <- bridge_centroids
  bridge_attributes
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
