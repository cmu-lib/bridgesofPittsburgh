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
      osm_url = str_glue("https://openstreetmap.org/way/{name}")
    ) %>%
    filter(within_boundaries | flagged_edge) %>%
    # Remove any orphaned nodes
    select_main_component() %>%
    activate(edges) %>%
    mutate_at(vars(flagged_edge), funs(as.factor(as.logical(.))))
}

# SF transforms ----

# Functions to take a graph and render it as sf object, including as linestrings and multilinestrings

produce_pathway_sf <- function(graph, pathway, linefun) {
  edges <- as_tibble(graph, "edges") %>%
    mutate(osm_url = str_glue("https://openstreetmap.org/way/{name}")) %>%
    arrange(.id)
  nodes <- as_tibble(graph, "nodes") %>% mutate(index = row_number())

  # Create one sf per step of the pathway, dividing into multiline or
  # linestring based on the function passed to linefun
  res <- imap(pathway$epath, linefun, edges = edges, nodes = nodes)
  do.call(rbind, args = res)
}

# Render each step as 1 multilinestring
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

# Render each step as a set of liestrings (one for each edge in the graph)
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

# Create an sf for the entire graph, including ucrossed roads
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
