# Drake ----

pkgconfig::set_config(
  "drake::strings_in_dots" = "literals",
  "drake::verbose" = 4)

library(drake)
library(osmar)
library(tidyverse)
library(tidygraph)
library(igraph)
library(rgdal)
library(assertr)
library(httr)
library(furrr)
library(geosphere)
library(sf)
library(fs)
library(mapview)

# Load all functions
dir_walk("osmar/R", source)

# Build graph ----

tiny_limits <- list(xlim = c(-80.0054, -79.9817), ylim = c(40.4424, 40.4602))
big_limits <- list(xlim = c(-80.1257, -79.7978), ylim = c(40.3405, 40.5407))

plan(multiprocess)

# Sample Graph
tiny_plan <- drake_plan(
  download_tiny = target(
    command = get_osm_bbox(str_glue("{tiny_limits$xlim[1]},{tiny_limits$ylim[1]},{tiny_limits$xlim[2]},{tiny_limits$ylim[2]}")),
    # Must manually trigger a new data download
    trigger = trigger(command = FALSE, depend = FALSE, file = FALSE)),
  tiny_raw = read_osm_response(download_tiny),
  tiny_graph = as_igraph(tiny_raw),
  tiny_tidy_graph_unmarked = enrich_osmar_graph(tiny_raw, tiny_graph, limits = tiny_limits),
  tidy_tiny_graph = mark_interface_nodes(tiny_tidy_graph_unmarked),
  tiny_interface_points = get_interface_points(tidy_tiny_graph)
)

large_plan <- drake_plan(
  download_osm = target(
    command = get_osm_bbox("{big_limits$xlim[1]},{big_limits$ylim[1]},{big_limits$xlim[2]},{big_limits$ylim[2]}"),
    # Must manually trigger a new data download
    trigger = trigger(command = FALSE, depend = FALSE, file = FALSE)),
  pgh_raw = read_osm_response(download_osm),
  # Shapefile for PGH boundaries
  pgh_boundary_shp = as(readOGR(file_in("osmar/input_data/Pittsburgh_City_Boundary")), "SpatialPolygons"),
  pgh_boundary_layer = read_sf(file_in("osmar/input_data/Pittsburgh_City_Boundary")),
  pgh_points_sp = as_sp(pgh_raw, "points"),
  point_overlap = over(pgh_points_sp, pgh_boundary_shp),
  in_bound_points = names(na.omit(point_overlap)),
  
  # Full Graph
  pgh_graph = as_igraph(pgh_raw),
  pgh_tidy_graph_unmarked = enrich_osmar_graph(pgh_raw, pgh_graph, in_pgh_nodes = in_bound_points),
  pgh_tidy_graph = mark_interface_nodes(pgh_tidy_graph_unmarked),
  pgh_interface_points = get_interface_points(pgh_tidy_graph),
  pgh_starting_points = get_starting_points(pgh_tidy_graph, pgh_interface_points),
  pgh_nodes = write_csv(as_tibble(pgh_tidy_graph, "nodes"), path = file_out("osmar/output_data/pgh_nodes.csv"), na = ""),
  pgh_edges = write_csv(as_tibble(pgh_tidy_graph, "edges"), path = file_out("osmar/output_data/pgh_edges.csv"), na = "")
)

pgh_plan <- bind_plans(
  tiny_plan,
  large_plan
)

# Locate pathways ----

make(pgh_plan)

pgh_pathway_plan_generic <- drake_plan(pgh_pathway = target(
  greedy_search(starting_point = sp__, graph = pgh_tidy_graph, quiet = TRUE),
  trigger = trigger(command = FALSE, depend = FALSE, file = FALSE)))

pgh_expanded_pathways <- evaluate_plan(pgh_pathway_plan_generic, rules = list(sp__ = readd("pgh_starting_points")))

assessment_plan_generic <- drake_plan(path_performance = assess_path(p = p__, graph = pgh_tidy_graph))
assessment_plan <- evaluate_plan(assessment_plan_generic, rules = list(p__ = pgh_expanded_pathways$target))
pgh_performances <- gather_plan(assessment_plan, target = "pgh_performances", gather = "rbind")

pgh_plan <- bind_plans(
  pgh_plan,
  pgh_expanded_pathways,
  assessment_plan,
  pgh_performances
)

# Visualize Pathways ----

plot_plan <- drake_plan(
  filtered_graph = filter_graph_to_pathway(graph = pgh_tidy_graph, pathway = pgh_pathway_2469),
  pathway_sf = graph_as_sf(filtered_graph),
  modular_graph = add_modularity(filtered_graph, group_walktrap, steps = 10000),
  
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

pgh_plan <- bind_plans(
  pgh_plan,
  shp_plan,
  plot_plan
)
