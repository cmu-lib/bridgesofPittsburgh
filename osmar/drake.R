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
library(dequer)

# Load all functions
dir_walk("osmar/R", source)

# Build graph ----

pgh_plan <- drake_plan(
  big_limits = list(xlim = c(-80.1257, -79.7978), ylim = c(40.3405, 40.5407)),
  download_osm = get_osm_bbox(big_limits$xlim, big_limits$ylim),
  pgh_raw = read_osm_response(download_osm),
  pgh_nodes_sf = osm_nodes_to_sf(pgh_raw),
  # Shapefile for PGH boundaries
  pgh_boundary_layer = read_sf(file_in("osmar/input_data/Pittsburgh_City_Boundary")),
  in_bound_points = nodes_within_boundaries(pgh_nodes_sf, pgh_boundary_layer),
  
  # Full Graph
  pgh_graph = as_igraph(pgh_raw),
  pgh_tidy_graph_unmarked = enrich_osmar_graph(pgh_raw, pgh_graph, in_pgh_nodes = in_bound_points),
  pgh_tidy_graph = mark_interface_nodes(pgh_tidy_graph_unmarked),
  pgh_interface_points = get_interface_points(pgh_tidy_graph),
  pgh_starting_points = get_starting_points(pgh_tidy_graph, pgh_interface_points),
  pgh_nodes = write_csv(as_tibble(pgh_tidy_graph, "nodes"), path = file_out("osmar/output_data/pgh_nodes.csv"), na = ""),
  pgh_edges = write_csv(as_tibble(pgh_tidy_graph, "edges"), path = file_out("osmar/output_data/pgh_edges.csv"), na = "")
)

# Locate pathways ----

# In order to determine which 
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

# At this point, it is necessary to run the pathways to evaluate which one we want to use for visualization purposes.

# Visualize Pathways ----

plot_plan <- drake_plan(
  filtered_graph = filter_graph_to_pathway(graph = pgh_tidy_graph, pathway = pgh_pathway_100279),
  pathway_sf = graph_as_sf(filtered_graph),
  modular_graph = add_modularity(filtered_graph, group_walktrap, steps = 10000),
  
  # Layers
  pathway_layer = produce_pathway_sf(graph = pgh_tidy_graph, pathway = pgh_pathway_100279, linefun = produce_step_linestring),
  pathway_multiline_layer = produce_pathway_sf(graph = pgh_tidy_graph, pathway = pgh_pathway_100279, linefun = produce_step_multiline),
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
