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
  pgh_boundary_layer = read_sf(file_in("osmar/input_data/Pittsburgh_River_Boundary")),
  in_bound_points = nodes_within_boundaries(pgh_nodes_sf, pgh_boundary_layer),

  # Full Graph
  pgh_graph = as_igraph(pgh_raw),
  pgh_tidy_graph_unmarked = enrich_osmar_graph(raw_osmar = pgh_raw, 
                                               graph_osmar = pgh_graph, 
                                               in_pgh_nodes = in_bound_points, 
                                               excluded_highways = c("pedestrian", "footway", "cycleway", "steps", "track", 
                                                                     "elevator", "bus_stop", "construction", "no", "escape", 
                                                                     "proposed", "raceway", "services", "path"), 
                                               allowed_bridge_attributes =  c("motorway", "primary", "secondary", "tertiary", "trunk")),
  pgh_tidy_graph = mark_interface_nodes(pgh_tidy_graph_unmarked),
  pgh_interface_points = get_interface_points(pgh_tidy_graph),
  pgh_starting_points = get_starting_points(pgh_tidy_graph, pgh_interface_points),
  pgh_nodes = write_csv(as_tibble(pgh_tidy_graph, "nodes"), path = file_out("osmar/output_data/pgh_nodes.csv"), na = ""),
  pgh_edges = write_csv(as_tibble(pgh_tidy_graph, "edges"), path = file_out("osmar/output_data/pgh_edges.csv"), na = "")
)

# Locate pathways ----

# In order to determine which starting points need to be used, we need to build
# all pgh_plan targets UP TO THIS POINT in order to derive the correct
# pgh_starting_points values. From these values, we'll generate a new set of
# targets within pgh_expanded_pathways.

# make(pgh_plan)
# 
# pgh_pathway_plan_generic <- drake_plan(pgh_pathway = target(
#   greedy_search(starting_point = sp__, graph = pgh_tidy_graph, quiet = TRUE),
#   trigger = trigger(command = FALSE, depend = FALSE, file = FALSE)))
# 
# pgh_expanded_pathways <- evaluate_plan(pgh_pathway_plan_generic, rules = list(sp__ = readd("pgh_starting_points")))
# 
# # After building each pathway, run the assessment functions...
# assessment_plan_generic <- drake_plan(path_performance = assess_path(p = p__, graph = pgh_tidy_graph))
# assessment_plan <- evaluate_plan(assessment_plan_generic, rules = list(p__ = pgh_expanded_pathways$target))
# # ... and bind them into a dataframe
# pgh_performances <- gather_plan(assessment_plan, target = "pgh_performances", gather = "rbind")
# 
# pgh_plan <- bind_plans(
#   pgh_plan,
#   pgh_expanded_pathways,
#   assessment_plan,
#   pgh_performances
# )



# At this point, it is necessary to run the pathways to evaluate which one we want to use for visualization purposes.

# Visualize Pathways ----

plot_plan <- drake_plan(
  pgh_pathway_2480 = greedy_search(starting_point = 2480, graph = pgh_tidy_graph),
  
  # For visualization purposes only keep the graph within city limits + any
  # additional edges traversed by the pathway
  filtered_graph = filter_graph_to_pathway(graph = pgh_tidy_graph, pathway = pgh_pathway_2480),
  pathway_sf = graph_as_sf(filtered_graph),

  # Produce different collections of simple features to be rendered on maps or
  # as shapefiles
  pathway_layer = produce_pathway_sf(graph = pgh_tidy_graph, pathway = pgh_pathway_2480, linefun = produce_step_linestring),
  bridges_layer = filter(pathway_sf, edge_category == "crossed bridge"),
  crossed_roads_layer = filter(pathway_sf, edge_category == "crossed road"),
  uncrossed_roads_layer = filter(pathway_sf, edge_category == "uncrossed road"),

  # Generate a variety fo static PDF maps
  bridge_map = bridges_only_plot(pathway_sf, file_out("osmar/output_data/bridges_map.pdf")),
  pathway_map = pathway_plot(pathway_sf, file_out("osmar/output_data/pathway_map.pdf")),

  # Generate a standalone leaflet map
  leaflet_map = mapview(x = pathway_layer,
    zcol = "times_bridge_crossed_so_far"
    # color = list("#FD9E3F", "#7F5CFF", NA_character_)
    ),
  output_leaflet = mapshot(leaflet_map, url = file_out(fs::path(getwd(), "osmar/output_data/pgh_leaflet.html")))
)

# Produce several shapefiles
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
