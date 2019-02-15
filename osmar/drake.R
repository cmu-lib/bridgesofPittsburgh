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
library(konigsbergr)
library(pathfinder)
library(jsonlite)

# Load all functions
dir_walk("osmar/R", source)

# Build graph ----

pgh_plan <- drake_plan(
  download_osm = get_osm_bbox(-80.1257, -79.7978, 40.3405, 40.5407),
  pgh_nodes_sf = osm_nodes_to_sf(download_osm),
  # Shapefile for PGH boundaries
  pgh_boundary_layer = read_sf(file_in("osmar/input_data/Pittsburgh_River_Boundary")),
  pgh_block_data_layer = read_sf(file_in("osmar/input_data/Neighborhoods_")),
  in_bound_points = nodes_within_boundaries(pgh_nodes_sf, pgh_boundary_layer),
  node_communities = st_join(pgh_nodes_sf, pgh_block_data_layer),
  pgh_unlimited_graph = konigsberg_graph(src = download_osm, path_filter = automobile_highways, bridge_filter = main_bridges),
  pgh_filtered_graph = filter_bridges_to_nodes(pgh_unlimited_graph, node_ids = in_bound_points),
  pgh_tidy_graph = pgh_filtered_graph %>% 
    activate(nodes) %>% 
    left_join(select(st_set_geometry(node_communities, NULL), id, neighborhood = hood) %>% distinct(id, .keep_all = TRUE), by = "id"),
  pgh_edge_bundles = collect_edge_bundles(pgh_tidy_graph),
  pgh_nodes = write_csv(
    as_tibble(pgh_tidy_graph, "nodes") %>% 
      mutate(node_id = row_number()) %>% 
      select(id = node_id, osm_node_id = id, lat, lon, osm_label = label, neighborhood),
    path = file_out("osmar/output_data/pgh_nodes.csv"), na = ""),
  pgh_edges = write_csv(
    as_tibble(pgh_tidy_graph, "edges") %>% 
      mutate(edge_id = row_number()) %>% 
      select(from, to, id = edge_id, 
             osm_way_id = id, 
             osm_bridge_relation_id = bridge_relation, 
             osm_bridge = bridge, 
             osm_highway = highway, 
             osm_label = label, 
             bridge_id, distance, 
             within_boundaries), 
    path = file_out("osmar/output_data/pgh_edges.csv"), na = ""),
  pgh_bundles = write_lines(toJSON(pgh_edge_bundles, pretty = TRUE), path = "osmar/output_data/pgh_bundles.json"),

  test_run = greedy_search(pgh_tidy_graph, edge_bundles = pgh_edge_bundles, 
                           distances = E(pgh_tidy_graph)$distance, starting_point = 1),
  path_result = write_lines(toJSON(test_run$epath, pretty = TRUE), path = file_out("osmar/output_data/paths/edge_steps.json")),
  path_summary = write_csv(glance(test_run), na = "", path = file_out("osmar/output_data/paths/path_summary.csv")),
  path_details = write_csv(augment(test_run), na = "", path = file_out("osmar/output_data/paths/path_details.csv")),
  
  # For visualization purposes only keep the graph within city limits + any
  # additional edges traversed by the pathway
  pathway_sf = produce_pathway_sf(graph = pgh_tidy_graph, pathway = test_run),

  # Generate a standalone leaflet map
  leaflet_map = mapview(x = pathway_sf,
                        zcol = "total_times_bridge_crossed",
    color = c("#2B83BA", "#ABDDA4", "#FDAE61"),
    lwd = 4
    ),
  output_leaflet = mapshot(leaflet_map, url = file_out(fs::path(getwd(), "osmar/output_data/pgh_leaflet.html")))
)
