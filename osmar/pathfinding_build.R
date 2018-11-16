source("osmar/osmar_graph.R")
graph_cache <- new_cache(".graph")
make(pgh_plan, c("tiny_interface_points", "pgh_interface_points"), cache = graph_cache)
orig_tiny_interface_points = readd("tiny_interface_points", cache = graph_cache)
orig_pgh_interface_points = readd("pgh_interface_points", cache = graph_cache)

path_cache <- new_cache(".path")
original_targets <- drake_plan(
  tidy_tiny_graph = readd("tidy_tiny_graph", cache = graph_cache),
  pgh_tidy_graph = readd("pgh_tidy_graph", cache = graph_cache)
)

tiny_pathway_plan_generic <- drake_plan(tiny_pathway = greedy_search(starting_point = sp__, graph = tidy_tiny_graph, quiet = TRUE))
pgh_pathway_plan_generic <- drake_plan(pgh_pathway = greedy_search(starting_point = sp__, graph = pgh_tidy_graph, quiet = TRUE))

tiny_expanded_pathways <- evaluate_plan(tiny_pathway_plan_generic, rules = list(sp__ = orig_tiny_interface_points))
pgh_expanded_pathways <- evaluate_plan(pgh_pathway_plan_generic, rules = list(sp__ = orig_pgh_interface_points))

tiny_gathered <- gather_plan(tiny_expanded_pathways, target = "tiny_results", gather = "list")
pgh_gathered <- gather_plan(pgh_expanded_pathways, target = "pgh_results", gather = "list")

assessment_plan <- drake_plan(
  tiny_performances = assess_pathway(tiny_results),
  pgh_performances = assess_pathway(pgh_results)
)

all_pathways <- bind_plans(
  original_targets,
  tiny_expanded_pathways,
  pgh_expanded_pathways,
  tiny_gathered,
  pgh_gathered,
  assessment_plan
)

assess_pathway <- function(l) {
  map_df(l, function(x) {
    data_frame(point = x$pathfinding_results$point, 
               distance = x$path_distance, 
               missing_points = length(x$pathfinding_results$search_set))
  }, .id = "target") %>% 
    arrange(missing_points, distance) %>% 
    mutate(performance_rank = row_number())
}
