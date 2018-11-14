source("osmar/osmar_graph.R")

make(pgh_plan, c("tiny_interface_points", "pgh_interface_points"))
loadd(list = c("tidy_tiny_graph", "tiny_interface_points", "pgh_tidy_graph", "pgh_interface_points"), verbose = 4)

tiny_pathway_plan_generic <- drake_plan(tiny_pathway = greedy_search(starting_point = sp__, graph = tidy_tiny_graph))
pgh_pathway_plan_generic <- drake_plan(pgh_pathway = greedy_search(starting_point = sp__, graph = pgh_tidy_graph))

tiny_expanded_pathways <- evaluate_plan(tiny_pathway_plan_generic, rules = list(sp__ = tiny_interface_points))
pgh_expanded_pathways <- evaluate_plan(pgh_pathway_plan_generic, rules = list(sp__ = pgh_interface_points))

assessment_plan <- drake_plan(
  tiny_results = map(set_names(tiny_expanded_pathways$target), readd, character_only = TRUE),
  tiny_performances = map_df(tiny_results, function(x) data_frame(point = x$pathfinding_results$point, distance = x$path_distance, missing_points = length(x$pathfinding_results$search_set)), .id = "target") %>% 
    arrange(missing_points, distance) %>% 
    mutate(performance_rank = row_number()),
  pgh_results = map(set_names(pgh_expanded_pathways$target), readd, character_only = TRUE),
  pgh_performances = map_df(pgh_results, function(x) data_frame(point = x$pathfinding_results$point, distance = x$path_distance, missing_points = length(x$pathfinding_results$search_set)), .id = "target") %>% 
    arrange(missing_points, distance) %>% 
    mutate(performance_rank = row_number())
)

all_pathways <- bind_plans(
  tiny_expanded_pathways,
  pgh_expanded_pathways,
  gather_plan(tiny_expanded_pathways, target = "tiny_pathways"),
  gather_plan(pgh_expanded_pathways, target = "pgh_pathways"),
  assessment_plan
)

make(all_pathways, "pgh_pathways", jobs = 12)
