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
pgh_pathway_plan_generic <- drake_plan(pgh_pathway = target(
  greedy_search(starting_point = sp__, graph = pgh_tidy_graph, quiet = TRUE),
  trigger = trigger(command = FALSE, depend = FALSE, file = FALSE)))

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

# Greedy Search ----

# This strategy doesn't rely on artificially rewiring bridges. Instead, it uses
# a greedy search along the original graph, starting at a randomly-chosen
# bridge, crossing it, then taking the shortest path to the next closest bridge.
# Crossing that 2nd bridge, it then searches for the 3rd, disregarding the nodes
# already visited. If all points are Inf distances (i.e. unreachable) the
# function stops and returns the path as edge indices.

# starting_point = 577

library(dequer)

greedy_search <- function(starting_point, graph, quiet = !interactive()) {
  search_set <- get_interface_points(graph)
  stopifnot(starting_point %in% search_set)
  
  # Create queue to hold edgelist, since we don't know how long it will expand to
  qe <- queue()
  qv <- queue()
  
  # Start by crossing a bridge
  pathfinding_results <- locate_next_path(
    graph = graph, 
    starting_point = starting_point, 
    search_set = search_set, 
    qe = qe, qv = qv, 
    is_bridge_crossing = TRUE, 
    quiet = quiet)
  
  vpath = as.list(qv)
  epath = as.list(qe)
  path_distance = total_distance(graph, epath)
  
  results <- lst(
    epath,
    vpath,
    path_distance,
    pathfinding_results
  )
  
  # rev() the queues holding the edge and node lists so that R can efficiently garbage-collect them
  rev(qe)
  rev(qv)
  remove(qe)
  remove(qv)
  
  return(results)
}

locate_next_path <- function(graph, starting_point, search_set, qe, qv, is_bridge_crossing, quiet) {
  
  if (is_bridge_crossing) {
    bridge_id <- vertex_attr(graph, "associated_bridge", starting_point)
    candidate_points <- setdiff(get_bridge_points(graph, bridge_id), starting_point)
  } else {
    bridge_id <- NULL
    candidate_points <- setdiff(search_set, starting_point)
  }
  
  # If the candidate point lengths are 0, this means the point may likely be
  # tangent to another bridge and so it's not inherited both associated bridge
  # IDs. In this case, allow pathfinding to seek out the next closest bridge.
  if (length(candidate_points) == 0) {
    is_bridge_crossing <- FALSE
    bridge_id <- NULL
    candidate_points <- setdiff(search_set, starting_point)
  }
  
  # Report on status
  if (!quiet) message(step_status_message(starting_point, search_set, is_bridge_crossing, bridge_id))
  
  # Calculate possible distances
  candidate_distances <- distances(graph, v = starting_point, to = candidate_points,
                                   weights = edge_attr(graph, "distance"))
  
  # If no path can be found, then return out
  if (all(is.infinite(candidate_distances)))
    return(list(
      is_bridge_crossing = is_bridge_crossing,
      break_reason = "No paths found to point",
      point = starting_point, 
      candidates = candidate_points, 
      search_set = search_set,
      distances = candidate_distances))
  
  if (is_bridge_crossing) {
    target_point <- candidate_points[which.max(candidate_distances)]
  } else {
    target_point <- candidate_points[which.min(candidate_distances)]
  }
  
  suppressWarnings({
    possible_paths <- shortest_paths(graph, from = starting_point, to = target_point, 
                                     weights = edge_attr(graph, "distance"), output = "both")
  })
  
  epath <- as.integer(possible_paths$epath[[1]])
  vpath <- as.integer(possible_paths$vpath[[1]])
  
  pushback(qe, epath)
  pushback(qv, vpath)
  
  if (is_bridge_crossing) {
    # Penalized crossed edges that are ALSO bridges with an extremely high weight so they are only
    # returned to as a last resort
    penalized_bridges <- intersect(epath, which(edge_attr(graph, "is_bridge")))
    edge_attr(graph, "distance", index = penalized_bridges) <- edge_attr(graph, "distance", index = penalized_bridges) + 200000
    
    # Remove all nodes on the crossed bridge from the remaining search set
    search_set <- remove_bridge_from_set(graph, search_set, bridge_id)
  }
  
  # Pass two items to the next search step:
  # 1) the final node of the vpath - this becomes the starting point for the next step
  # 2) the pruned search set that removes all the nodes from the bridge just considered
  new_starting_point <- last(vpath)
  
  # If all bridges have been reached, return out empty set
  if (length(search_set) <= 0) {
    message("We made it!")
    return(list(
      is_bridge_crossing = is_bridge_crossing,
      break_reason = "All paths done",
      point = starting_point, 
      candidates = candidate_points, 
      search_set = search_set,
      distances = candidate_distances))
  }
  
  ignore( # To keep drake from reading this recursion as a cyclic dependency, we need to explicity ignore() this function call
    locate_next_path(graph = graph, 
                     starting_point = new_starting_point, 
                     search_set = search_set,
                     qe = qe, qv = qv, 
                     is_bridge_crossing = !is_bridge_crossing,
                     quiet = quiet)
  )
}

total_distance <- function(graph, epath) {
  sum(edge_attr(graph, "distance", index = which(edge_attr(graph, ".id") %in% epath)))
}

step_status_message <- function(starting_point, search_set, is_bridge_crossing, bridge_id = NULL) {
  starting <- str_glue("Starting from {starting_point}.")
  points_left <- str_glue("{length(search_set)} candidate points left.")
  
  if (is_bridge_crossing) {
    str_glue("{starting} Crossing bridge {bridge_id}. {points_left}")
  } else {
    str_glue("{starting} Looking for roads. {points_left}")
  }
}

enquque <- function(x, v) {
  walk(v, function(i) pushback(x, data = i))
}

get_bridge_points <- function(graph, bid) {
  vertex_points <- graph %>% 
    subgraph.edges(eids = which(edge_attr(graph, "bridge_id") == bid), delete.vertices = TRUE) %>% 
    induced_subgraph(which(vertex_attr(., "is_interface"))) %>% 
    vertex_attr("name")
  
  which(vertex_attr(graph, "name") %in% vertex_points)
}

get_interface_points <- function(graph) {
  which(V(graph)$is_interface)
}

remove_bridge_from_set <- function(graph, search_set, bridge_id) {
  bridge_points <- get_bridge_points(graph, bridge_id)
  setdiff(search_set, bridge_points)
}

assess_pathway <- function(l) {
  map_df(l, function(x) {
    data_frame(point = x$pathfinding_results$point, 
               distance = x$path_distance, 
               missing_points = length(x$pathfinding_results$search_set))
  }, .id = "target") %>% 
    arrange(missing_points, distance) %>% 
    mutate(performance_rank = row_number())
}
