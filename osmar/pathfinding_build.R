source("osmar/osmar_graph.R")
make(pgh_plan, c("tiny_interface_points", "pgh_interface_points"))

tiny_pathway_plan_generic <- drake_plan(tiny_pathway = greedy_search(starting_point = sp__, graph = tidy_tiny_graph, quiet = TRUE))
pgh_pathway_plan_generic <- drake_plan(pgh_pathway = target(
  greedy_search(starting_point = sp__, graph = pgh_tidy_graph, quiet = TRUE)))

tiny_expanded_pathways <- evaluate_plan(tiny_pathway_plan_generic, rules = list(sp__ = readd("tiny_interface_points")))
pgh_expanded_pathways <- evaluate_plan(pgh_pathway_plan_generic, rules = list(sp__ = readd("pgh_interface_points")))

tiny_gathered <- gather_plan(tiny_expanded_pathways, target = "tiny_results", gather = "list")
pgh_gathered <- gather_plan(pgh_expanded_pathways, target = "pgh_results", gather = "list")

assessment_plan_generic <- drake_plan(path_performance = assess_path(p = p__, graph = pgh_tidy_graph))
assessment_plan <- evaluate_plan(assessment_plan_generic, rules = list(p__ = pgh_expanded_pathways$target))
pgh_performances <- gather_plan(assessment_plan, target = "pgh_performances", gather = "rbind")

all_pathways <- bind_plans(
  pgh_plan,
  tiny_expanded_pathways,
  pgh_expanded_pathways,
  tiny_gathered,
  pgh_gathered,
  assessment_plan,
  pgh_performances
)

# Greedy Search ----

# This strategy doesn't rely on artificially rewiring bridges. Instead, it uses
# a greedy search along the original graph, starting at a randomly-chosen
# bridge, crossing it, then taking the shortest path to the next closest bridge.
# Crossing that 2nd bridge, it then searches for the 3rd, disregarding the nodes
# already visited. If all points are Inf distances (i.e. unreachable) the
# function stops and returns the path as edge indices.

library(dequer)

greedy_search <- function(starting_point, graph, quiet = !interactive()) {
  search_set <- get_interface_points(graph)
  stopifnot(starting_point %in% search_set)
  
  # Create queue to hold edgelist, since we don't know how long it will expand to
  qe <- queue()
  qv <- queue()
  qb <- queue()
  
  # Start by crossing a bridge
  pathfinding_results <- locate_next_path(
    graph = graph, 
    starting_point = starting_point, 
    search_set = search_set, 
    qe = qe, qv = qv, qb = qb,
    is_bridge_crossing = TRUE, 
    quiet = quiet)
  
  vpath = as.list(qv)
  epath = as.list(qe)

  results <- lst(
    epath,
    vpath,
    pathfinding_results,
    starting_point
  )
  
  # rev() the queues holding the edge and node lists so that R can efficiently garbage-collect them
  rev(qe)
  rev(qv)
  rev(qb)
  remove(qe)
  remove(qv)
  remove(qb)
  
  return(results)
}

locate_next_path <- function(graph, starting_point, search_set, qe, qv, qb, is_bridge_crossing, quiet) {
  
  crossed_bridges <- flatten_chr(as.list(qb))
  search_set <- setdiff(search_set, starting_point)
  
  if (is_bridge_crossing) {
    # Get first uncrossed bridge that goes from this node
    bridge_id <- first(setdiff(na.omit(edge_attr(graph, "bridge_id", E(graph)[.from(starting_point)])), crossed_bridges))
    candidate_edges <- which(edge_attr(graph, "bridge_id") == bridge_id)
    # Collect the head/"to" nodes of all the bridge edges, since we will always
    # be starting at the tail/"from" node of a bridge edge
    candidate_points <- setdiff(unique(as.integer(head_of(graph, es = candidate_edges))), starting_point)
  } else {
    bridge_id <- NULL
    candidate_points <- search_set
  }
  
  # If the candidate point lengths are 0, this means the point may likely be
  # tangent to another bridge and so it's not inherited both associated bridge
  # IDs. In this case, allow pathfinding to seek out the next closest bridge.
  if (length(candidate_points) == 0) {
    message("no bridge candidates, looking for a new point")
    is_bridge_crossing <- FALSE
    bridge_id <- NULL
    candidate_points <- search_set
  }
  
  # Report on status
  if (!quiet) message(step_status_message(starting_point, search_set, is_bridge_crossing, bridge_id))
  
  # Calculate possible distances
  candidate_distances <- distances(graph, v = starting_point, to = candidate_points, mode = "out",
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
    # Find a path within the subgraph comprising only that bridge
    bridge_graph <- subgraph.edges(graph, eids = candidate_edges, delete.vertices = FALSE)
    candidate_distances <- distances(bridge_graph, v = starting_point, to = candidate_points, mode = "out", weights = edge_attr(bridge_graph, "distance"))
    ranking <- min_rank(candidate_distances)
    ranking[is.infinite(candidate_distances)] <- 0L
    target_point <- candidate_points[which.max(ranking)]
    suppressWarnings({
      possible_paths <- shortest_paths(bridge_graph, from = starting_point, to = target_point, mode = "out", output = "both", weights = edge_attr(bridge_graph, "distance"))
    })
  } else {
    target_point <- candidate_points[which.min(candidate_distances)]
    suppressWarnings({
      possible_paths <- shortest_paths(graph, from = starting_point, to = target_point, 
                                       weights = edge_attr(graph, "distance"), output = "both", mode = "out")
    })
  }
  
  epath <- possible_paths$epath[[1]]$.id
  vpath <- as.integer(possible_paths$vpath[[1]])
  
  pushback(qe, epath)
  pushback(qv, vpath)
  
  bridges_crossed <- na.omit(unique(edge_attr(graph, "bridge_id", index = epath)))
  if (length(bridges_crossed) > 0) {
    # Any bridges crossed get added to the queue
    pushback(qb, bridges_crossed)
    message("Bridges crossed: ", str_c(bridges_crossed, collapse = "; "))
    
    bridge_edges <- which(edge_attr(graph, "bridge_id") %in% bridges_crossed)
    bridge_nodes <- as.integer(head_of(graph, es = bridge_edges))
    
    message("increasing weights for ", str_c(bridge_edges, collapse = "; "))
    edge_attr(graph, "distance", index = bridge_edges) <- edge_attr(graph, "distance", index = bridge_edges) + 20000000
    
    # Remove all nodes on the crossed bridge from the remaining search set
    removed_nodes <- intersect(search_set, bridge_nodes)
    message("removing nodes ", str_c(removed_nodes, collapse = "; "))
    search_set <- setdiff(search_set, bridge_nodes)
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
  
  # Force garbage collection before moving on
  gc(full = TRUE)
  
  ignore( # To keep drake from reading this recursion as a cyclic dependency, we need to explicity ignore() this function call
    locate_next_path(graph = graph, 
                     starting_point = new_starting_point, 
                     search_set = search_set,
                     qe = qe, qv = qv, qb = qb,
                     is_bridge_crossing = !is_bridge_crossing,
                     quiet = quiet)
  )
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

remove_bridge_from_set <- function(graph, search_set, bridge_id) {
  bridge_points <- get_bridge_points(graph, bridge_id)
  setdiff(search_set, bridge_points)
}

assess_all_paths <- function(l, graph) {
  map_df(l, assess_path, graph = graph, .id = "target")
}

path_step_distance <- function(eids, graph) {
  sum(edge_attr(graph, "distance", index = eids))
}

path_bridge_crossed <- function(eids, graph) {
  unique(edge_attr(graph, "bridge_id", index = eids))
}

assess_path <- function(p, graph) {
  starting_point <- p$starting_point
  ending_point <- p$pathfinding_results$point
  n_steps <- length(p$epath)
  
  total_distance <- sum(map_dbl(p$epath, path_step_distance, graph = graph))
  times_bridges_crossed <- p$epath %>% 
    map(path_bridge_crossed, graph = graph) %>% 
    flatten_chr() %>% 
    na.omit() %>% 
    fct_count(sort = TRUE)
  
  max_times_crossed <- first(times_bridges_crossed$n)
  bridge_most_crossed <- first(times_bridges_crossed$f)
  mean_times_crossed <- mean(times_bridges_crossed$n)
  
  tibble(
    n_steps,
    starting_point,
    ending_point,
    total_distance,
    mean_times_crossed,
    max_times_crossed,
    bridge_most_crossed
  )
}
