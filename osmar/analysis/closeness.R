library(drake)
library(igraph)
library(tidyverse)
library(konigsbergr)
library(mapview)

loadd(pgh_tidy_graph)
loadd(simplified_graph)
pgh_contract <- function() {
  mapping_contract <- as.integer(as.factor(V(simplified_graph)$neighborhood))
  new_seq <- as.integer(seq(max(mapping_contract, na.rm = TRUE) + 1, length.out = length(mapping_contract)))
  mapping_contract = coalesce(mapping_contract, new_seq)
  
  contracted_graph <- as_tbl_graph(contract(simplified_graph, mapping = mapping_contract,
                                            vertex.attr.comb = list(lat = "mean", lon = "mean", neighborhood = "first", "ignore"))) %>% 
    activate(edges) %>% 
    filter(within_boundaries) %>% 
    activate(nodes) %>% 
    filter(!is.na(neighborhood))
  
  contracted_graph
}


analysis_plan <- evaluate_plan(drake_plan(
  modified_closeness = removed_closeness(simplified_graph, bid__)),
  wildcard = "bid__", values = na.omit(unique(edge_attr(simplified_graph, "bridge_id")))
)

removed_closeness <- function(simplified_graph, b) {
  remove_edges <- which(edge_attr(simplified_graph, "bridge_id") == b)
  pruned_graph <- delete_edges(simplified_graph, remove_edges)
  closeness(pruned_graph, mode = "out", weights = V(pruned_graph)$distance, normalized = TRUE)
}

close_plan <- bind_plans(
  gather_plan(analysis_plan, target = "closeness_changes", gather = "c", append = TRUE),
  drake_plan(
    baseline_closeness = closeness(simplified_graph, mode = "out", weights = V(simplified_graph)$distance, normalized = TRUE),
    close_table = tibble(
      bridge_id = rep(na.omit(unique(edge_attr(simplified_graph, "bridge_id"))), length.out = length(closeness_changes)), 
      closeness_changes, 
      baseline_closeness = rep(baseline_closeness, length.out = length(closeness_changes))) %>% 
      mutate(delta = baseline_closeness - closeness_changes),
    impact_summaries = close_table %>% 
      group_by(bridge_id) %>% 
      summarize_at(vars(closeness_changes, delta), funs(mean, q95 = quantile(., p = .95), q99 = quantile(., p = .99)))
  )
)

bridge_lookup <- as_tibble(pgh_tidy_graph, "edges") %>% 
  filter(!is.na(bridge_id)) %>% 
  distinct(bridge_id, label) %>% 
  distinct(bridge_id, .keep_all = TRUE)

impact_summaries %>% 
  left_join(bridge_lookup, by = "bridge_id") %>% 
  View()

sl_sf <- nodes_to_sf(simplified_graph, V(simplified_graph)$lat, V(simplified_graph)$lon)

loadd(close_table)

removed_map <- sl_sf %>% 
  bind_cols(filter(close_table, bridge_id == 9155851))

plot(removed_map["delta"])

V(simplified_graph)$cl <- ec

plot(nodes_to_sf(simplified_graph, V(simplified_graph)$lat, V(simplified_graph)$lon)["cl"])

bridge_ids <- na.omit(unique(E(simplified_graph)$bridge_id))

library(tidygraph)
library(furrr)

plan(multiprocess)
closeness_removals <- future_map(bridge_ids, function(b) {
  simplified_graph %>% 
    activate(edges) %>% 
    filter(is.na(bridge_id) | bridge_id != b) %>% 
  closeness(mode = "out", weights = V(simplified_graph)$distance, normalized = TRUE)
})

wide_offsets <- closeness_removals %>% 
  map(~ ec - .x) %>% 
  bind_cols() %>% 
  set_names(bridge_ids)
  
clos_offsets <- wide_offsets %>% 
  gather(key = bridge_id, value = closeness_change, convert = TRUE)

clos_offsets_summary <- clos_offsets %>% 
  group_by(bridge_id) %>% 
  summarize(
    mean_change = mean(closeness_change),
    sd_change = sd(closeness_change)
  ) %>% 
  arrange(desc(mean_change))

simp_sf <- nodes_to_sf(simplified_graph, V(simplified_graph)$lat, V(simplified_graph)$lon)  
clos_sf <- bind_cols(simp_sf, wide_offsets)

plot(clos_sf[as.character(clos_offsets_summary$bridge_id[25])], logz = TRUE)
