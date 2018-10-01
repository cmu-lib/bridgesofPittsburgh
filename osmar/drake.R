pkgconfig::set_config("drake::strings_in_dots" = "literals")
library(drake)
library(ggraph)
library(igraph)

source("osmar/osmar_graph.R")

pgh_plan <- drake_plan(
  pgh_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/pgh_osm.xml"))),
  pgh_graph = as_igraph(pgh_raw),
  tiny_raw = get_osm(complete_file(), source = osmsource_file(file_in("osmar/tiny.xml"))),
  tiny_graph = as_igraph(tiny_raw),
  tiny_tidy_graph = enrich_osmar_graph(tiny_raw, tiny_graph)
)

make(pgh_plan)

loadd(lazy = TRUE)

node_positions <- tiny_tidy_graph %>% activate(nodes) %>% select(x = lon, y = lat) %>% as_data_frame()
tiny_ly <- ggraph:::layout_igraph_manual(tiny_tidy_graph, node_positions, circular = NULL)

ggraph(tiny_tidy_graph) +
  geom_edge_arc(alpha = 0.2) +
  geom_node_point()
