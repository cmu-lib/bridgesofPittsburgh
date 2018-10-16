# osmar

We use [osmar](https://cran.r-project.org/package=osmar) to process the original XML output from Open Street Map into graphs and images.

Both a full dataset as well as an example subset are found in `input_data`.

Edge and node lists for a Pittsburgh road graph ready for routefinding are generated in `output_data`.

## Processing steps

1. Read the OSM XML into R
1. Use the `Relation` metadata layer from OSM to identify sets of `Ways` that should be considered bridges
1. Convert the OSM object into a network, enhancing edges with bridge identity information
1. Identify which bridges contain multiple `Ways` and will need to be rewired into a single new synthetic edge with two new endpoints.
  1. First, simplify all bridges into single edges, rather than the chains of edges that are produced by the initial conversion of `Ways` into a graph
  1. Next, for each set of edges that we know to comprise one physical bridge, compute two new centroids for its sets of endpoints, draw a new edge between these new nodes, and then rewire its original endpoints to feed into these new termini
1. Filter the graph so that only one large connected component is left
1. Calculate rough distances for all edges based on the coordinates of their endpoints.
