# osmar

We use [osmar](https://cran.r-project.org/package=osmar) to process the original XML output from Open Street Map into graphs and images.

Both a full dataset as well as an example subset are found in `input_data`.

Edge and node lists for a Pittsburgh road graph ready for routefinding are generated in `output_data`.

## Processing steps

1. Read the OSM XML into R
1. Use the `Relation` metadata layer from OSM to identify sets of `Ways` that should be considered bridges
1. Convert the OSM object into a network, enhancing edges with bridge identity information
1. Use a greedy local search algorithm to find shortest paths starting from the terminus point of one bridge.
  1. Cross bridge
  1. Every bridge edge set that is crossed should have its weight increased dramatically, so it is possibly for the path to backtrack but only as a last resort.
  1. Find next closest bridge
1. Repeat until all bridge edge sets have been visited at least once.
