# osmar

We use [osmar](https://cran.r-project.org/package=osmar) to process the original XML output from Open Street Map into graphs and images.

Both a full data set as well as an example subset are found in `input_data`.

Edge and node lists for a Pittsburgh road graph ready for route-finding are generated in `output_data`.

## Processing steps

1. Read the OSM XML into R
1. Use the `Relation` metadata layer from OSM to identify sets of `Ways` that should be considered bridges
1. Convert the OSM object into a network, enhancing edges with bridge identity information
1. Use a greedy local search algorithm to find shortest paths starting from the terminus point of one bridge.
  1. Cross bridge
  1. Every bridge edge set that is crossed should have its weight increased dramatically, so it is possibly for the path to backtrack but only as a last resort.
  1. Find next closest bridge
1. Repeat until all bridge edge sets have been visited at least once.

## Running with Drake

We use [drake], an incremental build framework for R that will cache the results of long-running operations (such as constructing the initial large graph) so that they only need to be re-run if dependencies (such as the bounding box of the original data download) are changed.

Run `source("drake.R")` to load this build process, then run [`make(pgh_plan, ...)`](https://ropensci.github.io/drake/reference/make.html) with your desired arguments. On large maps with many bridges, this can take many hours to generate and then evaluate pathways form different starting bridges. If interrupted, however, you can restart the script and it will pick up where it left off. See ["High-performance computing with drake"](https://ropenscilabs.github.io/drake-manual/hpc.html) for advice on running build jobs in parallel and/or on a remote cluster.
