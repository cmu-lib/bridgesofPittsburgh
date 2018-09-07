This is a folder for collapsed parallel lanes.
 - **full_20180709_gml**<br> 
 This is an input graphml data file for creating pseudo nodes and edges in python code. The data file is created on July 9, 2018. Since everytime the dataset will be updated from Open Street Map, I stored the original data downloaded on July 9,2018. Adding additional attributes to the original dataset and simplifying the original network are all based on this dataset.
 - **full_20180709_simplified_gml**<br> 
 This is the output graphml data file for "test-pseudo_nodes_edges.ipynb". The dataset is a new network with simplified lanes on river bridges.

 - **parallel_riverbridges.csv**<br> 
 This is a temporary data file which stores parallel edges data for river bridges. Every row is a parallel river bridge (two lanes) with "from" node info and of "to" node info.

 - **test-pseudo_nodes_edges.ipynb**<br> 
 This is a python code file that is used to add pseudo nodes to simplify network for parallel river bridges. The input data are "full_20180709_gml" and "parallel_riverbridges.csv". The output data is "full_20180709_simplified_gml".
