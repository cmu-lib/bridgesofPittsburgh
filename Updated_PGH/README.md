### This is a folder for updated data.  

- **edges.csv**<br> 
This data file is exported from ArcGIS. I used ArcGIS tool to identify river bridges and edges that are outside Pittsburgh. In ArcGIS, I added two additional columns and value every column with 0 or 1 to store river_bridge and out_pittsburgh information. I also used this file later to add additional attributes in Python code.

- **nodes.csv**<br>
This data file is exported from ArcGIS with similar steps as edges.csv file did.

- **nodes_hood.csv**<br>
This data file is exported from ArcGIS to store some information about nodes and their neighborhood.  

- **test-pseudo_nodes_edges.ipynb**<br>
This python code file is a demo code used to add pseudo nodes on river bridges to solve the collapsed parallel lanes problem. The input file is a graphml file "full_20180709_gml".

- **pgh_road**<br>
This is a folder to store shapefiles of edges and nodes.

- **pseudo_nodes**<br>
This folder includes data and code that I used to create pseudo nodes(some README details are in this folder).

- **add_attr.ipynb**<br>
This is a python code file which is used to add addtional attributes from "edges.csv" and "nodes.csv" file. The input files are "edges.csv" and "nodes.csv". The output file is "pgh_road_marked.graphml".

- **build_graph.py**<br>
This is a python code file which is used for testing whether the number of edges and nodes built by networkX and OSMnx package are the same.

- **pgh_node_ngh.graphml**<br>
This is a graphml file for nodes with neighborhood information inside. 

- **pgh_road_marked_final.graphml**<br>
This is the final graphml file for marked Pittsburgh road information inside. Since some of the output created by "add_attr.ipynb" are inconsistent with real data. I revised and corrected the information by hand, for example, some oneway information, from "pgh_raod_marked.graphml." Therefore, this is the final marked graphml file with correct info.