- **2019_community_analysis** <br>
This folder contains all the files necessary to visualize and analyze road intersections by neighborhoods.  This includes a CSV of gephi-generated modularity data for each intersection and shapefiles for the pittsburgh roads/intersections; Pittsburgh neighborhoods as downloaded from WPRDC; and Pittsburgh neighborhoods joined to the intersections

pgh_intersections_by_neighbordhood contains the QGIS files where you can view this data already imported. The component pieces for this QGIS file are also primarily in this folder.

full_road_network and simplified_road_network contain the node/edge lists generated from the OSM data elsewhere (currently in the osmar/output_data folder) as well as Gephi files of the node/edges and csvs of the nodes exported from Gephi with calculated modularity.

intersections_with_modularity_and_neighborhoods.csv contains the export of the attribute table generated via pgh_intersections_by_neighbordhood. NB: OSM provided official neighborhood data that exactly maps to the data in the WPRDC file but doesn't have the neighborhood numbers.

2019community_analysis_full_output.py analyzes the intersections_with_modularity_and_neighborhoods.csv to produce Jaccard indices for the intersections (JaccardIndices.csv). NB: several things are hard-coded into this program including the csv filename and may need to be tweaked if reproducing this workflow with different data.

2019community_analysis.py does the same as above but only outputs instances where the Jaccard index is nonzero.