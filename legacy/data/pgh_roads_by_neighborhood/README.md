- **pgh_roads_by_neighborhood** <br>
This folder contains all the files necessary to visualize and analyze road intersections by neighborhoods.  This includes a CSV of gephi-generated modularity data for each intersection and shapefiles for the pittsburgh roads/intersections; Pittsburgh neighborhoods as downloaded from WPRDC; Pittsburgh neighborhoods with a corrected CRS; and Pittsburgh neighborhoods joined to the roads

pgh_roads_by_neighborhood.qgs is a QGIS file where you can view this data already imported. The component pieces for this QGIS file are all also in this folder.

Although the pgh_road tables were originally generated and contained in the folder above this one, they have been duplicated here to avoid breaking the file paths necessary to view those tables in the QGIS file.

pgh_road-tables_with_modularity.csv contains the Gephi-generated modularity data for each intersection as originally generated in the pgh_road tables then imported into the Gephi file in the folder above this one.

pgh_neighborhoods shapefile was downloaded from WPRDC but had a different CRS from the pgh_road shapefile.  Therefore it has been transformed into the same CRS and that new shapefile is in pgh_neighbordhoods_correct_CRS.  (Yes, I noticed the typo afterwards, it hasn't seemed worth the effort of relinking the files to the QGIS file to fix the typo but someone else is welcome to do so and/or I might do so at a later date if it becomes a problem.)

The shapefile output from joining the pgh_neighbordhoods_correct_CRS with the modularity csv has been saved in pgh_roads_by_neighborhood_joined