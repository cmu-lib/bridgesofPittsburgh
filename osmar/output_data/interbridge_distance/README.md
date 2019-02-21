# Interbridge distances

`interbridge_distance_matrix.csv` contains a CSV with an even number of rows and columns (~455 at last data export):
- The first column contains the origin node index numbers (these index numbers map to the `id` column in `osmar/output_data/pgh_nodes.csv`). 
- The first row contains the destination node index numbers. 
- Values are distance between those two nodes in meters along the edge path. 
    - `NA` values indicate that a distance was not calculated - this is in cases where two nodes are part of the same bridge. 
    - `Inf` values indicate that the distance was infinite, i.e. the destination node was unreachable from the origin node.
    
`bridge_node_correspondence.csv` contains a CSV with 2 columns:
- `node_index` maps to the `id` column in `osmar/output_data/pgh_nodes.csv`
- `bridge_id` maps to the `bridge_id` column in `osmar/output_data/pgh_edges.csv`. It indicates that this `node_index` is an entry/exit point for that bridge. Note: most nodes are an entry/exit point for only one bridge, however there are ~4 nodes that are incident to 2 edges that belong to different bridges - for example the [overpass at East 5th Avneue and Homstead Grays Bridge](https://www.openstreetmap.org/relation/9340973) are two bridges that intersect.
