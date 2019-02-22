# Bridge distance matrices

`bridge_node_correspondence.csv` contains a CSV with 2 columns:
- `node_index` maps to the `id` column in `osmar/output_data/pgh_nodes.csv`
- `bridge_id` maps to the `bridge_id` column in `osmar/output_data/pgh_edges.csv`. It indicates that this `node_index` is an entry/exit point for that bridge. Note: most nodes are an entry/exit point for only one bridge, however there are ~4 nodes that are incident to 2 edges that belong to different bridges - for example the [overpass at East 5th Avneue and Homstead Grays Bridge](https://www.openstreetmap.org/relation/9340973) are two bridges that intersect.

Each of the additional directories contains distance and path information for three versions of the graph:

- `all` The entire graph, including both bridges and roads
- `inter_bridge` The graph with all bridge edges removed
- `intra_bridge` The graph with all non-bridge edges removed

Each has two files:

`*_distance_matrix.csv` contains a CSV with an even number of rows and columns (~455 at last data export):
- The first column contains the origin node index numbers (these index numbers map to the `id` column in `osmar/output_data/pgh_nodes.csv`). 
- The first row contains the destination node index numbers. 
- Values are distance between those two nodes in meters along the edge path. 
    - Empty values indicate that a distance was not calculated - e.g., for `inter_distance_matrix.csv` because the origin and destination node belong to the same graph. 
    - `Inf` values indicate that the distance was infinite, i.e. the destination node was unreachable from the origin node.
    
`*_pathways.json` is a JSON file of nested objects. The first level of keys represent the origin node index number (these index numbers map to the `id` column in `osmar/output_data/pgh_nodes.csv`), with their child keys representing the target index number. Each of these child keys points to an array of edge index ids describing the pathway from origin to target in the full graph. These edge index ids map to the `id` column in `osmar/output_data/pgh_edges.csv`. Where no path was available, the array is empty:

```json
{
  "530": {
    "530": [],
    "1548": [1603, 1604, 1605, 1606, 1601],
    }
    # etc...
}
```