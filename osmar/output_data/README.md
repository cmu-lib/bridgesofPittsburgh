# Node attributes

| field name        | field description                                                       | notes |
|-------------------|-------------------------------------------------------------------------|-------|
| `name`            | Unique OSM `Node` id                                                    |       |
| `lat`               | Latitude                                                                |       |
| `lon`               | Longitude                                                               |       |
| `component`         | Graph connected component id                                            |       |
| `is_interface`      | Node is a terminal for at least one bridge edge and one non-bridge edge |       |
| `associated_bridge` | If node `is_interface`, this is a joining ID to `bridge_id`             |       |

# Edge attributes

n.b. edges are directed. Two-way roads are represented by parallel edges between the same nodes, and other than reversed values for `from` and `to` and a distinct `.id`, have identical attributes

| field name          | field description                                                                                       | notes                                                           |
|---------------------|---------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| `from`              | node index                                                                                              | n.b. not node `name`                                            |
| `to`                | node index                                                                                              | n.b. not node `name`                                            |
| `name`              | OSM `Way` id                                                                                            | not unique - many edges can be used to represent a single `Way` |
| `access`            | OSM metadata field                                                                                      |                                                                 |
| `bridge`            | OSM metadata field                                                                                      |                                                                 |
| `bridge_name`       | OSM metadata field                                                                                      | Display name of the larger bridge represented with this edge    |
| `highway`           | OSM metadata field                                                                                      |                                                                 |
| `label`             | OSM metadata field                                                                                      | Display name of the associated OSM `Way`                        |
| `bridge_relation`   | OSM parent `Relation` id for this edge                                                                  | Present when bridge comprises several often-unconnected `Ways`  |
| `is_bridge`         | Is this edge part of a bridge according to our criterion?                                               |                                                                 |
| `bridge_id`         | Which bridge (either OSM `Way` or `Relation`) does this edge belong to?                                 |                                                                 |
| `distance`          | Geodesic distance in meters between the terminal nodes of this edge                                     |                                                                 |
| `within_boundaries` | Boolean. Does at least one of this edge's endpoints fall within the Pittsburgh administrative boundary? |                                                                 |
| `.id`               | Unique integer row number.                                                                              |                                                                 |
