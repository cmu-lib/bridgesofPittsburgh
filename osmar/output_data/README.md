# Node attributes

| field name        | field description                                                       |
|-------------------|-------------------------------------------------------------------------|
| `id`                | node index                                                            |
| `osm_node_id`       | OSM `Node` id (View online at `https://www.openstreetmap.org/node/osm_node_id`) |
| `lat`               | Latitude                                                              |
| `lon`               | Longitude                                                             |
| `osm_label`         | OSM label for this node if it exists                                  |

# Edge attributes

| field name          | field description                                                                                       |
|---------------------|---------------------------------------------------------------------------------------------------------|
| `from`              | node index                                                                                              |
| `to`                | node index                                                                                              |
| `id`                | edge index                                                                                              |
| `osm_way_id`        | OSM `Way` id (View online at `https://www.openstreetmap.org/way/osm_way_id`)                             |
| `osm_bridge_relation_id`   | OSM parent `Relation` id for this edge if it exists (View online at `https://www.openstreetmap.org/relation/osm_bridge_relation_id`) | 
| `osm_bridge`             | OSM metadata field                                                                                      |
| `osm_highway`             | OSM metadata field                                                                                      |
| `osm_label`             | OSM metadata field                                                                                      |
| `bridge_id`         | Which bridge (either OSM `Way` or `Relation`) does this edge belong to?                                 |
| `distance`          | Distance in meters between the terminal nodes of this edge                                              |
| `within_boundaries` | Boolean. Does at least one of this edge's endpoints fall within the Pittsburgh administrative boundary? |

## Notes

All edges are directed. Two-way roads are represented by reciprocal edges between the same nodes, and should have identical attributes other than reversed values for `from` and `to`, and a distinct `id`. Each edge represents a straight line in physical space, therefore curved roads may be represented by many contiguous edges.

`osm_node_id`, `osm_way_id`, and `osm_bridge_relation_id` are references to original OSM identifiers, and you can use those values to construct URLs to view those map features online as indicated by the field descriptions.

All attributes prefixed with `osm_` are data preserved from the original OSM export. The values in `osm_bridge` and `osm_highway` in particular are only semi-normalized (for example, `osm_bridge` in this data set can have the values `NULL`, `yes`, and `viaduct`!) and so are included here for reference only. Further information on how OSM tries to standardize the values for these fields can be found on the ["Key:bridge"](https://wiki.openstreetmap.org/wiki/Key:bridge) and ["Key:highway"](https://wiki.openstreetmap.org/wiki/Key:highway) entries on their internal Wiki pages.

`bridge_id` is the identifier for edge-groups that we would like our pathway to cross. (Note that we exclude some edges like highway onramps that may be called a "bridge" in OSM's data model - while they are present in the graph and may be traversed, we do not _require_ them to be traversed, and so do not assign a `bridge_id`.) The `bridge_id` value also marks which edges belong to the same groups/bundles, necessary to describe cases where a bridge either comprises a series of contiguous edges representing a curve (e.g. the [Wabash Tunnel](https://www.openstreetmap.org/way/54223852)) or several non-contiguous edges representing multiple lanes that may or may not intersect with each other (e.g. [Veteran's Bridge](https://www.openstreetmap.org/relation/4246068)). In cases where a bridge has multiple non-contiguous edges, you will note that we have used the `osm_bridge_relation_id` (from OSM's original data model) to assign the `bridge_id` value.

## Path constraints

For our problem, if we traverse any edge(s) within an edge group defined by `bridge_id`, we consider all of the edges with that same `bridge_id` "crossed". For example, if the path traverses one lane of Veteran's bridge, we would like to consider that entire bridge crossed, and forbid (or discourage) the path from traversing _any_ of the edges on that bridge again.

`pgh_bundles.json` contains a list of lists of edge `id`s, but is just an alternate representation of edge groups defined by `bridge_id` offered for convenience.
