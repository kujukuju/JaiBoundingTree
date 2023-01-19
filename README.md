## Jai Binary BVH Tree

API
```jai
create_bounding_tree :: ($T: Type) -> BoundingTree(T);
free :: (tree: BoundingTree);
create_proxy :: (tree: *BoundingTree($T), aabb: AABB3, user_data: T) -> int;
destroy_proxy :: (tree: *BoundingTree, proxy_id: int);
get_user_data :: inline (tree: BoundingTree($T), proxy_id: int) -> T;
query :: inline (tree: BoundingTree($T), aabb: AABB3, $callback: (value: T, data: *$D) -> bool, data: *D);
raycast :: inline (tree: BoundingTree(Triangle3), ray: Ray3, $callback: (point: Vector3, data: *$D) -> bool, data: *D);
raycast_hit :: inline (tree: BoundingTree(Triangle3), ray: Ray3) -> hit: bool;
raycast_nearest :: inline (tree: BoundingTree(Triangle3), ray: Ray3) -> nearest: Vector3, normal: Vector3, hit: bool;
raycast :: inline (tree: BoundingTree($T), ray: Ray3, $callback: (ray: Ray3, value: T, data: *$D) -> bool, data: *D);
raycast :: inline (tree: BoundingTree($T), segment: Line3, $callback: (segment: Line3, value: T, data: *$D) -> bool, data: *D);
raycast :: inline (tree: BoundingTree($T), segment: Line3, $callback: (segment: Line3, value: T, data: *$D) -> (hit: bool), data: *D);
```

It's basically expected that you construct the tree with a type of Triangle3. If not you will have to manually provide collision information as the callback api requests it.

> NOTE: This library is still experimental and in development, the API may change and it may not function exactly as intended.

A 3d implemention of a binary bvh tree natively in jai.

Implemented using notes from Erin Catto `https://box2d.org/files/ErinCatto_DynamicBVH_GDC2019.pdf` and box2d `https://github.com/erincatto/box2d/`.

See `examples/` for usage information.
