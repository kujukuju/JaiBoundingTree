#load "../module.jai";

main :: () {
    tree: Tree(u32) = Tree(u32).create();
    defer Tree(u32).free(tree);

    aabb1 := AABB.{.{1, 1, 1}, .{2, 2, 2}};
    create_proxy(*tree, aabb1, 1);

    aabb2 := AABB.{.{3, 3, 3}, .{4, 3, 3}};
    create_proxy(*tree, aabb2, 2);

    aabb_test_1 := AABB.{.{0.5, 1.5, 1.5}, .{1.5, 1.5, 1.5}};
    query(tree, aabb_test_1, (data: u32) -> bool {
        print("found data 1 %\n", data);
        return true;
    });

    aabb_test_middle := AABB.{.{2.4, 2.4, 2.4}, .{2.6, 2.6, 2.6}};
    query(tree, aabb_test_middle, (data: u32) -> bool {
        print("found data middle %\n", data);
        return true;
    });

    aabb_test_2 := AABB.{.{3.5, 2, 2}, .{3.5, 4, 4}};
    query(tree, aabb_test_2, (data: u32) -> bool {
        print("found data 2 %\n", data);
        return true;
    });

    aabb_test_all := AABB.{.{0, 0, 0}, .{8, 8, 8}};
    query(tree, aabb_test_all, (data: u32) -> bool {
        print("found data all %\n", data);
        return true;
    });

    ray_test_all := Ray.{.{0, 0, 0}, .{1, 1, 1}};
    ray_cast(tree, ray_test_all, (ray: Ray, value: u32) -> (bool, bool, Vector3) {
        print("hit aabb ray all %\n", value);
        return true, false, .{};
    });

    ray_test_2 := Ray.{.{-4, 1, 1}, .{1, 0, 0}};
    ray_cast(tree, ray_test_2, (ray: Ray, value: u32) -> (bool, bool, Vector3) {
        print("hit aabb ray 2 %\n", value);
        return true, false, .{};
    });

    segment_test_1 := Segment.{.{-4, 1, 1}, .{4, 1, 1}};
    ray_cast(tree, segment_test_1, (segment: Segment, value: u32) -> (bool, bool, Vector3) {
        print("hit aabb segment 1 %\n", value);
        return true, false, .{};
    });

    segment_test_all := Segment.{.{0, 0, 0}, .{3, 3, 3}};
    ray_cast(tree, segment_test_all, (segment: Segment, value: u32) -> (bool, bool, Vector3) {
        print("hit aabb segment all %\n", value);
        return true, false, .{};
    });

    segment_test_2 := Segment.{.{3.5, 2, 2}, .{3.5, 4, 4}};
    ray_cast(tree, segment_test_2, (segment: Segment, value: u32) -> (bool, bool, Vector3) {
        print("hit aabb segment 2 %\n", value);
        return true, false, .{};
    });
}