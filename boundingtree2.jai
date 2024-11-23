
BoundingTree2 :: struct (T: Type) {
    root_index: s32 = NULL_NODE;

    nodes: [] Node2(T);
    node_count: s32;
    node_capacity: s32 = 16;

    free_list: s32;

    insertion_count: s32;

    stack: [..] s32;
}

Node2 :: struct (T: Type) {
    aabb: AABB2;

    user_data: T;

    // TODO I cant use unions until my serializer supports unions of pod, which is a whole project in itself
    // union {
        parent_index: s32;
        next: s32 = NULL_NODE;
    // }

    child1: s32;
    child2: s32;

    height: s32;

    moved: bool;
}

for_expansion :: (tree: BoundingTree2($T), body: Code, flags: For_Flags) #expand {
    #assert(!(flags & .REVERSE));

    for node, proxy_id: tree.nodes {
        if is_leaf(node) {
            #if flags & .POINTER {
                `it := *node.user_data;
            } else {
                `it := node.user_data;
            }

            `it_index := proxy_id;

            #insert body;
        }
    }
}

create_bounding_tree2 :: ($T: Type, initial_capacity: s32 = 0) -> BoundingTree2(T) {
    tree: BoundingTree2(T);
    if initial_capacity then tree.node_capacity = initial_capacity;
    tree.nodes = NewArray(tree.node_capacity, Node2(T), false);
    
    for i: 0..tree.node_capacity - 2 {
        tree.nodes[i].next = i + 1;
        tree.nodes[i].height = -1;
    }
    tree.nodes[tree.node_capacity - 1].next = NULL_NODE;
    tree.nodes[tree.node_capacity - 1].height = -1;

    return tree;
}

// when I name this free it collides with free *void idk why
free_bounding_tree :: (tree: BoundingTree2($T)) {
    array_free(tree.nodes);
    array_free(tree.stack);
}

tree_reset :: (tree: *BoundingTree2($T)) {
    tree.root_index = NULL_NODE;
    tree.node_count = 0;
    tree.free_list = 0;
    tree.insertion_count = 0;
    if tree.nodes.data {
        for i: 0..tree.node_capacity - 2 {
            tree.nodes[i].next = i + 1;
            tree.nodes[i].height = -1;
        }
        tree.nodes[tree.node_capacity - 1].next = NULL_NODE;
        tree.nodes[tree.node_capacity - 1].height = -1;
    }
    tree.stack.count = 0;
}

surface_area :: (a: AABB2) -> float {
    diff := a.upper - a.lower;
    return diff.x * diff.y;
}

intersection :: (ray: Ray2, aabb: AABB2) -> bool {
    t := 0.0; // set to -FLT_MAX to get first hit on line
    tmax := FLOAT32_MAX; // set to max distance ray can travel (for segment)
    // For all three slabs
    if abs(ray.dir.x) < EPSILON {
        // Ray is parallel to slab. No hit if origin not within slab
        if ray.p.x < aabb.lower.x || ray.p.x > aabb.upper.x {
            return false;
        }
    } else {
        // Compute intersection t value of ray with near and far plane of slab
        ood := 1.0 / ray.dir.x;
        t1 := (aabb.lower.x - ray.p.x) * ood;
        t2 := (aabb.upper.x - ray.p.x) * ood;
        // Make t1 be intersection with near plane, t2 with far plane
        if t1 > t2 {
            t1, t2 = swap(t1, t2);
        }
        // Compute the intersection of slab intersection intervals
        if t1 > t {
            t = t1;
        }
        if t2 > tmax {
            tmax = t2;
        }
        // Exit with no collision as soon as slab intersection becomes empty
        if t > tmax {
            return false;
        }
    }
    if abs(ray.dir.y) < EPSILON {
        // Ray is parallel to slab. No hit if origin not within slab
        if ray.p.y < aabb.lower.y || ray.p.y > aabb.upper.y {
            return false;
        }
    } else {
        // Compute intersection t value of ray with near and far plane of slab
        ood := 1.0 / ray.dir.y;
        t1 := (aabb.lower.y - ray.p.y) * ood;
        t2 := (aabb.upper.y - ray.p.y) * ood;
        // Make t1 be intersection with near plane, t2 with far plane
        if t1 > t2 {
            t1, t2 = swap(t1, t2);
        }
        // Compute the intersection of slab intersection intervals
        if t1 > t {
            t = t1;
        }
        if t2 > tmax {
            tmax = t2;
        }
        // Exit with no collision as soon as slab intersection becomes empty
        if t > tmax {
            return false;
        }
    }

    // Ray intersects all 3 slabs. Return point (q) and intersection t value (tmin)
    return true;
}

intersection :: (line: Line2, aabb: AABB2) -> bool {
    e := aabb.upper - aabb.lower;
    d := line.p2 - line.p1;
    m := line.p1 + line.p2 - aabb.lower - aabb.upper;
    // Try world coordinate axes as separating axes
    adx := abs(d.x);
    if abs(m.x) > e.x + adx {
        return false;
    }
    ady := abs(d.y);
    if abs(m.y) > e.y + ady {
        return false;
    }
    // Add in an epsilon term to counteract arithmetic errors when segment is
    // (near) parallel to a coordinate axis (see text for detail)
    adx += EPSILON;
    ady += EPSILON;
    // Try cross products of segment direction vector with coordinate axes
    if abs(m.x * d.y - m.y * d.x) > e.x * ady + e.y * adx {
        return false;
    }
    // No separating axis found; segment must be overlapping AABB
    return true;
}

is_leaf :: (node: Node2) -> bool {
    return node.child1 == NULL_NODE;
}

create_proxy :: (tree: *BoundingTree2($T), aabb: AABB2, user_data: T) -> int {
    assert(tree.nodes.data != null, "You have to create your tree before you try to use it.\n");
    
    proxy_id := allocate_node(tree);

    r := Vector2.{AABB_EXTENSION, AABB_EXTENSION};
    tree.nodes[proxy_id].aabb.lower = aabb.lower - r;
    tree.nodes[proxy_id].aabb.upper = aabb.upper + r;
    tree.nodes[proxy_id].user_data = user_data;
    tree.nodes[proxy_id].height = 0;
    tree.nodes[proxy_id].moved = true;

    insert_leaf(tree, proxy_id);

    return proxy_id;
}

destroy_proxy :: (tree: *BoundingTree2, proxy_id: int) {
	assert(0 <= proxy_id && proxy_id < tree.node_capacity);
	assert(is_leaf(tree.nodes[proxy_id]));

	remove_leaf(tree, cast(s32) proxy_id);
	free_node(tree, cast(s32) proxy_id);
}

move_proxy :: (tree: *BoundingTree2, proxy_id: int, aabb: AABB2, displacement: Vector3 = .{0, 0, 0}) -> bool {
	assert(0 <= proxy_id && proxy_id < tree.node_capacity);
	assert(is_leaf(tree.nodes[proxy_id]));

	// Extend AABB2
	fat_aabb: AABB2 = ---;
	r := Vector2.{AABB_EXTENSION, AABB_EXTENSION};
    fat_aabb.lower = aabb.lower - r;
    fat_aabb.upper = aabb.upper + r;

	// Predict AABB2 movement
    // TODO I don't really understand what's happening here
    d := AABB_MULTIPLIER * displacement;

	if d.x < 0 {
		fat_aabb.lower.x += d.x;
	} else {
		fat_aabb.upper.x += d.x;
	}

	if d.y < 0 {
		fat_aabb.lower.y += d.y;
	} else {
		fat_aabb.upper.y += d.y;
	}

    tree_aabb := tree.nodes[proxy_id].aabb;
	if contains(tree_aabb, aabb) {
		// The tree AABB3 still contains the object, but it might be too large.
		// Perhaps the object was moving fast but has since gone to sleep.
		// The huge AABB3 is larger than the new fat AABB3.
		huge_aabb: AABB3 = ---;
		huge_aabb.lower = fat_aabb.lower - 4 * r;
		huge_aabb.upper = fat_aabb.upper + 4 * r;

		if contains(huge_aabb, tree_aabb) {
			// The tree AABB3 contains the object AABB3 and the tree AABB3 is
			// not too large. No tree update needed.
			return false;
		}

		// Otherwise the tree AABB3 is huge and needs to be shrunk
	}

	remove_leaf(tree, cast(s32) proxy_id);

	tree.nodes[proxy_id].aabb = fat_aabb;

	insert_leaf(tree, cast(s32) proxy_id);

	tree.nodes[proxy_id].moved = true;

	return true;
}

get_user_data :: inline (tree: BoundingTree2($T), proxy_id: int) -> T {
	assert(0 <= proxy_id && proxy_id < tree.node_capacity);
	return tree.nodes[proxy_id].user_data;
}

was_moved :: inline (tree: BoundingTree2, proxy_id: int) -> bool {
	assert(0 <= proxy_id && proxy_id < tree.node_capacity);
	return tree.nodes[proxy_id].moved;
}

clear_moved :: inline (tree: *BoundingTree2, proxy_id: int) {
	assert(0 <= proxy_id && proxy_id < tree.node_capacity);
	tree.nodes[proxy_id].moved = false;
}

get_fat_aabb :: inline (tree: BoundingTree2, proxy_id: int) -> AABB3 {
	assert(0 <= proxy_id && proxy_id < tree.node_capacity);
	return tree.nodes[proxy_id].aabb;
}

BoundingQuery2 :: struct (T: Type) {
    tree: *BoundingTree2(T);
    aabb: AABB2;
    stack: *[..] s32;
}

query :: (tree: *BoundingTree2($T), aabb: AABB2, stack: *[..] s32 = null) -> BoundingQuery2(T) {
    return BoundingQuery2(T).{tree, aabb, stack};
}

for_expansion :: (query: BoundingQuery2($T), body: Code, flags: For_Flags) #expand {
    // not sure what reverse would mean for a tree, although we could pop a stack I guess
    #assert(!(flags & .REVERSE));

    tree := query.tree;
    aabb := query.aabb;

    stack := query.stack;
    if !stack {
        stack = *tree.stack;
    }
    stack.count = 0;

    stack_push(stack, tree.root_index);

	while !stack_empty(stack) {
		node_id := stack_pop(stack);
		if node_id == NULL_NODE {
			continue;
		}

		if intersects(tree.nodes[node_id].aabb, aabb) {
            // TODO does this syntax perform a copy?
			if is_leaf(tree.nodes[node_id]) {
                `it := tree.nodes[node_id].user_data;
                `it_index := node_id;

                #insert body;
			} else {
                stack_push(stack, tree.nodes[node_id].child1);
                stack_push(stack, tree.nodes[node_id].child2);
			}
		}
	}
}

query_single :: (tree: *BoundingTree2($T), aabb: AABB2) -> T, bool {
    for data: query(tree, aabb) {
        return data, true;
    }

    empty: T;
    return empty, false;
}

raycast :: inline (tree: BoundingTree2($T), ray: Ray2, callback: (ray: Ray2, value: T) -> bool, safe_stack: *[..] s32 = null) {
    stack := safe_stack;
    if !stack {
        stack = *tree.stack;
    }
    stack.count = 0;

    stack_push(stack, tree.root_index);

    while !stack_empty(stack) {
        index := stack_pop(stack);

        if index == NULL_NODE {
            continue;
        }

        if !intersection(ray, tree.nodes[index].aabb) {
            continue;
        }

        if is_leaf(tree.nodes[index]) {
            cont := callback(ray, tree.nodes[index].user_data);
            if !cont {
                return;
            }
        } else {
            stack_push(stack, tree.nodes[index].child1);
            stack_push(stack, tree.nodes[index].child2);
        }
    }
}

raycast :: inline (tree: BoundingTree2($T), ray: Ray2, $callback: (ray: Ray2, value: T, data: $D) -> bool, data: D, safe_stack: *[..] s32 = null) {
    stack := safe_stack;
    if !stack {
        stack = *tree.stack;
    }
    stack.count = 0;

    stack_push(stack, tree.root_index);

    while !stack_empty(stack) {
        index := stack_pop(stack);

        if index == NULL_NODE {
            continue;
        }

        if !intersection(ray, tree.nodes[index].aabb) {
            continue;
        }

        if is_leaf(tree.nodes[index]) {
            cont := inline callback(ray, tree.nodes[index].user_data, data);
            if !cont {
                return;
            }
        } else {
            stack_push(stack, tree.nodes[index].child1);
            stack_push(stack, tree.nodes[index].child2);
        }
    }
}

// raycast :: inline (tree: BoundingTree2($T), ray: Ray3, $callback: (ray: Ray3, value: T) -> (hit: bool)) -> (hit: bool) {
//     hit: bool;
//     hit, point, data := raycast(tree, ray, (ray: Ray3, value: T) -> bool {
//         if callback(ray, value) {
//             return false;
//         }

//         return true, false, .{};
//     });

//     return hit;
// }

raycast :: inline (tree: BoundingTree2($T), segment: Line2, $callback: (segment: Line2, value: T, data: $D) -> bool, data: D, safe_stack: *[..] s32 = null) {
    stack := safe_stack;
    if !stack {
        stack = *tree.stack;
    }
    stack.count = 0;

    push(stack, tree.root_index);

    segment_aabb: AABB2 = ---;
    segment_aabb.lower = min(segment.p1, segment.p2);
    segment_aabb.upper = max(segment.p1, segment.p2);

    current_segment := segment;

    while !stack_empty(stack) {
        index := stack_pop(stack);

        if index == NULL_NODE {
            continue;
        }

        if !intersection(segment_aabb, tree.nodes[index].aabb) {
            continue;
        }

        // TODO you can apparently do an optimization here that has to do with the
        // separating axis for segment (Gino, p80) but I'm not sure how to do it in 3 dimensions

        if !intersection(current_segment, tree.nodes[index].aabb) {
            continue;
        }

        if is_leaf(tree.nodes[index]) {
            cont := inline callback(current_segment, tree.nodes[index].user_data, data);
            if !cont {
                return;
            }

            if hit {
                current_segment.p2 = point;

                segment_aabb.lower = min(segment.p1, point);
                segment_aabb.upper = max(segment.p1, point);
            }
        } else {
            stack_push(stack, tree.nodes[index].child1);
            stack_push(stack, tree.nodes[index].child2);
        }
    }
}

raycast :: inline (tree: BoundingTree2($T), segment: Line2, $callback: (segment: Line2, value: T, data: $D) -> (hit: bool), data: D, safe_stack: *[..] s32 = null) {
    // TODO this could be improved with scope context capturing
    hit, point, data := raycast(tree, segment, (segment: Line2, value: T) -> (bool, bool, Vector2) {
        if inline callback(segment, value) {
            return false, true, .{};
        }

        return true, false, .{};
    }, safe_stack);

    return hit;
}

validate :: (tree: BoundingTree2) {
    #if TREE_DEBUG {
        validate_structure(tree, tree.root_index);
        validate_metrics(tree, tree.root_index);

        free_count := 0;
        free_index := tree.free_list;
        while free_index != NULL_NODE {
            assert(0 <= free_index && free_index < tree.node_capacity);
            free_index = tree.nodes[free_index].next;
            free_count += 1;
        }

        assert(get_height(tree) == compute_height(tree));

        assert(tree.node_count + free_count == tree.node_capacity);
    }
}

get_height :: (tree: BoundingTree2) -> int {
	if (tree.root_index == NULL_NODE) {
		return 0;
	}

	return tree.nodes[tree.root_index].height;
}

get_max_balance :: (tree: BoundingTree2($T)) -> int {
	max_balance := 0;

    for i: 0..tree.node_capacity - 1 {
		if tree.nodes[i].height <= 1 {
			continue;
		}

		assert(is_leaf(tree.nodes[i]) == false);

		child1 := tree.nodes[i].child1;
		child2 := tree.nodes[i].child2;
		balance := abs(tree.nodes[child2].height - tree.nodes[child1].height);
		max_balance = max(max_balance, balance);
	}

	return max_balance;
}

get_area_ratio :: (tree: BoundingTree2($T)) -> float {
	if (tree.root_index == NULL_NODE) {
		return 0;
	}

    root: *Node2(T) = *tree.nodes[tree.root_index];
	root_area := surface_area(root.aabb);

	total_area := 0.0;
    for i: 0..tree.node_capacity - 1 {
        node: *Node2(T) = *tree.nodes[i];
		if node.height < 0 {
			// Free node in pool
			continue;
		}

		total_area += surface_area(node.aabb);
	}

	return total_area / root_area;
}

rebuild_bottom_up :: (tree: *BoundingTree2($T)) {
    nodes: [] int = NewArray(tree.node_count, int, false);
	count := 0;

	// Build array of leaves. Free the rest.
    for i: 0..tree.node_capacity - 1  {
		if tree.nodes[i].height < 0 {
			// free node in pool
			continue;
		}

		if is_leaf(tree.nodes[i]) {
			tree.nodes[i].parent_index = NULL_NODE;
			nodes[count] = i;
			count += 1;
		} else {
			free_node(tree, i);
		}
	}

	while count > 1 {
		min_cost: float = 0xffffffff;
		imin := -1;
        jmin := -1;
        for i: 0..count - 1 {
			aabbi := tree.nodes[nodes[i]].aabb;

            for j: i + 1..count - 1 {
				aabbj := tree.nodes[nodes[j]].aabb;
				b := merge(aabbi, aabbj);
				cost := surface_area(b);
				if cost < min_cost {
					imin = i;
					jmin = j;
					min_cost = cost;
				}
			}
		}

		index1 := nodes[imin];
		index2 := nodes[jmin];
        child1: *Node2(T) = *tree.nodes[index1];
        child2: *Node2(T) = *tree.nodes[index2];

		parent_index := allocate_node(tree);
        parent: *Node2(T) = *tree.nodes[parent_index];
		parent.child1 = index1;
		parent.child2 = index2;
		parent.height = 1 + max(child1.height, child2.height);
		parent.aabb = merge(child1.aabb, child2.aabb);
		parent.parent_index = NULL_NODE;

		child1.parent_index = parent_index;
		child2.parent_index = parent_index;

		nodes[jmin] = nodes[count - 1];
		nodes[imin] = parent_index;
		count -= 1;
	}

	tree.root_index = nodes[0];
	array_free(nodes);

	validate(tree);
}

shift_origin :: (tree: *BoundingTree2, new_origin: Vector2) {
	// Build array of leaves. Free the rest.
    for i: 0..tree.node_capacity - 1 {
		tree.nodes[i].aabb.lower -= new_origin;
		tree.nodes[i].aabb.upper -= new_origin;
	}
}

#scope_file

allocate_node :: (tree: *BoundingTree2($T)) -> s32 {
	// Expand the node pool as needed.
	if tree.free_list == NULL_NODE {
		assert(tree.node_count == tree.node_capacity);

		// The free list is empty. Rebuild a bigger pool.
        old_nodes: [] Node2(T) = tree.nodes;
        tree.node_capacity *= 2;
        tree.nodes = NewArray(tree.node_capacity, Node2(T), false);
        memcpy(tree.nodes.data, old_nodes.data, tree.node_count * size_of(Node2(T)));
        free(old_nodes.data);

		// Build a linked list for the free list. The parent
		// pointer becomes the "next" pointer.
        for i: tree.node_count..tree.node_capacity - 2 {
			tree.nodes[i].next = i + 1;
			tree.nodes[i].height = -1;
		}
        tree.nodes[tree.node_capacity - 1].next = NULL_NODE;
        tree.nodes[tree.node_capacity - 1].height = -1;
        tree.free_list = tree.node_count;
	}

	// Peel a node off the free list.
    node_id := tree.free_list;
	tree.free_list = tree.nodes[node_id].next;
	tree.nodes[node_id].parent_index = NULL_NODE;
	tree.nodes[node_id].child1 = NULL_NODE;
	tree.nodes[node_id].child2 = NULL_NODE;
	tree.nodes[node_id].height = 0;
	// tree.nodes[node_id].user_data = null;
	tree.nodes[node_id].moved = false;
	tree.node_count += 1;
	return node_id;
}

free_node :: (tree: *BoundingTree2, node_id: s32) {
    assert(0 <= node_id && node_id < tree.node_capacity);
	assert(0 < tree.node_count);
	tree.nodes[node_id].next = tree.free_list;
	tree.nodes[node_id].height = -1;
	tree.free_list = node_id;
	tree.node_count -= 1;
}

insert_leaf :: (tree: *BoundingTree2($T), leaf: s32) {
    tree.insertion_count += 1;

	if tree.root_index == NULL_NODE {
		tree.root_index = leaf;
		tree.nodes[tree.root_index].parent_index = NULL_NODE;
		return;
	}

	// Find the best sibling for this node
	leaf_aabb := tree.nodes[leaf].aabb;
	index := tree.root_index;
	while !is_leaf(tree.nodes[index]) {
		child1 := tree.nodes[index].child1;
		child2 := tree.nodes[index].child2;

		area := surface_area(tree.nodes[index].aabb);

		combined_aabb := merge(tree.nodes[index].aabb, leaf_aabb);
		combined_area := surface_area(combined_aabb);

		// Cost of creating a new parent for this node and the new leaf
		cost := 2.0 * combined_area;

		// Minimum cost of pushing the leaf further down the tree
		inheritance_cost := 2.0 * (combined_area - area);

		// Cost of descending into child1
        cost1: float;
		if is_leaf(tree.nodes[child1]) {
			aabb := merge(leaf_aabb, tree.nodes[child1].aabb);
			cost1 = surface_area(aabb) + inheritance_cost;
		} else {
			aabb := merge(leaf_aabb, tree.nodes[child1].aabb);
			old_area := surface_area(tree.nodes[child1].aabb);
			new_area := surface_area(aabb);
			cost1 = new_area - old_area + inheritance_cost;
		}

		// Cost of descending into child2
		cost2: float;
		if is_leaf(tree.nodes[child2]) {
			aabb := merge(leaf_aabb, tree.nodes[child2].aabb);
			cost2 = surface_area(aabb) + inheritance_cost;
		} else {
			aabb := merge(leaf_aabb, tree.nodes[child2].aabb);
			old_area := surface_area(tree.nodes[child2].aabb);
			new_area := surface_area(aabb);
			cost2 = new_area - old_area + inheritance_cost;
		}

		// Descend according to the minimum cost.
		if cost < cost1 && cost < cost2 {
			break;
		}

		// Descend
		if cost1 < cost2 {
			index = child1;
		} else {
			index = child2;
		}
	}

	sibling := index;

	// Create a new parent.
	old_parent := tree.nodes[sibling].parent_index;
	new_parent := allocate_node(tree);
	tree.nodes[new_parent].parent_index = old_parent;
    uninitialized: T = ---;
	tree.nodes[new_parent].user_data = uninitialized;
	tree.nodes[new_parent].aabb = merge(leaf_aabb, tree.nodes[sibling].aabb);
	tree.nodes[new_parent].height = tree.nodes[sibling].height + 1;

	if old_parent != NULL_NODE {
		// The sibling was not the root.
		if tree.nodes[old_parent].child1 == sibling {
			tree.nodes[old_parent].child1 = new_parent;
		} else {
			tree.nodes[old_parent].child2 = new_parent;
		}

		tree.nodes[new_parent].child1 = sibling;
		tree.nodes[new_parent].child2 = leaf;
		tree.nodes[sibling].parent_index = new_parent;
		tree.nodes[leaf].parent_index = new_parent;
	} else {
		// The sibling was the root.
		tree.nodes[new_parent].child1 = sibling;
		tree.nodes[new_parent].child2 = leaf;
		tree.nodes[sibling].parent_index = new_parent;
		tree.nodes[leaf].parent_index = new_parent;
		tree.root_index = new_parent;
	}

	// Walk back up the tree fixing heights and AABB3s
	index = tree.nodes[leaf].parent_index;
	while index != NULL_NODE {
		index = balance(tree, index);

		child1 := tree.nodes[index].child1;
		child2 := tree.nodes[index].child2;

		assert(child1 != NULL_NODE);
		assert(child2 != NULL_NODE);

		tree.nodes[index].height = 1 + max(tree.nodes[child1].height, tree.nodes[child2].height);
		tree.nodes[index].aabb = merge(tree.nodes[child1].aabb, tree.nodes[child2].aabb);

		index = tree.nodes[index].parent_index;
	}

	//Validate();
}

remove_leaf :: (tree: *BoundingTree2, leaf: s32) {
    if leaf == tree.root_index {
		tree.root_index = NULL_NODE;
		return;
	}

	parent := tree.nodes[leaf].parent_index;
	grand_parent := tree.nodes[parent].parent_index;
	sibling: s32;
	if tree.nodes[parent].child1 == leaf {
		sibling = tree.nodes[parent].child2;
	} else {
		sibling = tree.nodes[parent].child1;
	}

	if grand_parent != NULL_NODE {
		// Destroy parent and connect sibling to grand_parent.
		if tree.nodes[grand_parent].child1 == parent {
			tree.nodes[grand_parent].child1 = sibling;
		} else {
			tree.nodes[grand_parent].child2 = sibling;
		}
		tree.nodes[sibling].parent_index = grand_parent;
		free_node(tree, parent);

		// Adjust ancestor bounds.
		index := grand_parent;
		while index != NULL_NODE {
			index = balance(tree, index);

			child1 := tree.nodes[index].child1;
			child2 := tree.nodes[index].child2;

			tree.nodes[index].aabb = merge(tree.nodes[child1].aabb, tree.nodes[child2].aabb);
			tree.nodes[index].height = 1 + max(tree.nodes[child1].height, tree.nodes[child2].height);

			index = tree.nodes[index].parent_index;
		}
	} else {
		tree.root_index = sibling;
		tree.nodes[sibling].parent_index = NULL_NODE;
		free_node(tree, parent);
	}

	//Validate();
}

balance :: (tree: *BoundingTree2($T), i_a: s32) -> s32 {
    assert(i_a != NULL_NODE);

    a: *Node2(T) = *tree.nodes[i_a];
	if is_leaf(<< a) || a.height < 2 {
		return i_a;
	}

	i_b := a.child1;
	i_c := a.child2;
	assert(0 <= i_b && i_b < tree.node_capacity);
	assert(0 <= i_c && i_c < tree.node_capacity);

    b: *Node2(T) = *tree.nodes[i_b];
    c: *Node2(T) = *tree.nodes[i_c];

    balance := c.height - b.height;

	// Rotate C up
	if balance > 1 {
		i_f := c.child1;
		i_g := c.child2;
        f: *Node2(T) = *tree.nodes[i_f];
        g: *Node2(T) = *tree.nodes[i_g];
		assert(0 <= i_f && i_f < tree.node_capacity);
		assert(0 <= i_g && i_g < tree.node_capacity);

		// Swap A and C
        c.child1 = i_a;
		c.parent_index = a.parent_index;
		a.parent_index = i_c;

		// A's old parent should point to C
		if c.parent_index != NULL_NODE {
			if tree.nodes[c.parent_index].child1 == i_a {
				tree.nodes[c.parent_index].child1 = i_c;
			} else {
				assert(tree.nodes[c.parent_index].child2 == i_a);
				tree.nodes[c.parent_index].child2 = i_c;
			}
		} else {
			tree.root_index = i_c;
		}

		// Rotate
		if f.height > g.height {
			c.child2 = i_f;
			a.child2 = i_g;
			g.parent_index = i_a;
			a.aabb = merge(b.aabb, g.aabb);
			c.aabb = merge(a.aabb, f.aabb);

			a.height = 1 + max(b.height, g.height);
			c.height = 1 + max(a.height, f.height);
		} else {
			c.child2 = i_g;
			a.child2 = i_f;
			f.parent_index = i_a;
			a.aabb = merge(b.aabb, f.aabb);
			c.aabb = merge(a.aabb, g.aabb);

			a.height = 1 + max(b.height, f.height);
			c.height = 1 + max(a.height, g.height);
		}

		return i_c;
	}
	
	// Rotate B up
	if balance < -1 {
		i_d := b.child1;
		i_e := b.child2;
        d: *Node2(T) = *tree.nodes[i_d];
        e: *Node2(T) = *tree.nodes[i_e];
		assert(0 <= i_d && i_d < tree.node_capacity);
		assert(0 <= i_e && i_e < tree.node_capacity);

		// Swap A and B
		b.child1 = i_a;
		b.parent_index = a.parent_index;
		a.parent_index = i_b;

		// A's old parent should point to B
		if b.parent_index != NULL_NODE {
			if tree.nodes[b.parent_index].child1 == i_a {
				tree.nodes[b.parent_index].child1 = i_b;
			} else {
				assert(tree.nodes[b.parent_index].child2 == i_a);
				tree.nodes[b.parent_index].child2 = i_b;
			}
		} else {
			tree.root_index = i_b;
		}

		// Rotate
		if d.height > e.height {
			b.child2 = i_d;
			a.child1 = i_e;
			e.parent_index = i_a;
			a.aabb = merge(c.aabb, e.aabb);
			b.aabb = merge(a.aabb, d.aabb);

			a.height = 1 + max(c.height, e.height);
			b.height = 1 + max(a.height, d.height);
		} else {
			b.child2 = i_e;
			a.child1 = i_d;
			d.parent_index = i_a;
			a.aabb = merge(c.aabb, d.aabb);
			b.aabb = merge(a.aabb, e.aabb);

			a.height = 1 + max(c.height, d.height);
			b.height = 1 + max(a.height, e.height);
		}

		return i_b;
	}

	return i_a;
}

compute_height :: (tree: BoundingTree2) -> s32 {
	return compute_height(tree, tree.root_index);
}

compute_height :: (tree: BoundingTree2($T), node_id: s32) -> s32 {
	assert(0 <= node_id && node_id < tree.node_capacity);
    node: *Node2(T) = *tree.nodes[node_id];

	if is_leaf(<< node) {
		return 0;
	}

	height1 := compute_height(tree, node.child1);
	height2 := compute_height(tree, node.child2);
	return 1 + max(height1, height2);
}

validate_structure :: (tree: BoundingTree2($T), index: s32) {
	if index == NULL_NODE {
		return;
	}

	if index == tree.root_index {
		assert(tree.nodes[index].parent_index == NULL_NODE);
	}

    // once again, cost ref would be nice
    node: *Node2(T) = *tree.nodes[index];

	child1 := node.child1;
	child2 := node.child2;

	if is_leaf(<< node) {
		assert(child1 == NULL_NODE);
		assert(child2 == NULL_NODE);
		assert(node.height == 0);
		return;
	}

	assert(0 <= child1 && child1 < tree.node_capacity);
	assert(0 <= child2 && child2 < tree.node_capacity);

	assert(tree.nodes[child1].parent_index == index);
	assert(tree.nodes[child2].parent_index == index);

	validate_structure(tree, child1);
	validate_structure(tree, child2);
}

validate_metrics :: (tree: BoundingTree2($T), index: s32) {
	if index == NULL_NODE {
		return;
	}

    // it'd be nice if this could be a const ref or something
    node: *Node2(T) = *tree.nodes[index];

	child1 := node.child1;
	child2 := node.child2;

	if is_leaf(<< node) {
		assert(child1 == NULL_NODE);
		assert(child2 == NULL_NODE);
		assert(node.height == 0);
		return;
	}

	assert(0 <= child1 && child1 < tree.node_capacity);
	assert(0 <= child2 && child2 < tree.node_capacity);

	height1 := tree.nodes[child1].height;
	height2 := tree.nodes[child2].height;
	height := 1 + max(height1, height2);
	assert(node.height == height);

	aabb := merge(tree.nodes[child1].aabb, tree.nodes[child2].aabb);

	assert(aabb.lower == node.aabb.lower);
	assert(aabb.upper == node.aabb.upper);

	validate_metrics(tree, child1);
	validate_metrics(tree, child2);
}

NULL_NODE: s32 : -1;
EPSILON :: 0.000001;
// the padding of an aabb to avoid needless updates every time the object moves
AABB_EXTENSION :: 0.1;
AABB_MULTIPLIER :: 4.0;
TREE_DEBUG :: true;

#import "Math";
#import "Basic";
#import "JaiMath";