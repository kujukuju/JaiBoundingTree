
// why did I make this I'm now realizing I don't actually need it lol

LinkedArray :: struct (T: Type) {
    LinkedEntry :: struct {
        item: T;
        prev: int;
        next: int;
        allocated: bool;
    }

    entries: [..] LinkedEntry;
    root_index: int = -1;
    recent_index: int = -1;
    free_index: int = -1;
    last_free_index: int = -1;

    count: int;
#place entries;
    // is this really dangerous actually?
    allocated: int = ---;
}

// im pretty sure there are some ways to somewhat optimize these function calls by making assumptions about things
// like right now the last entry in my allocated block has a next index of -1 and the first entry in my freed block
// has a prev index of -1 and neither of those are probably needed

// #run {
//     print_items :: (linked: LinkedArray($T)) {
//         for item: linked {
//             print("% ", item);
//         }
//         print("\n");
//     }
//     linked: LinkedArray(int);
//     iz := linked_add(*linked, 0);
//     linked_add(*linked, 1);
//     linked_add(*linked, 2);
//     linked_add(*linked, 3);
//     linked_pop(*linked);
//     linked_add(*linked, 4);
//     linked_add(*linked, 5);
//     linked_add(*linked, 6);

//     print_items(linked);
//     print("root % %\n", linked.root_index, iz);
//     print("0 1 2 4 5 6\n\n");

//     linked_reset(*linked);

//     ia := linked_add(*linked, 0);
//     ib := linked_add(*linked, 1);
//     ic := linked_add_front(*linked, -1);

//     print_items(linked);
//     print("root % = %\n", linked.root_index, ic);
//     print("-1 0 1\n\n");

//     linked_release(*linked, ib);

//     print_items(linked);
//     print("root % = %\n", linked.root_index, ic);
//     print("-1 0\n\n");

//     linked_release(*linked, ic);

//     print_items(linked);
//     print("root % = %\n", linked.root_index, ia);
//     print("0\n\n");

//     id := linked_add_front(*linked, -1);

//     print_items(linked);
//     print("root % = %\n", linked.root_index, id);
//     print("-1 0\n\n");

//     linked_release(*linked, ia);

//     print_items(linked);
//     print("root % = %\n", linked.root_index, id);
//     print("-1\n\n");

//     linked_release(*linked, id);

//     print_items(linked);
//     print("root % = %\n", linked.root_index, -1);
//     print("\n\n");

//     iz = linked_add(*linked, 0);
//     linked_add(*linked, 1);
//     linked_add(*linked, 2);
//     linked_add(*linked, 3);
//     linked_pop(*linked);
//     linked_add(*linked, 4);
//     linked_add(*linked, 5);
//     linked_add(*linked, 6);
//     linked_add(*linked, 0);
//     linked_add(*linked, 1);
//     linked_add(*linked, 2);
//     linked_add(*linked, 3);
//     linked_pop(*linked);
//     linked_add(*linked, 4);
//     linked_add(*linked, 5);
//     linked_add(*linked, 6);
//     linked_add(*linked, 0);
//     linked_add(*linked, 1);
//     linked_add(*linked, 2);
//     linked_add(*linked, 3);
//     linked_pop(*linked);
//     linked_add(*linked, 4);
//     linked_add(*linked, 5);
//     linked_add(*linked, 6);

//     print_items(linked);
//     print("root % %\n", linked.root_index, iz);
//     print("0 1 2 4 5 6 0 1 2 4 5 6 0 1 2 4 5 6\n\n");
// };

operator [] :: (linked: LinkedArray($T), index: int) -> T {
    assert(linked.entries[index].allocated, "Cannot access unclaimed memory.\n");
    if !linked.entries[index].allocated {
        empty: T;
        return empty;
    }
    return linked.entries[index].item;
}

operator [] :: (linked: *LinkedArray($T), index: int) -> *T {
    if !linked.entries[index].allocated {
        return null;
    }
    return *linked.entries[index].item;
}

for_expansion :: (linked: *LinkedArray($T), body: Code, flags: For_Flags) #expand {
    #if flags & .REVERSE {
        index := linked.recent_index;
    } else {
        index := linked.root_index;
    }

    if index == -1 {
        return;
    }

    // we can't use an actual for loop because remove could delete a chunk potentially
    while index >= 0 {
        assert(linked.entries[index].allocated, "Tried to iterate through an unallocated entry which shouldn't happen.\n");
        
        #if flags & .POINTER {
            `it := *linked.entries[index].item;
            `it_index := index;
        } else {
            `it := linked.entries[index].item;
            `it_index := index;
        }

        #if flags & .REVERSE {
            next_index := linked.entries[index].prev;
        } else {
            next_index := linked.entries[index].next;
        }
        defer index = next_index;

        #insert (remove={linked_release(linked, index);}) body;
    }
}

linked_push :: linked_add;

linked_pop :: (linked: *LinkedArray($T)) -> T {
    if linked.recent_index >= 0 {
        item := linked.entries[linked.recent_index].item;
        linked_release(linked, linked.recent_index);
        return item;
    } else {
        empty: T;
        return empty;
    }
}

linked_pop_front :: (linked: *LinkedArray($T)) -> T {
    if linked.root_index >= 0 {
        item := linked.entries[linked.root_index].item;
        linked_release(linked, linked.root_index);
        return item;
    } else {
        empty: T;
        return empty;
    }
}

linked_empty :: (linked: *LinkedArray) -> bool {
    return linked.count == 0;
}

linked_reset :: (linked: *LinkedArray) {
    linked.root_index = -1;
    linked.recent_index = -1;
    linked.free_index = 0;
    linked.last_free_index = linked.entries.count - 1;
    linked.count = 0;

    for i: 0..linked.entries.count - 1 {
        linked.entries[i].allocated = false;
        linked.entries[i].next = i + 1;
        linked.entries[i].prev = i - 1;
    }
    if linked.entries.count > 0 {
        linked.entries[0].prev = -1;
        linked.entries[linked.entries.count - 1].next = -1;
    }
}

linked_add_front :: (linked: *LinkedArray($T), entry: T) -> int {
    index, added := linked_add_front(linked);
    added.* = entry;
    return index;
}

linked_add_front :: (linked: *LinkedArray($T)) -> int, *T {
    linked_maybe_expand(linked);

    index := linked.free_index;

    linked.free_index = linked.entries[index].next;

    if linked.free_index == -1 {
        linked.last_free_index = -1;
    } else {
        linked.entries[linked.free_index].prev = -1;
    }

    linked.entries[index].allocated = true;

    linked.entries[index].prev = -1;
    linked.entries[index].next = linked.root_index;

    if linked.root_index >= 0 {
        linked.entries[linked.root_index].prev = index;
    }
    linked.root_index = index;

    if linked.recent_index == -1 {
        linked.recent_index = index;
    }

    linked.count += 1;

    return index, *linked.entries[index].item;
}

linked_add :: (linked: *LinkedArray($T), entry: T) -> int {
    index, item := linked_add(linked);
    item.* = entry;
    return index;
}

linked_add :: (linked: *LinkedArray($T)) -> int, *T {
    linked_maybe_expand(linked);

    index := linked.free_index;

    linked.free_index = linked.entries[index].next;

    if linked.free_index == -1 {
        linked.last_free_index = -1;
    } else {
        linked.entries[linked.free_index].prev = -1;
    }

    linked.entries[index].allocated = true;

    linked.entries[index].prev = linked.recent_index;
    linked.entries[index].next = -1;

    if linked.recent_index >= 0 {
        assert(linked.entries[linked.recent_index].next == -1, "Recent index is not really recent...\n");
        linked.entries[linked.recent_index].next = index;
    }

    linked.recent_index = index;

    if linked.root_index == -1 {
        linked.root_index = index;
    }

    linked.count += 1;

    return index, *linked.entries[index].item;
}

linked_release :: (linked: *LinkedArray($T), index: int) {
    assert(linked.entries[index].allocated, "Tried to release an unallocated entry.\n");
    if !linked.entries[index].allocated {
        return;
    }

    entry := *linked.entries[index];
    entry.allocated = false;

    if entry.prev >= 0 {
        linked.entries[entry.prev].next = entry.next;
    } else {
        assert(linked.root_index == index, "Entry has no previous index but also isn't the root index.\n");
        linked.root_index = entry.next;
    }
    if entry.next >= 0 {
        linked.entries[entry.next].prev = entry.prev;
    } else {
        assert(linked.recent_index == index, "Entry has no next index but also isn't the recent index.\n");
        linked.recent_index = entry.prev;
    }

    // free index can be -1 and that's fine
    entry.prev = -1;
    entry.next = linked.free_index;

    if linked.free_index >= 0 {
        linked.entries[linked.free_index].prev = index;
    }

    linked.free_index = index;
    if linked.last_free_index == -1 {
        linked.last_free_index = index;
    }

    linked.count -= 1;
}

linked_reserve :: (linked: *LinkedArray, required_size: int) {
    if linked.entries.count >= required_size {
        return;
    }

    // the logic I'm trying to stick to is the most recently freed entry is the next one to be allocated
    // so if I'm reserving things they should be the last to be allocated

    last_count := linked.entries.count;
    array_resize(*linked.entries, required_size);

    for i: last_count..linked.entries.count - 1 {
        linked.entries[i].prev = i - 1;
        linked.entries[i].next = i + 1;
    }
    
    linked.entries[last_count].prev = linked.last_free_index;
    linked.entries[linked.entries.count - 1].next = -1;

    if linked.free_index == -1 {
        linked.free_index = last_count;
    }
    if linked.last_free_index >= 0 {
        linked.entries[linked.last_free_index].next = last_count;
    }
    linked.last_free_index = linked.entries.count - 1;
}

#scope_file

linked_maybe_expand :: (linked: *LinkedArray) {
    if linked.free_index >= 0 {
        return;
    }

    linked_reserve(linked, max(8, linked.entries.count * 2));
}
