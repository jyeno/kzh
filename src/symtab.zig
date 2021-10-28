const std = @import("std");
// TODO consider if should have in the same table functions, aliases, strings and arrays

// TODO consider if should be a pointer
pub const ValType = union(enum) {
    // should be the owner of memory
    str: []const u8,
    // TODO parser word array
    array: [][]const u8,
    // TODO function
};

/// Symbol Table multi-level representation, internally it uses a hash map.
pub const SymTab = struct {
    arena: std.heap.ArenaAllocator,
    parent: ?*SymTab = null,
    table: HashMap = .{},

    pub const HashMap = std.StringHashMapUnmanaged(ValType);
    pub const Iterator = HashMap.Iterator;

    /// Initializes a symbol table
    pub fn init(allocator: *std.mem.Allocator, parent: ?*SymTab) SymTab {
        return SymTab{ .arena = std.heap.ArenaAllocator.init(allocator), .parent = parent };
    }

    pub fn initCapacity(allocator: *std.mem.Allocator, parent: ?*SymTab, capacity: u32) !SymTab {
        var arena_allocator = std.heap.ArenaAllocator.init(allocator);
        var table: HashMap = .{};
        try table.ensureTotalCapacity(&arena_allocator.allocator, capacity);
        return SymTab{ .arena = arena_allocator, .parent = parent, .table = table };
    }

    /// Deinitializes the symbol table and its members
    pub fn deinit(self: *SymTab) void {
        self.arena.deinit();
    }

    /// Merges the symbol table with its parent
    pub fn mergeWithParent(self: *SymTab) !void {
        if (self.parent) |parent| {
            parent.table.ensureUnusedCapacity(parent.allocator, self.table.capacity);
            var it = self.table.iterator();
            while (it.next()) |item| {
                parent.putAssumeCapacityCopyVal(item.key_ptr, item.value_ptr.*);
            }
        }
    }

    /// Do a local search based on given key, returns the value of
    /// given key or null in failure
    pub fn lookup(self: *SymTab, key: []const u8) ?ValType {
        return self.table.get(key);
    }

    /// Do a global search based on given key, first tries local search,
    /// if there is no match, search on the top-level symbol tables
    /// return the value of given key or null in failure.
    pub fn globalLookup(self: *SymTab, key: []const u8) ?ValType {
        if (self.lookup(key)) |value| {
            return value;
        } else if (self.parent) |parent| {
            return parent.globalLookup(key);
        } else {
            return null;
        }
    }

    /// Inserts `value` with given `key`, if there is already an entry, it is overwriten
    pub fn put(self: *SymTab, key: []const u8, value: ValType) !void {
        try self.table.put(&self.arena.allocator, key, value);
    }

    /// Inserts `value` with given `key`, if there is already an entry, it is overwriten
    /// Copies the data of `value`
    pub fn putCopyVal(self: *SymTab, key: []const u8, value: ValType) !void {
        const new_value = .{ .str = try std.mem.dupe(&self.arena.allocator, u8, value.str) };
        try self.table.put(&self.arena.allocator, key, new_value);
    }

    /// Inserts `value` with given `key`, if there is already an entry, it is overwriten
    pub fn putAssumeCapacity(self: *SymTab, key: []const u8, value: ValType) void {
        self.table.putAssumeCapacity(key, value);
    }

    /// Inserts `value` with given `key`, if there is already an entry, it is overwriten
    /// Copies the data of `value`
    pub fn putAssumeCapacityCopyVal(self: *SymTab, key: []const u8, value: ValType) !void {
        // TODO maybe a replace function, and it would require no allocation inside, so safe
        const new_value = .{ .str = try std.mem.dupe(&self.arena.allocator, u8, value.str) };
        self.table.putAssumeCapacity(self.allocator, key, new_value);
    }

    /// Remove entry with given `key`, and returns it if exists.
    pub fn pop(self: *SymTab, key: []const u8) ValType {
        return self.table.remove(key);
    }

    /// Remove entry with given `key`, and also deallocate the memory allocated
    pub fn del(self: *SymTab, key: []const u8) void {
        const value = self.table.remove(key);
        self.arena.allocator.free(value.?.str);
    }

    /// Returns the table iterator.
    pub fn iterator(self: *SymTab) Iterator {
        return self.table.iterator();
    }

    /// Creates an array of null terminated strings, copying the entries of the symbol table.
    /// Its strings are formated as:
    ///          "KEY_STRING=VALUE"
    /// Where KEY_STRING is the key of the entry and VALUE is the value of the entry.
    // TODO analize a way to optimize it
    pub fn dupeZ(self: *SymTab, allocator: *std.mem.Allocator) ![*:null]?[*:0]const u8 {
        var array = try std.ArrayList(?[*:0]const u8).initCapacity(allocator, self.table.count() + 1);
        defer array.deinit();
        // TODO analize possibility of get parent items too

        var it = self.table.iterator();
        while (it.next()) |entry| {
            const env = try std.mem.joinZ(allocator, "=", &[_][]const u8{ entry.key_ptr.*, entry.value_ptr.str });
            array.appendAssumeCapacity(env);
        }
        return try array.toOwnedSliceSentinel(null);
    }
};

// this variable is populated at the main
pub var global_symtab: SymTab = undefined;

/// Initializes the global symbol table, with the contents std.os.environ
pub fn initGlobalSymbolTable(allocator: *std.mem.Allocator) !void {
    global_symtab = try SymTab.initCapacity(allocator, null, @intCast(u32, std.os.environ.len));
    for (std.os.environ) |env| {
        const equalsIdx: ?usize = blk: {
            var index: usize = 0;
            while (env[index] != '=' and env[index] != 0) : (index += 1) {}
            // it cant be on the beginning and end of the env, if it is invalid therefore return null
            break :blk if (index != 0 and env[index] != 0) index else null;
        };
        if (equalsIdx) |index| {
            const value = blk: {
                var end = index + 1;
                while (env[end] != 0) : (end += 1) {}
                break :blk .{ .str = env[index + 1 .. end] };
            };
            global_symtab.putAssumeCapacity(env[0..index], value);
        }
    }
}

test "Symbol Table" {}
