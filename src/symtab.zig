const std = @import("std");

const SymTabString = SymTab([]const u8);

// this variable is populated at the main
pub var global_symtab: SymTabString = undefined;

/// Symbol Table multi-level representation, internally it uses a hash map.
pub fn SymTab(comptime T: type) type {
    return struct {
        arena: std.heap.ArenaAllocator,
        parent: ?*Self = null,
        table: HashMap = .{},

        pub const HashMap = std.StringHashMapUnmanaged(T);
        pub const Iterator = HashMap.Iterator;
        pub const Self = @This();

        /// Initializes a symbol table
        pub fn init(allocator: *std.mem.Allocator, parent: ?*Self) Self {
            return Self{ .arena = std.heap.ArenaAllocator.init(allocator), .parent = parent };
        }

        pub fn initCapacity(allocator: *std.mem.Allocator, parent: ?*Self, capacity: u32) !Self {
            var arena_allocator = std.heap.ArenaAllocator.init(allocator);
            var table: HashMap = .{};
            try table.ensureTotalCapacity(&arena_allocator.allocator, capacity);
            return Self{ .arena = arena_allocator, .parent = parent, .table = table };
        }

        /// Deinitializes the symbol table and its members
        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        /// Merges the symbol table with its parent
        pub fn mergeWithParent(self: *Self) !void {
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
        pub fn lookup(self: *Self, key: []const u8) ?T {
            return self.table.get(key);
        }

        /// Do a global search based on given key, first tries local search,
        /// if there is no match, search on the top-level symbol tables
        /// return the value of given key or null in failure.
        pub fn globalLookup(self: *Self, key: []const u8) ?T {
            if (self.lookup(key)) |value| {
                return value;
            } else if (self.parent) |parent| {
                return parent.globalLookup(key);
            } else {
                return null;
            }
        }

        /// Inserts `value` with given `key`, if there is already an entry, it is overwriten
        pub fn put(self: *Self, key: []const u8, value: T) !void {
            try self.table.put(&self.arena.allocator, key, value);
        }

        /// Inserts `value` with given `key`, if there is already an entry, it is overwriten
        /// Copies the data of `value`
        pub fn putCopyVal(self: *Self, key: []const u8, value: T) !void {
            // TODO verify type of data and act accordingly
            const new_value = try std.mem.dupe(&self.arena.allocator, @TypeOf(value[0]), value);
            try self.table.put(&self.arena.allocator, key, new_value);
        }

        /// Inserts `value` with given `key`, if there is already an entry, it is overwriten
        pub fn putAssumeCapacity(self: *Self, key: []const u8, value: T) void {
            self.table.putAssumeCapacity(key, value);
        }

        /// Inserts `value` with given `key`, if there is already an entry, it is overwriten
        /// Copies the data of `value`
        pub fn putAssumeCapacityCopyVal(self: *Self, key: []const u8, value: T) !void {
            // TODO verify type of data and act accordingly
            const new_value = try std.mem.dupe(&self.arena.allocator, @TypeOf(value[0]), value);
            self.table.putAssumeCapacity(self.allocator, key, new_value);
        }

        /// Remove entry with given `key`, and returns it if exists.
        pub fn pop(self: *Self, key: []const u8) T {
            return self.table.remove(key);
        }

        /// Remove entry with given `key`, and also deallocate the memory allocated
        pub fn del(self: *Self, key: []const u8) void {
            const value = self.table.remove(key);
            self.arena.allocator.free(value.?.str);
        }

        /// Returns the table iterator.
        pub fn iterator(self: *Self) Iterator {
            return self.table.iterator();
        }

        pub fn ensureTotalCapacity(self: *Self, capacity: u32) !void {
            try self.table.ensureTotalCapacity(&self.arena_allocator.allocator, capacity);
        }

        /// Creates an array of null terminated strings, copying the entries of the symbol table.
        /// Its strings are formated as:
        ///          "KEY_STRING=VALUE"
        /// Where KEY_STRING is the key of the entry and VALUE is the value of the entry.
        // TODO analize a way to optimize it
        pub fn dupeZ(self: *Self, allocator: *std.mem.Allocator) ![*:null]?[*:0]const u8 {
            if (T != []const u8) {
                @compileError("only []const u8 symtabs are allowed to use this function.");
            }
            var array = try std.ArrayList(?[*:0]const u8).initCapacity(allocator, self.table.count() + 1);
            defer array.deinit();
            // TODO analize possibility of get parent items too

            var it = self.table.iterator();
            while (it.next()) |entry| {
                const env = try std.mem.joinZ(allocator, "=", &[_][]const u8{ entry.key_ptr.*, entry.value_ptr.* });
                array.appendAssumeCapacity(env);
            }
            return try array.toOwnedSliceSentinel(null);
        }
    };
}

/// Initializes the global symbol table, with the contents std.os.environ
pub fn initGlobalSymbolTable(allocator: *std.mem.Allocator) !void {
    global_symtab = try SymTabString.initCapacity(allocator, null, @intCast(u32, std.os.environ.len));
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
                break :blk env[index + 1 .. end];
            };
            global_symtab.putAssumeCapacity(env[0..index], value);
        }
    }
}

test "Symbol Table" {
    // TODO
}

test "Symbol Table assumeCapacity" {
    // TODO
}
