const std = @import("std");

pub const ValEnum = enum {
    str,
};

pub const ValType = union(ValEnum) {
    str: []const u8,
};

/// Symbol Table multi-level representation, internally it uses a hash map.
pub const SymTab = struct {
    const Self = @This();

    allocator: *std.mem.Allocator,
    parent: ?*Self,
    table: HashMap,

    pub const HashMap = std.StringHashMapUnmanaged(*ValType);
    pub const Iterator = HashMap.Iterator;

    /// Initializes a symbol table
    pub fn init(allocator: *std.mem.Allocator, parent: ?*Self) Self {
        return Self{ .allocator = allocator, .parent = parent, .table = std.StringHashMapUnmanaged(*ValType){} };
    }

    // TODO maybe init with capacity
    // TODO check out if using arena is better

    /// Deinitializes the symbol table and its members
    pub fn deinit(self: *Self) void {
        // deinitializing keys
        var it = self.table.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*.str);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.table.deinit(self.allocator);
        self.allocator.destroy(self);
        // TODO fix, see how to deallocate memory
    }

    /// Merges the symbol table with its parent
    pub fn mergeWithParent(self: *Self) !void {
        // maybe error if someone tries to merge and has no parent
        // TODO see user case
        if (self.parent) |parent| {
            var it = self.table.iterator();
            while (it.next()) |item| {
                try parent.insert(item.key_ptr, item.value_ptr.*);
            }
        }
    }

    /// Do a local search based on given key, returns the value of
    /// given key or null in failure
    pub fn local_lookup(self: *Self, key: []const u8) ?*ValType {
        return self.table.get(key);
    }

    /// Do a global search based on given key, first tries local search,
    /// if there is no match, search on the top-level symbol tables
    /// return the value of given key or null in failure.
    pub fn global_lookup(self: *Self, key: []const u8) ?*ValType {
        if (self.local_lookup(key)) |value| {
            return value;
        } else if (self.parent) |parent| {
            return parent.global_lookup(key);
        } else {
            return null;
        }
    }

    /// Inserts `value` with given `key`, if there is already an entry, it is overwrited
    pub fn insert(self: *Self, key: []const u8, value: *ValType) !void {
        // TODO maybe a replace function, and it would require no allocation inside, so safe
        try self.table.put(self.allocator, try std.mem.dupe(self.allocator, u8, key), value);
    }

    /// Remove entry with given `key`, if it exists.
    pub fn remove(self: *Self, key: []const u8) void {
        _ = self.table.remove(key);
    }

    /// Returns the table iterator.
    pub fn iterator(self: *Self) Iterator {
        return self.table.iterator();
    }

    // Creates an array of null terminated strings, copying the entries of the symbol table.
    // Its strings are formated as:
    //          "KEY_STRING=VALUE"
    // Where KEY_STRING is the key of the entry and VALUE is the value of the entry.
    pub fn dupeZ(self: *Self, allocator: *std.mem.Allocator) ![*:null]?[*:0]const u8 {
        var array = try std.ArrayList(?[*:0]const u8).initCapacity(allocator, self.table.count() + 1);
        defer array.deinit();
        errdefer array.deinit();

        var it = self.table.iterator();
        while (it.next()) |entry| {
            const env = try std.mem.joinZ(allocator, "=", &[_][]const u8{ entry.key_ptr.*, entry.value_ptr.*.str });
            array.appendAssumeCapacity(env);
        }
        // add null termination
        array.appendAssumeCapacity(null);
        return @ptrCast([*:null]?[*:0]const u8, array.toOwnedSlice());
    }
};

var global_sym_tab: ?*SymTab = null;

/// Returns the global symbol table, **does not** initialize the global symbol table,
/// see `initGlobalSymbolTable` for it
pub fn globalSymbolTable() *SymTab {
    return global_sym_tab.?;
}

/// Initializes the global symbol table, with the contents std.os.environ
pub fn initGlobalSymbolTable(allocator: *std.mem.Allocator) !void {
    // under analisis, if it is the best way, disabled by now
    if (false and global_sym_tab == null) {
        global_sym_tab = try allocator.create(SymTab);
        global_sym_tab.?.* = SymTab.init(allocator, null);
        const environ = std.os.environ;
        for (environ) |env| {
            const equalsIdx: ?usize = blk: {
                var index: usize = 0;
                while (env[index] != '=' and env[index] != 0) : (index += 1) {}
                // it cant be on the beginning and end of the env, if it is invalid therefore return null
                break :blk if (index != 0 and env[index] != 0) index else null;
            };
            // TODO change variable name index to something else
            if (equalsIdx) |index| {
                var val = try allocator.create(ValType);
                val.* = blk: {
                    var end = index + 1;
                    while (env[end] != 0) : (end += 1) {}
                    break :blk ValType{ .str = try std.mem.dupe(allocator, u8, env[index + 1 .. end]) };
                };
                try global_sym_tab.?.insert(env[0..index], val);
            }
        }
    }
}

test "Symbol Table" {}
