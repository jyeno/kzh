const std = @import("std");
const os = std.os;
const ast = @import("ast.zig");
const Command = ast.Command;
const builtins = @import("builtins.zig").builtins;
const executor = @import("exec.zig");
const symtab = @import("symtab.zig");
const kzhAlias = @import("builtins/alias.zig").kzhAlias;
const exec = @import("exec.zig");

// TODO have a context struct for data that needs to be checked/used
// every run (like functions, envvars and such)
//
// context have functions, the function name should be the key to the function, meaning that it doesnt need
// to be saved on the funcdecl struct, only saved on the context struct

// initialized by initGlobalController
pub var global_controller: JobController = undefined;

const Aliases = symtab.SymTab([][]const u8);

pub const JobController = struct {
    allocator: *std.mem.Allocator,
    aliases: Aliases, // TODO currently broken SymTab needs fixes
    jobs: std.ArrayListUnmanaged(Job) = .{},

    pub fn init(allocator: *std.mem.Allocator) JobController {
        const aliases = Aliases.init(allocator, null);
        return .{ .allocator = allocator, .aliases = aliases };
    }

    pub fn deinit(self: *JobController) void {
        self.aliases.deinit();
        self.jobs.deinit(self.allocator);
    }

    pub fn run(self: *JobController, program: *ast.Program) !u8 {
        return exec.program(self.allocator, program);
    }
    fn createJob(self: *JobController, and_or_cmd: *ast.AndOrCmdList) !void {
        try self.jobs.append(self.allocator, .{ .controller = self, .and_or_cmd = and_or_cmd });
    }
};

const Job = struct {
    controller: *JobController,
    and_or_cmd: ast.AndOrCmdList,
    is_async: bool = true,
    status: u8 = 0, // TODO see correct status, maybe signal?
    pid: os.pid_t = undefined,
};

pub fn initGlobalJobController(allocator: *std.mem.Allocator) !void {
    global_controller = JobController.init(allocator);

    _ = kzhAlias(&.{
        "alias",
        "autoload='typeset -fu'",
        "functions='typeset -f'",
        "hash='alias -t'",
        "history='fc -l'",
        "integer='typeset -i'",
        "local='typeset'",
        "login='exec login'",
        "nohup='nohup '",
        "r='fc -s'",
        "stop='kill -STOP'",
    });
}
