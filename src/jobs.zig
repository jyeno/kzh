const std = @import("std");
const os = std.os;
const ast = @import("ast.zig");
const Command = ast.Command;
const builtins = @import("builtins.zig").builtins;
const executor = @import("exec.zig");
const symtab = @import("symtab.zig");
const kzhAlias = @import("builtins/alias.zig").kzhAlias;
const exec = @import("exec.zig");

// initialized by initGlobalController
pub var global_controller: JobController = undefined;

const Aliases = symtab.SymTab([][]const u8);

pub const JobController = struct {
    allocator: *std.mem.Allocator,
    aliases: Aliases, // TODO currently broken SymTab needs fixes
    jobs: std.ArrayListUnmanaged(Job) = .{},
    last_status: u8 = 0,

    pub fn init(allocator: *std.mem.Allocator) JobController {
        const aliases = Aliases.init(allocator, null);
        return .{ .allocator = allocator, .aliases = aliases };
    }

    pub fn deinit(self: *JobController) void {
        self.aliases.deinit();
        self.jobs.deinit(self.allocator);
    }

    pub fn run(self: *JobController, program: *ast.Program) !void {
        for (program.body) |cmd_list| {
            try self.runAndOrCmd(cmd_list.and_or_cmd_list);
        }
    }

    fn runAndOrCmd(self: *JobController, and_or_cmd: ast.AndOrCmdList) anyerror!void {
        switch (and_or_cmd.kind) {
            .PIPELINE => try self.runPipeline(and_or_cmd.cast(.PIPELINE).?),
            .BINARY_OP => {
                const binary_op = and_or_cmd.cast(.BINARY_OP).?;
                try self.runAndOrCmd(binary_op.left);
                if ((binary_op.kind == .AND and self.last_status == 0) or
                    (binary_op.kind == .OR and self.last_status != 0))
                {
                    try self.runAndOrCmd(binary_op.right);
                }
            },
        }
    }

    fn runPipeline(self: *JobController, pipeline: *ast.Pipeline) !void {
        if (pipeline.commands.len == 1) {
            try self.runCommand(pipeline.commands[0]);
        } else {
            // TODO implement
            unreachable;
        }
        if (pipeline.has_bang) {
            if (self.last_status != 0) {
                self.last_status = 0;
            } else {
                self.last_status = 1;
            }
        }
    }

    // TODO consider merging of jobs.zig and exec.zig
    fn runCommand(self: *JobController, cmd: Command) !void {
        // maybe a run function at the command interface?
        switch (cmd.kind) {
            .SIMPLE_COMMAND => self.last_status = try exec.simpleCommand(cmd.cast(.SIMPLE_COMMAND).?),
            else => unreachable,
        }
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
