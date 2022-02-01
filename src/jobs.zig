const std = @import("std");
const mem = std.mem;
const os = std.os;
const ast = @import("ast.zig");
const Command = ast.Command;
const builtins = @import("builtins.zig").builtins;
const symtab = @import("symtab.zig");
const kzhAlias = @import("builtins/alias.zig").kzhAlias; // TODO remove this
const exec = @import("exec.zig");
const printError = std.debug.print;

// TODO change the current way, jobcontroller only gonna have the logic necessary for it to work,
// also it should decide if the program gonna be execute async-ish or not, that will be it, move
// all the remaining code to exec.zig
//
// TODO have a context struct for data that needs to be checked/used
// every run (like functions, envvars and such)

// initialized by initGlobalController
pub var global_controller: JobController = undefined;

const Aliases = symtab.SymTab([][]const u8);
const Functions = symtab.SymTab(ast.Command);
const EnvVars = symtab.SymTab([]const u8);
const ArrayVars = symtab.SymTab([]const []const u8);

pub const F = struct {
    pub const EXPORT = 1 << 0; //,	/* -a: export all */
    pub const BRACEEXPAND = 1 << 1; //,	/* enable {} globbing */
    pub const BGNICE = 1 << 2; //,	/* bgnice */
    pub const COMMAND = 1 << 3; //,	/* -c: (invocation) execute specified command */
    pub const CSHHISTORY = 1 << 4; //,	/* csh-style history enabled */
    // #ifdef EMACS
    // pub const FEMACS,		/* emacs command editing */
    // #endif
    // pub const FERREXIT,	/* -e: quit on error */
    // #ifdef EMACS
    // pub const FGMACS,		/* gmacs command editing */
    // #endif
    pub const IGNOREEOF = 1 << 5; //,	/* eof does not exit */
    // pub const TALKING = 1 << 6; //,	/* -i: interactive */
    pub const KEYWORD = 1 << 7; //,	/* -k: name=value anywhere */
    pub const LOGIN = 1 << 8; //,		/* -l: a login shell */
    pub const MARKDIRS = 1 << 9; //,	/* mark dirs with / in file name completion */
    pub const MONITOR = 1 << 10; //,	/* -m: job control monitoring */
    pub const NOCLOBBER = 1 << 11; //,	/* -C: don't overwrite existing files */
    pub const NOEXEC = 1 << 12; //,	/* -n: don't execute any commands */
    pub const NOGLOB = 1 << 13; //,	/* -f: don't do file globbing */
    pub const NOHUP = 1 << 14; //,		/* -H: don't kill running jobs when login shell exits */
    pub const NOLOG = 1 << 15; //,		/* don't save functions in history (ignored) */
    pub const NOTIFY = 1 << 16; //,	/* -b: asynchronous job completion notification */
    pub const NOUNSET = 1 << 17; //,	/* -u: using an unset var is an error */
    pub const PHYSICAL = 1 << 18; //,	/* -o physical: don't do logical cd's/pwd's */
    pub const PIPEFAIL = 1 << 19; //,	/* -o pipefail: all commands in pipeline can affect $? */
    pub const POSIX = 1 << 20; //,		/* -o posix: be posixly correct */
    pub const PRIVILEGED = 1 << 21; //,	/* -p: use suid_profile */
    pub const RESTRICTED = 1 << 22; //,	/* -r: restricted shell */
    pub const SH = 1 << 23; //,		/* -o sh: favor sh behaviour */
    pub const STDIN = 1 << 24; //,		/* -s: (invocation) parse stdin */
    pub const TRACKALL = 1 << 25; //,	/* -h: create tracked aliases for all commands */
    pub const VERBOSE = 1 << 26; //,	/* -v: echo input */
    // #ifdef VI
    pub const VI = 1 << 27; //,		/* vi command editing */
    pub const VIRAW = 1 << 28; //,		/* always read in raw mode (ignored) */
    pub const VISHOW8 = 1 << 29; //,	/* display chars with 8th bit set as is (versus M-) */
    pub const VITABCOMPLETE = 1 << 30; //,	/* enable tab as file name completion char */
    pub const VIESCCOMPLETE = 1 << 31; //,	/* enable ESC as file name completion in command mode */
    // #endif
    pub const XTRACE = 1 << 32; //,	/* -x: execution trace */
};

// TODO rename to better name
pub const JobController = struct {
    // must be the owner of the saved data
    allocator: mem.Allocator,
    env_vars: EnvVars,
    array_vars: ArrayVars,
    aliases: Aliases, // TODO currently broken SymTab needs fixes
    funcs: Functions,
    jobs: std.ArrayListUnmanaged(Job) = .{},
    current_job: u8 = 0,
    settings: u64 = 0,
    saved_fds: ?[]SavedIOFd = null,

    pub fn init(allocator: mem.Allocator) JobController {
        var aliases = Aliases.init(allocator, null);
        var funcs = Functions.init(allocator, null);
        var env_vars = EnvVars.init(allocator, null);
        var array_vars = ArrayVars.init(allocator, null);
        return .{
            .allocator = allocator,
            .aliases = aliases,
            .funcs = funcs,
            .env_vars = env_vars,
            .array_vars = array_vars,
        };
    }

    pub fn deinit(self: *JobController) void {
        defer {
            self.aliases.deinit(); // TODO properly deinits their items
            self.funcs.deinit();
            self.env_vars.deinit();
            self.array_vars.deinit();
            self.jobs.deinit(self.allocator);
        }
        var i: usize = 0;
        while (i < self.jobs.items.len) : (i += 1) {
            self.jobs.items[i].deinit();
        }
    }

    pub fn lookupVar(self: *JobController, name: []const u8) ?[]const u8 {
        return self.env_vars.lookup(name);
    }

    pub fn putVar(self: *JobController, name: []const u8, value: []const u8) !void {
        try self.env_vars.put(name, value);
    }

    pub fn getAlias(self: *JobController, name: []const u8) ?[]const u32 {
        return self.aliases.lookup(name);
    }

    pub fn putAlias(self: *JobController, name: []const u8, value: []const []const u32) !void {
        try self.aliases.put(name, value);
    }

    pub fn getFunc(self: *JobController, name: []const u8) ?ast.Command {
        return self.funcs.lookup(name);
    }

    pub fn putFunc(self: *JobController, name: []const u8, value: ast.Command) !void {
        try self.funcs.put(name, value);
    }

    pub fn run(self: *JobController, program: *ast.Program) !u32 {
        return exec.program(self, program);
    }

    pub fn createProcess(self: *JobController, pid: os.pid_t) !void {
        const proc = Process.init(pid);
        try self.jobs.get(self.current_job).addProcess(proc);
    }

    pub fn appendJob(self: *JobController, job: Job) !void {
        try self.jobs.append(self.allocator, job);
    }

    pub fn restoreFds(self: *JobController) void {
        if (self.saved_fds) |fds| {
            for (fds) |saved_fd| {
                saved_fd.restoreOldFd();
            }
            self.allocator.free(fds);
            self.saved_fds = null;
        }
    }
};

pub const Job = struct {
    controller: *JobController,
    processes: std.ArrayListUnmanaged(Process) = .{},
    last_status: u32 = 0,
    pgid: ?os.gid_t = null,

    pub fn init(controller: *JobController) Job {
        return .{ .controller = controller };
    }

    pub fn initCapacity(controller: *JobController, len: usize) !Job {
        return Job{
            .controller = controller,
            .processes = try std.ArrayListUnmanaged(Process).initCapacity(controller.allocator, len),
        };
    }

    pub fn deinit(self: *Job) void {
        self.processes.deinit(self.controller.allocator);
        // os.kill(self.pgid, os.SIG.TERM) catch {};
    }

    pub fn addProcess(self: *Job, proc: Process) !void {
        if (self.pgid == null) {
            self.pgid = @intCast(u32, proc.pid);
        }
        // TODO change process group of proc to self.pgid
        try self.processes.append(self.controller.allocator, proc);
    }

    pub fn waitProcesses(self: *Job) u32 {
        for (self.processes.items) |proc| {
            const result = os.waitpid(proc.pid, 0);
            self.last_status = result.status;
        }
        self.processes.clearAndFree(self.controller.allocator);
        return self.last_status;
    }
};

pub const Process = struct {
    pid: os.pid_t,
    status: Status = .WAITING,
    sig: ?os.SIG = null,
    result: ?u32 = null,

    pub fn init(pid: os.pid_t) Process {
        return .{ .pid = pid };
    }
};

pub const Status = enum {
    WAITING,
    STOPPED,
    TERMINATED,
};

pub const SavedIOFd = struct {
    old_fd: os.fd_t = -1,
    current_fd: os.fd_t = -1,

    pub fn saveApplyFd(self: *SavedIOFd, alloca: std.mem.Allocator, io_redir: ast.IORedir) anyerror!void {
        _ = alloca;
        const filename = io_redir.name.cast(.STRING).?.str; // TODO use word() support other word types
        // TODO do defer here with dealloc

        self.current_fd = switch (io_redir.op) {
            .IO_LESS => try os.open(filename, os.O.CLOEXEC | os.O.RDONLY, 0),
            // .IO_DOUBLE_LESS, .IO_DOUBLE_LESS_DASH => createHereDocumentFd TODO
            .IO_GREAT, .IO_CLOBBER => try os.open(filename, os.O.WRONLY | os.system.O.CREAT | os.O.TRUNC, 0o644),
            .IO_DOUBLE_GREAT => try os.open(filename, os.O.WRONLY | os.O.CREAT | os.O.APPEND, 0o644),
            .IO_LESS_AND, .IO_GREAT_AND => std.fmt.parseInt(os.fd_t, filename, 10) catch -1,
            else => -1,
        };
        if (io_redir.io_num) |io_number| {
            self.old_fd = io_number;
        } else {
            self.old_fd = switch (io_redir.op) {
                .IO_LESS, .IO_LESS_AND, .IO_DOUBLE_LESS, .IO_DOUBLE_LESS_DASH => os.STDIN_FILENO,
                .IO_LESS_GREAT, .IO_GREAT, .IO_DOUBLE_GREAT, .IO_GREAT_AND, .IO_CLOBBER => os.STDOUT_FILENO,
            };
        }
        if (self.old_fd == self.current_fd) return error.SameFd;
        if (self.current_fd == -1) {
            printError("something wrong happened, better handling in the futureTM\n", .{});
        }
        os.dup2(self.current_fd, self.old_fd) catch |err| {
            printError("something wrong happened dup2, {}\n", .{err});
        };
    }

    pub fn restoreOldFd(self: SavedIOFd) void {
        os.dup2(self.current_fd, self.old_fd) catch |err| {
            printError("something wrong happened dup2, {}\n", .{err});
        };
    }
};
