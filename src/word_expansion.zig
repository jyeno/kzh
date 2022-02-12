const std = @import("std");
const os = std.os;
const ast = @import("ast.zig");
const jobs = @import("jobs.zig");
const Word = ast.Word;

const WordResult = union(enum) {
    empty: void,
    single_str: []const u8,
    multiple_str: []const []const u8,
};

pub fn expandWord(ctl: *jobs.JobController, word_arg: ast.Word, fields: *std.ArrayList([]const u8)) !u32 {
    // TODO evalTilde
    // TODO split fields
    // TODO expand pathnames
    // try splitFields(fields, word_ref);
    var result: u32 = 0;
    switch (try execWord(ctl, word_arg, &result)) {
        .multiple_str => |slice| {
            defer ctl.allocator.free(slice);
            std.debug.print("added slice: {s}\n", .{slice});
            try fields.appendSlice(slice);
        },
        .single_str => |str| try fields.append(str),
        else => unreachable,
    }
    return result;
}

fn execWord(ctl: *jobs.JobController, word_arg: ast.Word, result: *u32) anyerror!WordResult {
    return switch (word_arg.kind) {
        .STRING => WordResult{ .single_str = word_arg.cast(.STRING).?.str },
        .LIST => list: {
            // if double quoted, then merge all of the content into one []const u8
            // if not, then add all of them separatedly
            const word_list = word_arg.cast(.LIST).?;
            var word_array = std.ArrayList([]const u8).init(ctl.allocator);
            defer word_array.deinit();

            for (word_list.items) |word_item| {
                switch (try execWord(ctl, word_item, result)) {
                    .multiple_str => |slice| {
                        defer ctl.allocator.free(slice);
                        try word_array.appendSlice(slice);
                    },
                    .single_str => |str| try word_array.append(str),
                    else => unreachable, // TODO implement allocating empty string only when is_double_quoted
                }
            }

            if (word_list.is_double_quoted) {
                var intern_char_array = std.ArrayList(u8).init(ctl.allocator);
                defer intern_char_array.deinit();
                for (word_array.items) |str| {
                    try intern_char_array.ensureUnusedCapacity(str.len);
                    for (str) |char| intern_char_array.appendAssumeCapacity(char);
                }
                break :list WordResult{ .single_str = intern_char_array.toOwnedSlice() };
            } else {
                break :list WordResult{ .multiple_str = word_array.toOwnedSlice() };
            }
        },
        .PARAMETER => unreachable,
        .COMMAND => cmd: {
            const word_cmd = word_arg.cast(.COMMAND).?;
            const fds = try os.pipe();
            errdefer {
                os.close(fds[0]);
                os.close(fds[1]);
            }
            const pid = try os.fork();
            if (pid == 0) {
                os.close(fds[0]);
                if (fds[1] != os.STDOUT_FILENO) {
                    try os.dup2(fds[1], os.STDOUT_FILENO);
                    os.close(fds[1]);
                }
                // TODO implement traps
                if (word_cmd.program) |prog| {
                    const progResult = try ctl.run(prog);
                    os.exit(@intCast(u8, progResult));
                }
                os.exit(0);
            }
            os.close(fds[1]);

            var output_handle = std.fs.File{ .handle = fds[0] };
            // TODO improve it
            var data = try output_handle.reader().readUntilDelimiterOrEofAlloc(ctl.allocator, 0, 4096);
            if (data) |buffer| {
                result.* = os.waitpid(pid, 0).status;
                // trim newlines at end
                var end_size: usize = buffer.len;
                while (buffer[end_size - 1] == '\n') : (end_size -= 1) {}
                if (end_size < buffer.len) {
                    ctl.allocator.free(buffer[end_size..buffer.len]);
                }

                break :cmd WordResult{ .single_str = buffer[0..end_size] };
            } else {
                break :cmd .empty;
            }
        },
        .ARITHMETIC => unreachable,
    };
}
