const std = @import("std");
const builtins = @import("builtins.zig").builtins;
const ast = @import("ast.zig");
const Node = ast.Node;
const AndOrCmdList = Node.AndOrCmdList;
const AndOrCmdListKind = AndOrCmdList.AndOrCmdListKind;
const Command = Node.Command;
const Word = ast.Word;

pub fn runProgram(program: *Node.Program) !u8 {
    return try runCommandListArray(program.body);
}

fn runCommandListArray(cmd_list_array: []*Node.CommandList) !u8 {
    var ret: u8 = undefined;
    for (cmd_list_array) |cmd_list| {
        ret = try runAndOrCmdList(cmd_list.and_or_cmd_list);
    }
    return ret;
}

fn runAndOrCmdList(and_or_list: *Node.AndOrCmdList) !u8 {
    if (and_or_list.cast(AndOrCmdListKind.PIPELINE)) |pipeline| {
        return try runPipeline(pipeline);
    } else if (and_or_list.cast(AndOrCmdListKind.BINARY_OP)) |binary_op| {
        // TODO fix, make it the and_or_cmd_list
        return try runBinaryOp(binary_op);
    }
    unreachable;
}

fn runPipeline(pipeline: *AndOrCmdList.Pipeline) !u8 {
    if (pipeline.commands.len == 1) {
        // TODO improve it, whenever it has bang or not
        return try runCommand(pipeline.commands[0]);
    }
    unreachable;
}

fn runBinaryOp(binary_op: *AndOrCmdList.BinaryOp) !u8 {
    _ = binary_op;
    unreachable;
}

fn runCommand(command: *Command) !u8 {
    if (command.cast(Command.CommandKind.SIMPLE_COMMAND)) |simple_command| {
        return try runSimpleCommand(simple_command);
    }
    unreachable;
}

const BoundedArray = std.BoundedArray([]const u8, 60);

fn runSimpleCommand(simple_command: *Command.SimpleCommand) !u8 {
    if (simple_command.name) |word_name| {
        var argv: BoundedArray = undefined;
        if (simple_command.args) |args| {
            argv = try BoundedArray.init(args.len + 1);
            for (args) |arg, i| {
                if (arg.cast(Word.WordKind.STRING)) |a| {
                    argv.set(i + 1, a.str); // TODO consider others wordkinds
                }
            }
        } else {
            argv = try BoundedArray.init(1);
        }
        argv.set(0, word_name.cast(Word.WordKind.STRING).?.str);
        return try runProcess(argv.slice());
    }
    unreachable;
}

fn runProcess(argv: [][]const u8) !u8 {
    if (builtins.get(argv[0])) |builtin| {
        return builtin(argv);
    }

    return 1;
}
