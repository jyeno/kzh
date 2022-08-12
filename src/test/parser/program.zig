const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const Parser = @import("../../Parser.zig");
const ast = @import("../../ast.zig");

test "Parse Command List Separator" {
    const command_string = "echo oi\nprint test; builtin pwd &";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);

    const cmd_array = program.body;
    try testing.expect(cmd_array.len == 3);

    // TODO verify name of each command
    try testing.expect(cmd_array[0].is_async == false);
    const simple_command1 = cmd_array[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(mem.eql(u8, simple_command1.name.?.cast(.STRING).?.str, "echo"));

    try testing.expect(cmd_array[1].is_async == false);
    const simple_command2 = cmd_array[1].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(mem.eql(u8, simple_command2.name.?.cast(.STRING).?.str, "print"));

    try testing.expect(cmd_array[2].is_async == true);
    const simple_command3 = cmd_array[2].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(mem.eql(u8, simple_command3.name.?.cast(.STRING).?.str, "builtin"));
}

test "Parse And Or Cmd List" {
    const command_string = "pgrep kzh && echo kzh is running || echo kzh is not running";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);

    try testing.expect(program.body.len == 1);

    const cmd_list = program.body[0];
    try testing.expect(cmd_list.is_async == false);

    const binary_op_left = cmd_list.and_or_cmd_list.cast(.BINARY_OP);
    try testing.expect(binary_op_left != null);
    try testing.expect(binary_op_left.?.kind == .AND);
    try testing.expect(binary_op_left.?.left.cast(.PIPELINE) != null);

    const binary_op_right = binary_op_left.?.right.cast(.BINARY_OP);
    try testing.expect(binary_op_right != null);
    try testing.expect(binary_op_right.?.kind == .OR);

    try testing.expect(binary_op_right.?.left.cast(.PIPELINE) != null);
    try testing.expect(binary_op_right.?.right.cast(.PIPELINE) != null);
}

test "Parse Pipeline" {
    const command_string = "head somefile | grep 'Hello World' | tr '\n' ' '";

    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);

    const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
    try testing.expect(pipeline.commands.len == 3);

    const cmd1 = pipeline.commands[0].cast(.SIMPLE_COMMAND).?;
    const cmd1_args = cmd1.args.?;
    try testing.expect(mem.eql(u8, cmd1.name.?.cast(.STRING).?.str, "head"));
    try testing.expect(mem.eql(u8, cmd1_args[0].cast(.STRING).?.str, "somefile"));

    const cmd2 = pipeline.commands[1].cast(.SIMPLE_COMMAND).?;
    const cmd2_args = cmd2.args.?;
    try testing.expect(mem.eql(u8, cmd2.name.?.cast(.STRING).?.str, "grep"));
    try testing.expect(mem.eql(u8, cmd2_args[0].cast(.STRING).?.str, "Hello World"));

    const cmd3 = pipeline.commands[2].cast(.SIMPLE_COMMAND).?;
    const cmd3_args = cmd3.args.?;
    try testing.expect(mem.eql(u8, cmd3.name.?.cast(.STRING).?.str, "tr"));
    try testing.expect(mem.eql(u8, cmd3_args[0].cast(.STRING).?.str, "\n"));
    try testing.expect(mem.eql(u8, cmd3_args[1].cast(.STRING).?.str, " "));

    // TODO test pipeline Bangs
}
