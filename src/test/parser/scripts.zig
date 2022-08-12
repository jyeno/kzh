const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const Parser = @import("../../Parser.zig");
const ast = @import("../../ast.zig");

test "Parse test script" {
    // TODO change 'SERVICE' and 'TYPE' to double quotes
    // last line should be -ts'  '
    // improve legibility
    const script =
        \\s6-rc-db list all | {
        \\  printf '%s\t%s\n' 'SERVICE' 'TYPE'
        \\  while read -r srv; do
        \\    echo "$srv  $(s6-rc-db type "$srv")"
        \\  done
        \\} | column -ts '  '
    ;

    var parser = Parser.init(testing.allocator, script);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);

    const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
    try testing.expect(pipeline.commands.len == 3);
    try testing.expect(pipeline.commands[0].kind == .SIMPLE_COMMAND);
    try testing.expect(pipeline.commands[1].kind == .CMD_GROUP);
    try testing.expect(pipeline.commands[2].kind == .SIMPLE_COMMAND);

    {
        const cmd = pipeline.commands[0].cast(.SIMPLE_COMMAND).?;
        const cmd_args = cmd.args.?;
        try testing.expect(mem.eql(u8, cmd.name.?.cast(.STRING).?.str, "s6-rc-db"));
        try testing.expect(mem.eql(u8, cmd_args[0].cast(.STRING).?.str, "list"));
        try testing.expect(mem.eql(u8, cmd_args[1].cast(.STRING).?.str, "all"));
    }
    {
        const cmd = pipeline.commands[1].cast(.CMD_GROUP).?;
        try testing.expect(cmd.kind == .BRACE_GROUP);
        try testing.expect(cmd.body.len == 2); // TODO fix recognizing only one
        try testing.expect(cmd.body[0].and_or_cmd_list.kind == .PIPELINE);
        const cmd_group_pipeline = cmd.body[0].and_or_cmd_list.cast(.PIPELINE).?;
        const pipeline_cmd = cmd_group_pipeline.commands[0].cast(.SIMPLE_COMMAND).?;
        const pipeline_cmd_args = pipeline_cmd.args.?;
        try testing.expect(mem.eql(u8, pipeline_cmd.name.?.cast(.STRING).?.str, "printf"));
        // try testing.expect(mem.eql(u8, pipeline_cmd_args[1].cast(.LIST).?.str, "SERVICE")); TODO proper way to get the string contents
        try testing.expect(mem.eql(u8, pipeline_cmd_args[2].cast(.STRING).?.str, "TYPE"));

        try testing.expect(cmd.body[1].and_or_cmd_list.cast(.PIPELINE).?.commands[0].kind == .LOOP_DECL);
        const pipeline_while = cmd.body[1].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.LOOP_DECL).?;
        try testing.expect(pipeline_while.condition.len == 1);
        try testing.expect(pipeline_while.body.len == 1);
        try testing.expect(pipeline_while.condition[0].and_or_cmd_list.cast(.PIPELINE).?.commands.len == 1);
        try testing.expect(pipeline_while.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands.len == 1);

        const while_condition_cmd = pipeline_while.condition[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
        try testing.expect(mem.eql(u8, while_condition_cmd.name.?.cast(.STRING).?.str, "read"));
        try testing.expect(mem.eql(u8, while_condition_cmd.args.?[0].cast(.STRING).?.str, "-r"));
        try testing.expect(mem.eql(u8, while_condition_cmd.args.?[1].cast(.STRING).?.str, "srv"));

        const while_body_cmd = pipeline_while.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
        try testing.expect(mem.eql(u8, while_body_cmd.name.?.cast(.STRING).?.str, "echo"));
        try testing.expect(while_body_cmd.args.?.len == 1);
    }
    {
        const cmd = pipeline.commands[2].cast(.SIMPLE_COMMAND).?;
        const cmd_args = cmd.args.?;
        try testing.expect(mem.eql(u8, cmd.name.?.cast(.STRING).?.str, "column"));
        try testing.expect(mem.eql(u8, cmd_args[0].cast(.STRING).?.str, "-ts"));
        try testing.expect(mem.eql(u8, cmd_args[1].cast(.STRING).?.str, "  "));
    }
}
