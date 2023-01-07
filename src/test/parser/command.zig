const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const Parser = @import("../../Parser.zig");
const ast = @import("../../ast.zig");

test "Parse Brace Group and SubShell" {
    {
        const command_string = "{ echo hi; echo something else; }";
        var parser = Parser.init(testing.allocator, command_string);
        const program = try parser.parse();
        defer parser.deinit();

        const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
        try testing.expect(pipeline.commands.len == 1);
        try testing.expect(pipeline.commands[0].kind == .CMD_GROUP);
        try testing.expect(pipeline.commands[0].cast(.CMD_GROUP).?.body.len == 2);
    }
    {
        const command_string =
            \\(
            \\  echo hi
            \\  true; print "hello world"
            \\)
        ;
        var parser = Parser.init(testing.allocator, command_string);
        const program = try parser.parse();
        defer parser.deinit();

        const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
        try testing.expect(pipeline.commands.len == 1);
        try testing.expect(pipeline.commands[0].kind == .CMD_GROUP);
        try testing.expect(pipeline.commands[0].cast(.CMD_GROUP).?.body.len == 3);
    }
}

test "Parse For Declaration" {
    // TODO
}

test "Parse Func Declaration" {
    // TODO
}

test "Parse Case Declaration" {
    // TODO
}

test "Parse Loop Declaration" {
    {
        const command_string =
            \\while true
            \\do
            \\  printf \"hello world\n\"
            \\  sleep 1
            \\done
        ;
        var parser = Parser.init(testing.allocator, command_string);
        const program = try parser.parse();
        defer parser.deinit();

        const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
        try testing.expect(pipeline.commands.len == 1);

        try testing.expect(pipeline.commands[0].kind == .LOOP_DECL);
        const loop_decl = pipeline.commands[0].cast(.LOOP_DECL).?;
        try testing.expect(loop_decl.kind == .WHILE);
        try testing.expect(loop_decl.condition.len == 1);
        try testing.expect(loop_decl.body.len == 2);
    }
    {
        const command_string =
            \\until false; do
            \\  print hello wo
            \\  print r
            \\  print l
            \\  print d
            \\  print \n
            \\  sleep 1
            \\done
        ;
        var parser = Parser.init(testing.allocator, command_string);
        const program = try parser.parse();
        defer parser.deinit();

        const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
        try testing.expect(pipeline.commands.len == 1);

        try testing.expect(pipeline.commands[0].kind == .LOOP_DECL);
        const loop_decl = pipeline.commands[0].cast(.LOOP_DECL).?;
        try testing.expect(loop_decl.kind == .UNTIL);
        try testing.expect(loop_decl.condition.len == 1);
        try testing.expect(loop_decl.body.len == 6);
    }
}

test "Parse If Declaration" {
    // TODO
}

test "Parse Simple Command" {
    const command_string = "echo hi";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer parser.deinit();

    const pipeline = program.body[0].and_or_cmd_list.cast(.PIPELINE).?;
    try testing.expect(pipeline.commands.len == 1);

    const simple_command = pipeline.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command.name != null);
    const simple_command_name = simple_command.name.?.cast(.STRING).?;
    try testing.expect(mem.eql(u8, simple_command_name.str, "echo"));

    try testing.expect(simple_command.args != null);
    const args = simple_command.args.?;
    try testing.expect(args.len == 1);
    try testing.expect(mem.eql(u8, args[0].cast(.STRING).?.str, "hi"));

    try testing.expect(simple_command.assigns == null);
    try testing.expect(simple_command.io_redirs == null);
}

test "Parse Simple Command with IO redirection" {
    const command_string1 = "ls >/dev/null 2>&1";
    var parser1 = Parser.init(testing.allocator, command_string1);
    const program1 = try parser1.parse();
    defer parser1.deinit();

    const simple_command1 = program1.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command1.name != null);
    try testing.expect(simple_command1.args == null);
    try testing.expect(simple_command1.assigns == null);

    try testing.expect(simple_command1.io_redirs != null);
    try testing.expect(simple_command1.io_redirs.?.len == 2);

    const io_redir1 = simple_command1.io_redirs.?[0];
    try testing.expect(io_redir1.io_num == null);
    try testing.expect(io_redir1.here_doc == null);
    try testing.expect(io_redir1.op == .IO_GREAT);
    try testing.expect(mem.eql(u8, io_redir1.name.cast(.STRING).?.str, "/dev/null"));

    const io_redir2 = simple_command1.io_redirs.?[1];
    try testing.expect(io_redir2.io_num != null);
    try testing.expect(io_redir2.io_num.? == 2);
    try testing.expect(io_redir2.here_doc == null);
    try testing.expect(io_redir2.op == .IO_GREAT_AND);
    try testing.expect(mem.eql(u8, io_redir2.name.cast(.STRING).?.str, "1"));
}

test "Parse Simple Command Assignments" {
    const command_string1 = "some=thing else=where ls";
    var parser1 = Parser.init(testing.allocator, command_string1);
    const program1 = try parser1.parse();
    defer parser1.deinit();

    const simple_command1 = program1.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command1.name != null);
    try testing.expect(simple_command1.args == null);
    try testing.expect(simple_command1.io_redirs == null);

    try testing.expect(simple_command1.assigns != null);
    const assigns1 = simple_command1.assigns.?;
    try testing.expect(assigns1.len == 2);
    try testing.expect(mem.eql(u8, assigns1[0].name, "some"));
    try testing.expect(assigns1[0].value != null);
    try testing.expect(mem.eql(u8, assigns1[0].value.?.cast(.STRING).?.str, "thing"));
    try testing.expect(mem.eql(u8, assigns1[1].name, "else"));
    try testing.expect(assigns1[1].value != null);
    try testing.expect(mem.eql(u8, assigns1[1].value.?.cast(.STRING).?.str, "where"));

    const command_string2 = "only=envvar";
    var parser2 = Parser.init(testing.allocator, command_string2);
    const program2 = try parser2.parse();
    defer parser2.deinit();

    const simple_command2 = program2.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command2.name == null);
    try testing.expect(simple_command2.args == null);
    try testing.expect(simple_command2.io_redirs == null);

    try testing.expect(simple_command2.assigns != null);
    const assigns2 = simple_command2.assigns.?;
    try testing.expect(assigns2.len == 1);
    try testing.expect(mem.eql(u8, assigns2[0].name, "only"));
    try testing.expect(assigns2[0].value != null);
    try testing.expect(mem.eql(u8, assigns2[0].value.?.cast(.STRING).?.str, "envvar"));
}
