const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const Parser = @import("../../Parser.zig");
const ast = @import("../../ast.zig");

test "Parse Word Arithmetic" {
    // TODO
}

test "Parse Word Command" {
    const command_string1 = "ps_mem -p -s $(pgrep kzh)";
    var parser1 = Parser.init(testing.allocator, command_string1);
    const program1 = try parser1.parse();
    defer program1.deinit(testing.allocator);

    const simple_command1 = program1.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command1.name != null);
    try testing.expect(simple_command1.name.?.kind == .STRING);
    try testing.expect(mem.eql(u8, simple_command1.name.?.cast(.STRING).?.str, "ps_mem"));
    try testing.expect(simple_command1.args != null);

    const args1 = simple_command1.args.?;
    try testing.expect(args1[0].kind == .STRING);
    try testing.expect(mem.eql(u8, args1[0].cast(.STRING).?.str, "-p"));
    try testing.expect(args1[1].kind == .STRING);
    try testing.expect(mem.eql(u8, args1[1].cast(.STRING).?.str, "-s"));
    try testing.expect(args1[2].kind == .COMMAND);
    try testing.expect(args1[2].cast(.COMMAND).?.is_back_quoted == false);

    const sub_program1 = args1[2].cast(.COMMAND).?.program;
    try testing.expect(sub_program1 != null);

    const sub_simple_cmd1 = sub_program1.?.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(sub_simple_cmd1.name != null);
    try testing.expect(sub_simple_cmd1.name.?.kind == .STRING);
    try testing.expect(mem.eql(u8, sub_simple_cmd1.name.?.cast(.STRING).?.str, "pgrep"));
    try testing.expect(sub_simple_cmd1.args != null);

    const sub_args1 = sub_simple_cmd1.args.?;
    try testing.expect(args1[0].kind == .STRING);
    try testing.expect(mem.eql(u8, sub_args1[0].cast(.STRING).?.str, "kzh"));

    const command_string2 = "ls `pwd`";
    var parser2 = Parser.init(testing.allocator, command_string2);
    const program2 = try parser2.parse();
    defer program2.deinit(testing.allocator);

    const simple_command2 = program2.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command2.name != null);
    try testing.expect(simple_command2.name.?.kind == .STRING);
    try testing.expect(mem.eql(u8, simple_command2.name.?.cast(.STRING).?.str, "ls"));
    try testing.expect(simple_command2.args != null);

    const args2 = simple_command2.args.?;
    try testing.expect(args2[0].kind == .COMMAND);
    try testing.expect(args2[0].cast(.COMMAND).?.is_back_quoted == true);

    const sub_program2 = args2[0].cast(.COMMAND).?.program;
    try testing.expect(sub_program2 != null);

    const sub_simple_cmd2 = sub_program2.?.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(sub_simple_cmd2.name != null);
    try testing.expect(sub_simple_cmd2.name.?.kind == .STRING);
    try testing.expect(mem.eql(u8, sub_simple_cmd2.name.?.cast(.STRING).?.str, "pwd"));
}

test "Parse Word Parameter" {
    const command_string = "echo $HOME ${NOTHOME:-not home} ${VAR:=PWD} ${VAR:+HOME} ${NOTHOME:?VAR} ${#HOME} ${SOMESCRIPT1#.sh} ${SOMESCRIPT22##.sh} ${SOMESCRIPT333%.sh} ${S0MESCRIPT4%%.sh}";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);

    const simple_command = program.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command.args != null);

    const args = simple_command.args.?;
    try testing.expect(args.len == 10);

    const parameter_names = [_][]const u8{ "HOME", "NOTHOME", "VAR", "VAR", "NOTHOME", "HOME", "SOMESCRIPT1", "SOMESCRIPT22", "SOMESCRIPT333", "S0MESCRIPT4" };
    var param_array: [10]*ast.WordParameter = undefined;
    for (args) |arg, index| {
        try testing.expect(arg.kind == .PARAMETER);
        const param = arg.cast(.PARAMETER).?;
        param_array[index] = param;
        try testing.expect(mem.eql(u8, param.name, parameter_names[index]));
    }

    try testing.expect(param_array[0].op == .PARAMETER_NO_OP);
    try testing.expect(param_array[0].has_colon == false);
    try testing.expect(param_array[0].arg == null);

    try testing.expect(param_array[1].op == .PARAMETER_MINUS);
    try testing.expect(param_array[1].has_colon == true);
    try testing.expect(param_array[1].arg != null);

    try testing.expect(param_array[2].op == .PARAMETER_ASSIGN);
    try testing.expect(param_array[2].has_colon == true);
    try testing.expect(param_array[2].arg != null);

    try testing.expect(param_array[3].op == .PARAMETER_PLUS);
    try testing.expect(param_array[3].has_colon == true);
    try testing.expect(param_array[3].arg != null);

    try testing.expect(param_array[4].op == .PARAMETER_MAYBE);
    try testing.expect(param_array[4].has_colon == true);
    try testing.expect(param_array[4].arg != null);

    try testing.expect(param_array[5].op == .PARAMETER_LEADING_HASH);
    try testing.expect(param_array[5].has_colon == false);
    try testing.expect(param_array[5].arg == null);

    try testing.expect(param_array[6].op == .PARAMETER_HASH);
    try testing.expect(param_array[6].has_colon == false);
    try testing.expect(param_array[6].arg != null);

    try testing.expect(param_array[7].op == .PARAMETER_DOUBLE_HASH);
    try testing.expect(param_array[7].has_colon == false);
    try testing.expect(param_array[7].arg != null);

    try testing.expect(param_array[8].op == .PARAMETER_PERCENT);
    try testing.expect(param_array[8].has_colon == false);
    try testing.expect(param_array[8].arg != null);

    try testing.expect(param_array[9].op == .PARAMETER_DOUBLE_PERCENT);
    try testing.expect(param_array[9].has_colon == false);
    try testing.expect(param_array[9].arg != null);
}

test "Parse Word Quotes and backslash" {
    const command_string = "echo \"home $HOME dir\" '$NOTVAR' word\\ with\\ spaces";
    var parser = Parser.init(testing.allocator, command_string);
    const program = try parser.parse();
    defer program.deinit(testing.allocator);

    const simple_command = program.body[0].and_or_cmd_list.cast(.PIPELINE).?.commands[0].cast(.SIMPLE_COMMAND).?;
    try testing.expect(simple_command.args != null);

    const args = simple_command.args.?;
    try testing.expect(args[0].kind == .LIST);
    try testing.expect(args[1].kind == .STRING);
    try testing.expect(args[2].kind == .STRING);

    const word_arg1 = args[0].cast(.LIST).?;
    try testing.expect(word_arg1.is_double_quoted == true);
    try testing.expect(word_arg1.items.len == 3);

    try testing.expect(word_arg1.items[0].kind == .STRING);
    try testing.expect(mem.eql(u8, word_arg1.items[0].cast(.STRING).?.str, "home "));

    try testing.expect(word_arg1.items[1].kind == .PARAMETER);
    try testing.expect(word_arg1.items[1].cast(.PARAMETER).?.op == .PARAMETER_NO_OP);

    try testing.expect(word_arg1.items[2].kind == .STRING);
    try testing.expect(mem.eql(u8, word_arg1.items[2].cast(.STRING).?.str, " dir"));

    const word_arg2 = args[1].cast(.STRING).?;
    try testing.expect(word_arg2.is_single_quoted == true);
    try testing.expect(mem.eql(u8, word_arg2.str, "$NOTVAR"));

    const word_arg3 = args[2].cast(.STRING).?;
    try testing.expect(word_arg3.is_single_quoted == false);
    try testing.expect(mem.eql(u8, word_arg3.str, "word\\ with\\ spaces"));
}
