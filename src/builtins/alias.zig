const std = @import("std");
const JobController = @import("../jobs.zig").JobController;
const Option = @import("../builtins.zig").Option;
const OptIterator = @import("../builtins.zig").OptIterator;
const symtab = @import("../symtab.zig");
const writer = std.io.getStdOut().writer();

const AliasOptions = enum {
    BRUH,
};

/// alias [-d | -t [-r] | +-x] [-p] [+] [name [=value] ...]
///
/// Without arguments, alias lists all aliases. For any name without a value, the existing
/// alias is listed. Any name with a value defines an alias (see Aliases above).
///
/// When listing aliases, one of two formats is used.  Normally, aliases are listed as name=value,
/// where value is quoted.  If options were preceded with `+', or a lone `+' is given on the command
/// line, only name is printed.
///
/// The -d option causes directory aliases, which are used in tilde expansion, to be listed or set
/// (see Tilde expansion above).
///
/// If the -p option is used, each alias is prefixed with the string "alias ".
///
/// The -t option indicates that tracked aliases are to be listed/set (values specified on the command
/// line are ignored for tracked aliases).  The -r option indicates that all tracked aliases are to be reset.
///
/// The -x option sets (+x clears) the export attribute of an alias or, if no names are given, lists the
/// aliases with the export attribute (exporting an alias has no effect).
const options = [_]Option(AliasOptions){};

// TODO make kzhAlias ensureTotalCapacity by itself
pub fn kzhAlias(ctl: *JobController, args: []const []const u8) u8 {
    _ = ctl;
    _ = args;
    return 1;
}
