TODO in no particular order

init:

* proper initializing
* have option to only run a script/command

parser:

* have parser errors treated
* command types:
  - case
* parse here documents
* line continuation (PS2)
* word arithmetic
* do temporary storage to not do a bunch of small allocations, scratch buffer? see https://github.com/ziglang/zig/pull/10079/files
* async pipe (|&)

AST:

* command types

line read:

* change keybinds
* vi mode
* emacs mode
* history:
  - save
  - search
  - load
* PS1:
  $(printf $BLUE)\w $(git_info)
  $(_tab)$(get_exit)>$(printf $NORMAL)
* PS{2,3,4}

exec:

* dont try to execute invalid programs
* word expansion:
  - tilde expansion
  - parameter expansion
  - fields splits
  - brace expansion
  - filename patterns
* I/O redirection
  - builtin I/O redirection
  - here document
  - clobber
* pipeline greater than 1 command
* command types:
  - functions
  - subshell
  - for loops
  - while loops
  - case

symbol table:

* read-only vars
* env vars used by oksh
* aliases
* functions

builtins:

* read +/- options
* set
* unset
* typeset
* alias
* unalias
* bg, fg
* command
* fc
* getopts
* jobs
* kill
* read
* umask
* wait
* readonly
* shift
* times
* trap
* break, continue
* eval
* exec
* return
* export
* [, test
* echo, print
* let
* suspend
* ulimit
* whence

job controller:

* needs fg, bg, wait, jobs, kill

misc:

* check memory leaks
* consider removal of positions
* priority of execution
* POSIX mode
* errors messages equivalent to oksh
* see where to handle errors
* strict bourne shell mode
* print error, a error printer that is centralized at some place
* profile allocators
* complete command/path
* hints?


TODO: there was a segfault after running with one or more arguments, also with only the command name it hangs

make easy to read files, it should make a blis to read configuration files
