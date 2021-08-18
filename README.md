A shell posix implemented using zig, for learning purposes.

Based on Korn shell

TODO rest of README

- [ ] output/input redirection (>, >>, <, <<, >&, <&)

- [ ] pipe (|) and async pipe (|&)

- [ ] implement readline and vi mode on it

- [ ] assignable keybinds

- [ ] run commands

- [ ] environment variables

- [ ] PS1 MUST HAVE

- [ ] custom functions

- [ ] builtins functions/programs

Dependencies, currently for line reading [linenoize](https://github.com/joachimschmidt557/linenoize) is used, therefore,
you need to execute (prior to compiling):

 git submodule update --init --recursive

builtins:


 * alias
 * cd - partial
 * exit - partial
 * readonly
 * set
 * typeset
 * which
 * command
 * false - complete
 * bg
 * fg
 * fc
 * getopts
 * jobs
 * kill
 * pwd - partial
 * read
 * true - complete
 * umask
 * unalias
 * wait
 * [, test
 * echo - partial
 * let
 * print - partial
 * suspend
 * ulimit
 * whence
 * .
 * :
 * break
 * continue
 * eval
 * exec - partial
 * export
 * readonly
 * return
 * set
 * shift
 * times
 * trap
 * unset
 * !
