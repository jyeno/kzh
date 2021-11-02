# Korn Zig sHell

A shell posix implemented using zig, for learning purposes.

Based on Korn shell

see [TODO](https://gitlab.com/jyeno/kzh/blob/master/TODO.md) for a list of planned fixes/improvements

## current features:

* binary operations (&& ||)
* simple command (pipeline with 1 command)
* execution with environment variables

## halfway features:

* word expansion (already parses word parameter, word command, word list, word string)
* job control (creation of jobs, async job)

## builtins:

* cd (needs support to CDPATH)
* pwd
* builtin
* true, false
* exit (needs deinitialize resources and jobs)
