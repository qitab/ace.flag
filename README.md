# Lisp flag libraries

[![Gitter](https://badges.gitter.im/qitab/community.svg)](https://gitter.im/qitab/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Utilities to parse command line flags.  The `parse-command-line` function offers parsing
of command line flags that is close to [Abseil](https://github.com/abseil/abseil-cpp)
command-line flag parsing.

The parser supports flags defined through the `ace.flag:define` macro.
`ace.flag:print-help` formats `--help` output for the flags registered with
`ace.flag:define-flag`.

Flags may refer to special variables. E.g. `--cl:print-base` and `--cl::*print-base*`
refer to the `*print-base*` special variable. This behavior is controlled by
`--lisp-global-flags`.  The user may also allow this by default by passing
`:global-flags t` to `ace.flag:parse-command-line`.

Flags whose type is defined as or expands to 'boolean' are considered boolean flags and
this affects how they are parsed. A '--no' prefix sets the flag to `NIL` and no other
argument is consumed. Boolean flags that are not followed by another argument are set to
`T` and no argument is consumed.  Finally, boolean flags accept a set of boolean
indicators: "yes", "no", "true", "false", ...  which are consumed by the parser.

Flags may accept any combination of types.  By default the values parsed are: numbers,
symbols, keywords, strings.  Numbers are parsed in C++/Java syntax allowing for
non-finite values ("inf", "-inf", "nan").

### Disclaimer: This is not an official Google product.
