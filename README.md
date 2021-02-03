# Command-line Flag Parsing

[![Gitter](https://badges.gitter.im/qitab/community.svg)](https://gitter.im/qitab/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

The `ace.flag:parse-command-line` parses command line flags as defined by the
`ace.flag:define` macro. `ace.flag:print-help` formats a `--help` message for
all defined flags.

Flags may set special variables, if this feature is enabled. For example,
`--cl:print-base` and `--cl:*print-base*` refer to the `*print-base*` special
variable. This behavior is controlled by `--lisp-global-flags`.  The user may
also allow this by default by passing `:global-flags t` to the
`ace.flag:parse-command-line` function.

Boolean flags are treated specially. Prefixing boolean flags with "no" sets the
flag value to nil and no other arguments are consumed (e.g. `--nofoo` means to
set the `foo` flag to nil). Boolean flags that are not followed by value
arguments (that is, arguments that are not prefixed with dash) are set to `t`
and no more argument are consumed. Boolean flags also accept explicit values:
`yes`, `no`, `true`, `false`, `t`, `null`, and `nil`. These values are consumed
during the parsing of the boolean flags.

Flags may accept any combination of types.  By default the values parsed are:
numbers, symbols, keywords, strings.  Numbers are parsed in C++/Java syntax
allowing for non-finite values (`inf`, `-inf`, `nan`).

### Disclaimer: This is not an official Google product.
