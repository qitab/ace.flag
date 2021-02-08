# Lisp flag libraries

[![Gitter](https://badges.gitter.im/qitab/community.svg)](https://gitter.im/qitab/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Utilities to parse command line flags.
The parse-command-line utility offers parsing of command line flags that
is close to google3 binary flag parsing.

The parser supports flags defined through the FLAG:DEFINE macro.
ace.flag:print-help is a utility that formats the flags registered with define-flag.

Flags may refer to special variables. E.g. --cl:print-base and --cl::*print-base* refer to
the '*print-base*' special variable. This behavior is controlled by --lisp-global-flags.
The user may also allow this by default by passing :global-flags t
to the PARSE-COMMAND-LINE function.

Flags whose type is defined or expands to 'boolean' are considered boolean flags and this
affects how those are parsed. Especially, the '--no' sets the flag to 'nil' and no other
argument is consumed. Boolean flags that are not followed by another argument or the other
argument is prefixed with '-' are set to 't' and no argument is consumed.
Finally, boolean flags accept a set of boolean indicators: "yes", "no", "true", "false", ...
which are consumed by the boolean flag at parsing.

Flags may accept any combination of types.
By default the values parsed are: numbers, symbols, keywords, strings.
Numbers are parsed in C++/Java syntax allowing for non-finite values ("inf", "-inf", "nan").

### Disclaimer: This is not an official Google product.
