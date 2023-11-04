# Tinip - Tiny Interpreter

![GitHub Actions](https://github.com/sporad/bapl23/actions/workflows/ci.yml/badge.svg)

November 2023

Tinip* is a single-file Lua implementation of a very simple interpreter
developed as a project to submit
in **Building a Programming Language** (BaPL) at Classpert, 2023.
(*Pronounced /tiˈnip/)

The source code of Tinip is available at GitHub:

- https://github.com/sporad/bapl23

This interpreter is based on [the code provided by the class organizer][refcode],
i.e., not built from scrach.
Tinip is distributed with the same MIT license as the original code.

[refcode]: https://github.com/classpert/bapl-class-language

## Dependencies

- Lua 5.4.1
- LPeg 1.1

## The Tinip language and program

Tinip program must have one `main` function.
Tinip is implemented as a Lua module, `tinip.lua`,
which requires [LPeg][url-lpeg] and a print table module `pt.lua`.

You can run a Tinip program within Lua.
For example, the following Lua program contains a Tinip program
as a Lua's long string between `[[` and `]]`.
Comment in Tinip starts with `#`.

[url-lpeg]: https://www.inf.puc-rio.br/~roberto/lpeg/

```lua
-- Lua program demonstrating how to run tinip program
local tinip = require "tinip"
tinip_source = [[
# Tinip program

function myfunc(k) {
  return k * 3;
}

function main() {
    var a = -1;
    return myfunc(a);
}
]]
result = tinip.interpreter(tinip_source) --> -3
```

- Function must be implemented starting with `function` keyword.
- Function name must start with a-z, A-Z followed by a-z, A-Z, 0-9.
- Function parameters must be comma-separate declared, after function name, between `(` and `)`.
- Function body must be implemented, after function parameter declaration, between `{` and `}`.
- Function must have a `return` statement at the end.
- Variable must be declared with `var`
- Variable must be either a scalar or an array.
- Array variable can be created with `new` as `var x = new [3]` where `[3]` specifies the size.
- Array index starts from 1.
- Array element is accessible as `x[1]`, `x[2]`, and so on.
- Statement must be terminated with `;`
- Arithmetic operations: `+`, `-`, `*`, `-`.
- Comparison operators: `==`, `~=`, `<=`, `<`, `>=`, `>`
- `&&` for short-cut AND
- `||` for short-cut OR
- Interger and float are supported as number literal.
- Unary plus or minus can be used in front of numbers.
- Reserved keywords:
  `function`,
  `return`,
  `while`,
  `else`,
  `new`,
  `var`,
  `if`.

When a syntax error is found, Tinip reports it with the row and column information
of the error site in the source code.

## Testing

Test files for the Tinip language are stored in `Test`.
They are implemented in Lua scripts using `assert`.

Tinip implementation is automatically tested in GitHub using GitHub Actions.
The source code of Lua and LPeg are included in Tinip's repository,
and they are built in Linux and used during the GitHub Actions CI process.
Tinip repository is self-contained as far as Lua is concerned,
but `gcc` and `make` are still required to run tests.

Test logs in GitHub Actions can be found here:

- https://github.com/sporad/bapl23/actions

If you have access to a Linux machine with `gcc` and `make`,
you can also run the tests locally.
Build Lua and LPeg first, then run tests, as follows.

```bash
make
make test
```

To clean up build artifacts, run `make clean`.

## Potential enhancements

Language features

- Support boolean; true, false.
- Support string.
- Support built-in functions, such as print.

Program analysis

- Find unused variables.
- Find unused functions.
  This may be ok if it is later used as a module to execute another program.

Compilation

- Compile a partial program, such as an expression, a statement, or a function.
- Compilation of a partial program such as a statement
  can be useful for debugging or learning purpose.
- Save a compiled function to file and load it later for executing another program.

## References

The original source code of the interpreter in the BaPL online course,
provided by class instructors, built for each lesson.

- https://github.com/classpert/bapl-class-language

LPeg

- https://www.inf.puc-rio.br/~roberto/lpeg/
- This page has the primary documentation for LPeg.

Lua documentation

- Collection of links to very useful docs: https://www.lua.org/docs.html
- Lua 5.4 reference manual: https://www.lua.org/manual/5.4/

## License

See [LICENSE](LICENSE).

_Copyright 2023 Sporadic Interlude, ihi_

