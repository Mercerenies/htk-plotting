
A sample project written using the HTk GUI library for Haskell,
primarily intended for didactic purposes targeted at anyone learning
GUI programming in Haskell.

The program is a very simple plotting utility that can plot basic
functions. Build the program using `./build.sh` and run it using
`./Main`. When the program runs, simply type the expression for a
function into the textbox and hit the "Plot" button (or the return
key, if you prefer). The following operations are supported:

 * Basic Arithmetic (`+`, `-`, `*`, `/`, `^`)
 * Trigonometric Functions (`sin`, `cos`, `tan`)
 * Miscellaneous Functions (`sqrt`, `ln`)

Use the variable `x` as the independent variable.

Note that division has a lower precedence than multiplication. This is
consistent with common mathematical notation but contrary to many
programming languages. So `5 * 6 / 7 * 8` parses as `(5 * 6) / (7 *
8)`, not `((5 * 6) / 7) * 8`.
