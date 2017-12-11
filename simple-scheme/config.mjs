"use strict";

// Expr ::=
//   number
//   {op: 'str-lit', val: string}
//   {op: 'bool-lit', val: value}
//   {op: 'quote', val: string}
//   string  (identifier reference)
//   ['lambda', [str ...], Expr]
//   ['let', [[str, Expr] ...], Expr]
//   ['letrec', [[str, Expr] ...], Expr]
//   ['let/cc', str, Expr]
//   ['set!', str, Expr]
//   ['if', Expr, Expr, Expr]
//   ['and', Expr ...]
//   ['or', Expr ...]
//   ['begin' Expr ..+]
//   [Expr ..+]

