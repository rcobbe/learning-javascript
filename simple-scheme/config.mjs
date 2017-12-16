"use strict";

// Value ::=
//   number
//   {value_tag: 'str-lit', val: string}
//   {value_tag: 'bool-lit', val: boolean}
//   {value_tag: 'symbol', val: string}
//   {value_tag: 'empty-list'}
//   {value_tag: 'void'}
//   {value_tag: 'pair', car: Addr, cdr: Addr}
//   {value_tag: 'closure', rho: Env, formals: [str, ...], body: Expr}
//   {value_tag: 'continuation', k: Continuation}
//   {value_tag: 'undefined'}
//   {value_tag: 'primitive', f: [Value, ...] Store -> [Value, Store]}

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

