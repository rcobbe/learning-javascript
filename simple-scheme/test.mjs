// Run with
//    node --experimental-modules test.mjs

import {cons, empty, show} from './list.mjs';

console.log(show(cons(1, cons(2, cons(3, empty)))));
