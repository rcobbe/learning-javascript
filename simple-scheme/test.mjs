// Run with
//    node --experimental-modules test.mjs

import {cons, empty} from './list.mjs';

console.log(cons(1, cons(2, cons(3, empty))));
