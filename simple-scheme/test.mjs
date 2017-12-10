// Run with
//    node --experimental-modules test.mjs

import {show} from './show.mjs';
import * as list from './list.mjs';

//console.log(show(cons(1, cons(2, cons(3, empty)))));
console.log(show(list.cons(1, list.cons(2, list.cons(3, list.empty)))));
