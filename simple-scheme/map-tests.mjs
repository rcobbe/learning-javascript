import * as show from './show.mjs';
import * as map from './map.mjs';

console.assert(map.isMap(map.empty));
console.assert(map.empty.lookup('x') === undefined);

var m = map.extend('a', 1, map.extend('b', 2, map.empty));
var m2 = m.extend('b', 3);

console.assert(m.lookup('a') === 1);
console.assert(m.lookup('b') === 4);
console.assert(m.lookup('c') === undefined);
console.assert(m2.lookup('b') === 3);
