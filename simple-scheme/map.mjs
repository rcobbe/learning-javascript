"use strict";

import * as list from "./list.mjs";

// Defines a functional map from keys to values.  Uses === to compare keys.
// For now, implement as alist for simplicity; can replace this with a
// more efficient representation as need requires.

var _tag = Symbol("tag");
var _data = Symbol("data");
var _key = Symbol("key");
var _val = Symbol("val");
var mapTag = Symbol("mapTag");

export function isMap(x) {
    return x.hasOwnProperty(_tag) && x[_tag] === mapTag;
}

export var empty = {
    [_tag]: mapTag,
    [_data]: list.empty
};

// extend :: a b Map<a, b> -> Map<a, b>
export function extend(k, v, m) {
    console.assert(isMap(m));
    return { [_tag]: mapTag,
             [_data]: list.cons({ [_key]: k, [_val]: v }, m[_data])
           };
};

// lookup :: k Map<k, v> -> U<v, undefined>
// returns undefined object if key not found
export function lookup(k, m) {
    console.assert(isMap(m));
    for (var x = m[_data]; !list.isEmpty(x); x = list.cdr(x)) {
        let head = list.car(x);
        if (head[_data] === k) {
            return head[_val];
        }
    }
    return undefined;
}
