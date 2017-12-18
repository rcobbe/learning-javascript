"use strict";

import * as list from "./list.mjs";
import * as show from "./show.mjs";

// Defines a functional map from keys to values.  Uses === to compare keys.
// For now, implement as alist for simplicity; can replace this with a
// more efficient representation as need requires.

var _tag = Symbol("tag");
var _key = Symbol("key");
var _val = Symbol("val");
var _next = Symbol("next");
var _showElements = Symbol("showElements");

export function isMap(x) {
    return x.hasOwnProperty(_tag);
}

export var empty = {
    [_tag]: _tag,  // value isn't important; we only check for existence
                   // of the field
    extend: function extend(k, v) { return extendImpl(k, v, this); },
    hasKey: function hasKey(k) { return false; }
    lookup: function lookup(k) { return undefined; },
    show: function show() { return "Map()"; },
    [_showElements]: function showElements() { return ''; }
};

// function interface to extension, for better chaining:
//   extend('a', 1, extend('b', 2, extend('c', 3, m)))
// reads better than
//   m.extend('c', 3).extend('b', 2).extend('a', 1)
// in some circumstances.
export function extend(k, v, map) {
    console.assert(isMap(map));
    return map.extend(k, v);
};

function extendImpl(k, v, map) {
    console.assert(isMap(map));
    return {
        [_tag]: _tag,
        [_key]: k,
        [_val]: v,
        [_next]: map,
        extend: function extend(k, v) { return extendImpl(k, v, this); },
        hasKey: function hasKey(k) {
            return k === this[_key] || this[_next].hasKey(k);
        }
        lookup: function lookup(k) {
            if (this[_key] === k) {
                return this[_val];
            } else {
                return this[_next].lookup(k);
            }
        },
        show: function show() {
            return 'Map(' + this[_showElements](true) + ')';
        },
        // Returns string representation of this node's key-value pair
        // with a leading ', ', unless we're the first in the map
        [_showElements]: function(isFirst) {
            let rest = this[_next][_showElements](false);
            rest =
                '(' + show.show(this[_key]) + ', ' +
                show.show(this[_val]) + ')' + rest;
            if (!isFirst) {
                rest = ', ' + rest;
            }
            return rest;
        }
    };
}
