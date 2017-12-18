"use strict";

import * as map from "./map.mjs";
import * as show from "./show.mjs";

var _addrTag = Symbol("addrTag");
var _addrVal = Symbol("addrVal");
var _storeTag = Symbol("storeTag");
var _nextAddr = Symbol("nextAddr");
var _memory = Symbol("memory");

export function isAddress(x) {
    return x.hasOwnProperty(_addrTag);
}

function mkAddress(p) {
    console.assert(typeof(p) === 'number');
    return { 
        [_addrTag]: _addrTag, 
        [_addrVal]: p,
        inc: function inc() {
            return mkAddress(this[_addrVal] + 1);
        },
        show: function show() {
            return 'address(' + this[_addrVal] + ')';
        }
    };
}

export function isStore(x) {
    return x.hasOwnProperty(_storeTag);
}

export var empty = mkStore(mkAddress(0), map.empty);

function mkStore(nextAddr, memory) {
    return {
        [_storeTag]: _storeTag,
        [_nextAddr]: nextAddr,
        [_memory]: memory,

        alloc: function alloc(v) {
            let addr = this[_nextAddr];
            return {
                addr: addr,
                store: mkStore(addr.inc(), memory.extend(addr, v))
            }
        },

        update: function update(addr, v) {
            console.assert(this[_memory].hasKey(addr));
            return mkStore(this[_nextAddr], this[_memory].extend(addr, v));
        },

        deref: function deref(addr) {
            console.assert(this[_memory].hasKey(addr));
            return this[_memory].lookup(addr);
        },

        show: function show() {
            return 'Store(' + this[_nextAdd].show() + ', ' +
                this[_memory].show() + ')';
        }
    };
}
