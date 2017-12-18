"use strict";

import * as map from './map.mjs';
import * as store from './store.mjs';

var _tag = Symbol("_tag");
var _bindings = Symbol("_bindings");

function mkEnv(bindings) {
    console.assert(map.isMap(bindings));
    return {
        [_tag]: _tag,
        [_bindings]: bindings,

        lookup: function lookup(id) {
            console.assert(typeof(id) === 'string');
            console.assert(this[_bindings].hasKey(id));
            return this[_bindings].lookup(id);
        }
        extend: function(id, addr) {
            console.assert(typeof(id) === 'string');
            console.assert(store.isAddress(addr));
            return mkEnv(this[_bindings].extend(id, addr));
        },
        show: function() {
            return 'Env(' + this[_bindings].show() + ')';
        }
    };
}

export var empty = mkEnv(map.empty);

export function bind(id, val, env, store) {
    let addrStore = store.alloc(val);
    let addr = addrStore.addr;
    let newStore = addrStore.store;
    return {
        env: env.extend(id, addr),
        store: newStore
    }
}
