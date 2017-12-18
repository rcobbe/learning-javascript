"use strict";

import { show } from './show.mjs';
import * as List from './list.mjs';
import * as Value from './value.mjs'

/**********************************************************************
 *
 * expression configurations
 *
 **********************************************************************/

function mkExprConfig(expr, env, st, k) {
    return {
        step: function step() {
            console.assert(false, "unimplemented");
        },
        show: function show() {
            return '<' + show(expr) +
                ', ' + show(env) +
                ', ' + show(st) +
                ', ' + show(k) +
                '>';
        }
    };
}

/**********************************************************************
 *
 * value configurations
 *
 **********************************************************************/

function mkValueConfig(val, st, k) {
    return {
        step: function step() {
            return k.apply(val, st);
        }
        show: function show() {
            return '<' + show(val) + ', ' + show(st) + ', ' + show(k) + '>';
        }
    };
}

/**********************************************************************
 *
 * Continuations
 *
 **********************************************************************/

var _continuationTag = Symbol("continuation");

export function isContinuation(x) {
    return x.hasOwnProperty(_continuationTag);
}

export var halt_k = {
    [_continuationTag]: 'halt',
    apply: function apply(val, st) { return val; }
    show: function show() { return 'halt-k'; }
};

export function mkLetrec_k(rho, currSym, restSyms, restRHSs, body, k) {
    return {
        [_continuationTag]: 'letrec',
        // XXX does apply close over mkLetrec_k's arguments?
        apply: function apply(val, st) {
            let newSt = updateID(currSym, val, rho, st);
            if (List.isEmpty(restSyms)) {
                mkExprConfig(body, rho, newSt, k);
            } else {
                mkExprConfig(
                    List.car(restRHSs),
                    rho,
                    newSt,
                    mkLetrec_k(
                        rho,
                        List.car(restSyms),
                        List.cdr(restSyms),
                        List.cdr(restRHSs),
                        body,
                        k
                    )
                );
            }
        },
        show: function show() {
            return 'letrec_k(' +
                show(rho) + ', ' +
                show(currSym) + ', ' +
                show(restSyms) + ', ' +
                show(restRHSs) + ', ' +
                show(body) + ', ' +
                show(k) + ')';
        }
    };
}

function mkSet_k(addr, k) {
    return {
        [_continuationTag]: "set",
        apply: function apply(val, st) {
            return mkValueConfig(
                Value.voidVal,
                st.update(addr, val),
                k
            );
        },
        show: function show() {
            return 'set_k(' + show(addr) + ', ' + show(k) + ')';
        }                
    };
}

function mkIf_k(rho, thenExpr, elseExpr, k) {
    return {
        [_continuationTag]: 'if',
        apply: function apply(val, st) {
            return mkExprConfig(
                (Value.isBool(val) && !(val.val)) ? elseExpr : thenExpr,
                rho,
                st,
                k
            );
        },
        show: function show() {
            return 'if_k(' + show(rho) + ', ' + show(thenExpr) + ', ' +
                show(elseExpr) + ', ' + show(k) + ')';
        }
    }
}

/**********************************************************************
 *
 * misc
 *
 **********************************************************************/

function updateID(sym, val, env, store) {
    return store.update(env.lookup(sym), val);
}
