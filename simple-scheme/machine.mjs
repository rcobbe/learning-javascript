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
                isTrue(val) ? elseExpr : thenExpr,
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

function mkAnd_k(rho, restArgs, k) {
    return {
        [_continuationTag]: 'and',
        apply: function apply(val, st) {
            if (isTrue(val)) {
                return mkExprConfig(Expr.mkAnd(restArgs), rho, st, k);
            } else {
                return mkValueConfig(val, st, k);
            }
        }
        show: function show() {
            return 'and_k(' + show(rho) +
                ', ' + show(restArgs) +
                ', ' + show(k) +
                ')';
        }
    };
}

function mkOr_k(rho, restArgs, k) {
    return {
        [_continuationTag]: 'or',
        apply: function apply(val, st) {
            if (isTrue(val)) {
                return mkValueConfig(val, st, k);
            } else {
                return mkExprConfig(Expr.mkOr(restArgs), rho, st, k);
            }
        },
        show: function show() {
            return 'or_k(' + show(rho) +
                ', ' + show(restArgs) +
                ', ' + show(k) +
                ')';
        }
    };
}

/**********************************************************************
 *
 * misc
 *
 **********************************************************************/

// Recognize any Scheme value that counts as true
function isTrue(v) {
    return !(Value.isBool(val) && !(val.val));
}

function updateID(sym, val, env, store) {
    return store.update(env.lookup(sym), val);
}
