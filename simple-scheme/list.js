"use strict";

var listTag = Symbol("list");
var emptyListTag = Symbol("emptyListTag");
var emptyList = { tag: emptyListTag };
    
export function isList(x) { return (x.tag === listTag || isNull(x)); };
export var empty = emptyList;
export function isEmpty(x) { return (x.tag === emptyListTag); };
export function cons(x, y) {
    console.assert(isList(y));
    return { tag: listTag, car: x, cdr: y };
};
export function car(x) {
    console.assert(isList(x));
    return x.car;
};
export function cdr(x) {
    console.assert(isList(x));
    return x.cdr;
};
