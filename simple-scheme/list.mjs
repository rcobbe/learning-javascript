"use strict";

import { show } from "./show.mjs";

var _tag = Symbol("tag");
var _car = Symbol("car");
var _cdr = Symbol("cdr");
var emptyListTag = Symbol("emptyListTag");
var listTag = Symbol("listTag");

export var empty = {
    [_tag]: emptyListTag,
    show: function show() { return '[]'; }
};

export function isList(x) {
    return x.hasOwnProperty(_tag) &&
        (x[_tag] === listTag || x[_tag] === emptyListTag);
}
export function isEmpty(x) {
    console.assert(isList(x));
    return (x[_tag] === emptyListTag);
}
export function cons(x, y) {
    console.assert(isList(y));
    return {
        [_tag]: listTag,
        [_car]: x,
        [_cdr]: y,
        show: function s() { return '[' + showElements(this) + ']'; }
    };
}
export function car(x) {
    console.assert(isList(x));
    return x[_car];
}
export function cdr(x) {
    console.assert(isList(x));
    return x[_cdr];
}

// foldr :: (a b -> b) b List<a> -> b
export function foldr(f, base, list) {
    console.assert(isList(list));
    if (isEmpty(list)) {
        return base;
    } else {
        return f(car(list), foldr(f, base, cdr(list)));
    }
}

// foldl :: (a b -> a) a List<b> -> a
export function foldl(f, base, list) {
    console.assert(isList(list));
    if (isEmpty(list)) {
        return base;
    } else {
        return foldl(f, f(base, car(list)), cdr(list));
    }
}

export function foldl2(f, base, list) {
    console.assert(isList(list));
    let result = base;
    let l = list;
    while (!(isEmpty(l))) {
        result = f(result, car(l));
        l = cdr(l);
    }
    return result;
}

// map :: (a -> b) List<a> -> List<b>
export function map(f, list) {
    console.assert(isList(list));
    return foldr(
        function(x, accum) { return cons(f(x), accum); },
        empty,
        list
    );
}

function showElements(list) {
    console.assert(isList(list) && !(isEmpty(list)));
    if (isEmpty(cdr(list))) {
        return '' + show(car(list));
    } else {
        return '' + show(car(list)) + ', ' + showElements(cdr(list));
    }
}

// XXX add Iterable
