"use strict";

var _valueTag = Symbol("valueTag");

export function mkString(s) {
    console.assert(typeof(s) === 'string');
    return { [_valueTag]: 'str-lit', val: s };
}

export function isString(v) {
    return v.hasOwnProperty(_valueTag) && v[_valueTag] === 'str-lit';
}
