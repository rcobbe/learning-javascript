"use strict";

var _valueTag = Symbol('value');

export function isValue(x) { return x.hasOwnProperty(_valueTag); }

export function mkString(s) {
    console.assert(typeof(s) === 'string');
    return {
        [_valueTag]: 'str-lit',
        val: s,
        show: function show() { return '"' + s + '"'; }
    };
}

export function isString(v) {
    return isValue(v) && v[_valueTag] === 'str-lit';
}

export function mkBool(b) {
    return {
        [_valueTag]: 'bool-lit',
        val: (b === true),
        show: function show() {
            if (this.val) {
                return '#t';
            } else {
                return '#f';
            }
        }
    };
}

export function isBool(v) {
    return isValue(v) && v[_valueTag] === 'bool-lit';
}

export function mkSymbol(s) {
    console.assert(typeof(s) === 'string');
    return {
        [_valueTag]: 'symbol',
        val: s,
        show: function show() {
            return "'" + this.val;
        }
    };
}

export function isSymbol(x) {
    return isValue(x) && x[_valueTag] === 'symbol';
}

export var nullVal = {
    [_valueTag]: 'null',
    show: function show() { return "'()"; }
};

export function isNull(v) {
    return isValue(v) && v[_valueTag] === 'null';
}

export voidVal = {
    [_valueTag]: 'void',
    show: function show() { return '#<void>'; }
}

export function isVoid(v) {
    return isValue(v) && v[_valueTag] === 'void';
}

export function mkPair(carAddr, cdrAddr) {
    console.assert(isAddress(carAddr));
    console.assert(isAddress(cdrAddr));
    return {
        [_valueTag]: 'pair',
        car: carAddr,
        cdr: cdrAddr,
        show: function show() {
            return 'pair(' + this.car.show() + ', ' + this.cdr.show() + ')';
        }
    };
}

export function isPair(x) {
    return isValue(x) && x.[_valueTag] === 'pair';
}

export function mkClosure(rho, formals, body) {
    console.assert(env.isEnv(rho));
    console.assert(typeof(formals) === 'array');
    // XXX assert body is expression
    return {
        [_valueTag]: 'closure',
        rho: rho,
        formals: formals,
        body: body,
        show: function show() {
            return 'closure(' + rho.show() + ', ' + formals.show() + ', ' +
                body.show() + ')';
        }
    };
}

export function isClosure(x) {
    return isValue(x) && x[_valueTag] === 'closure';
}

export function mkContinuation(k) {
    return {
        [_valueTag]: 'continuation',
        k: k,
        show: function show() {
            return 'continuation(' + k.show() + ')';
        }
    };
}

export function isContinuation(x) {
    return isValue(x) && x[_valueTag] === 'continuation';
}

export function mkUndefined() {
    return {
        [_valueTag]: 'undefined',
        show: function show() { return '#<undefined>'; }
    };
}

export fucntion isUndefined(x) {
    return isValue(x) && x[_valueTag] === 'undefined';
}

export function mkPrimitive(name, f) {
    return {
        [_valueTag]: 'primitive',
        name: name,
        f: f,
        show: function show() {
            return 'primitive(' + name + ')';
        }
    };
}
