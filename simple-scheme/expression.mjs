"use strict";

import * as List from './list.mjs';

function mkAnd(args) {
    return {
        tag: function tag() { return 'and'; },
        args: args
    };
}

function mkOr(args) {
    return {
        tag: function tag() { return 'or'; },
        args: args
    }
};
