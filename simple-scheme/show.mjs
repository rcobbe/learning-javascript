"use strict";

export function show(x) {
    if (typeof(x.show) == 'function') {
        return x.show();
    } else {
        return x.toString();
    }
}
