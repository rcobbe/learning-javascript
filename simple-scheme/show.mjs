"use strict";

// Convert an object to a human-readable representation.  Uses the argument's
// show method, if it exists; otherwise falls back on toString.
export function show(x) {
    if (typeof(x.show) == 'function') {
        return x.show();
    } else {
        return x.toString();
    }
}
