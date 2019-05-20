function bar() {
    return taint();
}

function taint() {
    return "tainted";
}

function bar_safe() {
    return "safe";
}

export { bar, bar_safe };
