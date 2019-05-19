var t = taint ();
var x = 1;
var y = t;
console.log(y);

var arr = [];
arr[1] = t;
var z = arr[1];
console.log(z);

function foo(a) {
    var b = a;
    return b;
}

var w = foo(t);
console.log(w);

var o = {
    fld1: x,
    fldt: t,
};

var a = o.fld1;
var b = o.fldt;

console.log(a);
console.log(b);
