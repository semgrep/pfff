function var_dump(x) { console.log(x); }

var x = 42;
//$x:&1{42}
var_dump(x);

x = 2;
//$x:&1{int}, no range, go very abstract very quickly
//(also means the analysis is flow-sensitive at some level
//as the same variable can have different values)
var_dump(x);

//$y will be a copy of $x (no sharing, new pointer)
var y = x;
var_dump(y);
