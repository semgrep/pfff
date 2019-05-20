import { bar, bar_safe } from './bar.js';

function foo() {
    return bar();
}

function main() {
    var x = foo();
    console.log(x);
    var y = bar_safe();
    console.log(y);
}

main();

