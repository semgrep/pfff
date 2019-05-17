function foo(x) {
  console.log(x);
}

// The interpreter will run those multiple funcalls with different contexts
// so it's kinda context-sensitive. The problem is that it's not really
// path sensitive (see if.js).
foo(42);
foo("bar");
