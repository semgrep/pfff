var o = {
    foo() {
        this.fld1 = 42;
    }
};

o.foo();
console.log(o);
