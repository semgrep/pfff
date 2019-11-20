void test_dead_break() {

// we have a few code like this. It's an error but a less important one
  switch(1) {
    case 1:
      return 2;
      //break; TODO: no loc information for now
      //ERROR: dead statement
      xbreak();
    default:
      foo();
      break;
  }

}


void test_dead_after_switch() {
  switch(1) {
    case 1:
      return 2;
    default:
      return 3;
  }
  //ERROR: dead statement, requires a not-so-easy CFG to detect that
  echo(1);
}
