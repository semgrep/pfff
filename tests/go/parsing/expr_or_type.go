package foo;


func f2(arg int) (int, error) {
	if arg == 42 {

		// In this case we use `&argError` syntax to build
		// a new struct, supplying values for the two
		// fields `arg` and `prob`.
		return -1, &argError{arg, "can't work with it"}
	}

	// This `mutex` will synchronize access to `state`.
	var mutex = &sync.Mutex{}

	return arg + 3, nil
}

