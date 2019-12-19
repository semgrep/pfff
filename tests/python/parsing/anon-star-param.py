def regular_fn(param1, param2):
    return param1 + param2

def has_named_varargs_param(param1, param2, *varargs, a_kwarg = 3):
    return param1 + param2 + a_kwarg

def has_anon_varargs_param(param1, param2, *, a_kwarg = 3):
    return param1 + param2 + a_kwarg
