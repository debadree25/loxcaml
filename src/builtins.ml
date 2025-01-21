open Value

let clock = NativeFunc (0, "clock", fun _ -> Primitive (LNumber (Unix.time ())))

let builtins_list = [
  ("clock", clock)
]