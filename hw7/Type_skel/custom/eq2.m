let val i = 2
    val foo = fn x => fn y =>
      if x = y then x else y
in
  foo i 1
end
