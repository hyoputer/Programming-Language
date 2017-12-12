let val f = fn x => fn y =>
  if x = y then
    write x
  else 
    write y
in
  f 1 2
end
