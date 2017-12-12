let rec fib = fn n =>
  if n = 0 then 0 else fib (n - 1)
in
  fib 5
end
