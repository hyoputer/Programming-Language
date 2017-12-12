let val k = fn x =>
  (x false, x true)
in
  k (fn x => x)
end
