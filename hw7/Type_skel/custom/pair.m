let val a = (fn p => (p.1) (p.2))
in
  a ((fn y => y.1 or y.2), (4,3))
end
