val tuple = (1, 'a', 2)

val (a, b, c) = tuple


val testMap = Map('a'->1, 'b'->2)

testMap map ({case (c, i) => i->c})


val (x, y) = (1, 2)
for {
  (x, y) <- List((1, 2), (5, 6))
} yield (x+1, y+1)