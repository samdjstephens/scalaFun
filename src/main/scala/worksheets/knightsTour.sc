import Math.abs

val boardSize = 8

case class Point(x: Int, y: Int) {
  def isInBounds = x <= boardSize-1 && y <= boardSize-1 && x >= 0 && y >= 0
}

def isKnightsMove(xDisplacement: Int, yDisplacement: Int): Boolean = {
  abs(xDisplacement) != abs(yDisplacement) && (abs(xDisplacement) + abs(yDisplacement) == 3)
}

def jump(p: Point): Seq[Point] = for {
  xDisplacement <- -2 to 2
  yDisplacement <- -2 to 2
  if isKnightsMove(xDisplacement, yDisplacement)
  point = Point(p.x + xDisplacement, p.y + yDisplacement)
  if point.isInBounds
} yield point

jump(Point(0,0)) foreach println

def routesToEnd(routes: Stream[List[Point]]): Stream[List[Point]] = {
  val newRoutes = for {
    route <- routes
    nextPos <- jump(route.head)
    if !(route contains nextPos)
  } yield nextPos :: route
  routes #::: routesToEnd(newRoutes)
}

val firstRoutes = routesToEnd(Stream(List(Point(0, 0)))).dropWhile(_.length < 4).head