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

def routeToEnd(currentRoute: List[Point]): List[Point] = {
  if (currentRoute.length == 10) currentRoute
  else {
    val otherRoutes = for {
      nextPos <- jump(currentRoute.head)
      if !(currentRoute contains nextPos)
      newRoute = nextPos :: currentRoute
      expandedRoute <- routeToEnd(newRoute)
      
    } yield routeToEnd(expandedRoute)
    if (otherRoutes.isEmpty) Nil

  }
}

val firstRoutes = routesToEnd(Stream(List(Point(0, 0)))).take(10)