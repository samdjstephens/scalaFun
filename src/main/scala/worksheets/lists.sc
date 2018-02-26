// http://aperiodic.net/phil/scala/s-99/

val myList = List(1, 1, 2, 3, 5, 8)
val emptyList = List()
val singleElList = List(1)

def last[T](list: List[T]): T = list match {
  case List() => throw new NoSuchElementException("List empty")
  case List(x) => x
  case x :: rest => last(rest)
}

last(myList)
//last(emptyList)

def penultimate[T](list: List[T]): T = list match {
  case List() => throw new NoSuchElementException("List empty")
  case List(x) => throw new NoSuchElementException("List only has one element")
  case x :: y :: Nil => x
  case x :: rest => penultimate(rest)
}

penultimate(myList)
//penultimate(emptyList)
//penultimate(singleElList)

def nth[T](n: Int, list: List[T]): T = (n, list) match {
  case (0, x :: _) => x
  case (_, Nil) => throw new IndexOutOfBoundsException("Element doesn't exist")
  case (m, _ :: rest) => nth(m - 1, rest)
}

nth(0, myList)
nth(5, myList)
//nth(10, myList)
//nth(1, emptyList)
//nth(1, singleElList)


def length[T](list: List[T]): Int = {
  def lengthRecur(l: Int, list: List[T]): Int = list match {
    case Nil => l
    case _ :: rest => lengthRecur(l + 1, rest)
  }

  lengthRecur(0, list)
}

length(myList)
length(emptyList)
length(singleElList)


for {
  x <- 1 to 10
  y <- 2 to 10
} yield List(x) :: List(y)