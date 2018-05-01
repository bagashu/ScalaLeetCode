def flatten(xs: List[Any]): List[Any] = {
  xs match {
    case Nil => List.empty[Any]
    case (x: List[Int]) :: y => flatten(x) ::: flatten(y)
    case (x: Int) :: y => x :: flatten(y)
  }
}


val xs = List(1,3)
xs.map{_ + 2}
flatten(List(List(1, 1), 2, List(3, List(5, 8))))

val ma1 = Map(1->66, 3->44)
val ma2 = Map(2->44, 1->22)
ma1 ++ ma2
