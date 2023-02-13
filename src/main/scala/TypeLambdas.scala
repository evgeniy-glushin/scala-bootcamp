object TypeLambdas extends App {
  class Functor[F[_]]
  val functorOption = Functor[Option]

  type MyList = [T] =>> List[T]

  type MapWithStringKey = [T] =>> Map[String, T]
  type MapWithStringKey2 [T] = Map[String, T]
  
  
}
