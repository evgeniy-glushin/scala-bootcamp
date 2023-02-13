object Monads extends App {
   // Props:

   // 1. Monad(x).flatMap(f) == f(x)
   def f(n: Int) = Option(x + 1)
   val x = 10

   val prop1 = Option(x).flatMap(f) == f(x)

   println(prop1)

   // 2. Monad(v).flatMap(x=>Monad(x))
   val prop2 = Option(10).flatMap(x => Option(x))

   println(prop2 == Option(10))

   // 3. Monad(v).flatMap(f).flatMap(g) == Monad(v).flatMap(x => f(x).flatMap(g))
   val incrementer = (n: Int) => List(n, n + 1)
   val doubler = (n: Int) => List(n, n * 2)

   val numbers = List(1,2,3)

   val prop3 =
      numbers.flatMap(incrementer).flatMap(doubler) ==
      numbers.flatMap(x => incrementer(x).flatMap(doubler))

   println(prop3)
   // val test =  4 == "sc"
   // println(4 == "sc")


}
