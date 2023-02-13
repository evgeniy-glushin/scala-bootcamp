object MatchType extends App {

  type ConstituentPartOf[T] = T match
    case BigInt => Int
    case String => Char
    case List[t] => t

  //val digit: ConstituentPartOf[BigInt] = 5
  // ...

//  def lastOf[T](value: T): ConstituentPartOf[T] = value match
//    case b: BigInt => (b % 10).toInt
//    case s: String =>
//      if s.isEmpty then throw new NotImplementedError
//      else s.charAt(s.length - 1)
//    case l: List[T] =>
//      if l.isEmpty then throw new NotImplementedError
//      else l.last

}
