import cats.{Applicative, Semigroup}
import cats.data.Validated


object FormValidation extends App {

  implicit val intCombine: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Must be non negative")))
      .combine(Validated.cond(n <= 100, n, List("Must be <= 100")))

  val valid = validateNumber(5)

  var either = valid.toEither

  //println(validateNumber(2))

  type FormValidation[T] = Validated[List[String], T]

  import cats.syntax.validated._

  def nameValidation(name: Option[String]): FormValidation[String] = name match
    case Some(value) if value != "" => Validated.valid(value)
    case Some(value) => Validated.invalid(List("name not be empty"))
    case None => Validated.invalid(List("name must contain value"))
  def emailValidation(email: Option[String]): FormValidation[String] = email match
    case Some(value) if value.contains("@") => value.valid[List[String]]
    case Some(value) => Validated.invalid(List("email must contain @"))
    case None => Validated.invalid(List("email must not be empty"))

  def pwdValidation(pwd: Option[String]): FormValidation[String] = pwd match
    case Some(value) if value.length >= 5 => Validated.valid(value)
    case Some(value) => Validated.invalid(List("pwd must contain at least 5 chars"))
    case None => Validated.invalid(List("pwd must not be empty"))

  //implicit val strSemigroup: Semigroup[String] = Semigroup.instance[String]((_, _) => "success")
  def validateForm(form: Map[String, String]): FormValidation[String] =
    nameValidation(form.get("name"))
      .combine(emailValidation(form.get("email")))
      .combine(pwdValidation(form.get("password")))

  val form = Map(
    "name" -> "yevhen",
    "email" -> "yevhen@gmail",
    "password" -> "ddvd"
  )

  println(validateForm(form).map(_ => "success"))

  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ???
  def product[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] =
    ap(applicative.map(wa)(a => (b: B) => (a,b)))(wb)
    //applicative.product(wa, wb)
}
