import cats.Applicative

object ApplicativesExercise extends App {
  def productWithApplicative[W[_], A, B](fa: W[A], fb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] =
    val w: W[B => (A, B)] = applicative.map(fa)(a => (b: B) => (a, b))
    applicative.ap(w)(fb)
}
