package scalacats.chapter7

object ListFolding {
  def map[A, B](la: List[A])(f: A => B): List[B] =
    la.foldRight(List[B]())((a, lb) => f(a) :: lb)

  def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] =
    la.foldRight(List[B]())((a, lb) => f(a) ::: lb)

  def filter[A](la: List[A])(p: A => Boolean): List[A] =
    la.foldRight(List[A]())((a, lp) => if (p(a)) a :: lp else lp)

  def sum[B >: A, A](la: List[A])(implicit num: Numeric[B]): B =
    la.foldRight(num.zero)((a: A, sum: B) => num.plus(a, sum))
}
