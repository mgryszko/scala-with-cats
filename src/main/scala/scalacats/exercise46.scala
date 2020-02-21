package scalacats.chapter4

import cats.Eval

object UnsafeFold {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil => acc
    }
}

object SafeFold {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    evalFoldRight(as, Eval.now(acc)) { (a, b) => b.map(fn(a, _))}.value

  def evalFoldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, evalFoldRight(tail, acc)(fn)))
      case Nil => acc
    }
}
