package scalacats.chapter9

import cats.instances.future._
import cats.instances.vector._
import cats.kernel.Monoid
import cats.syntax.foldable._
import cats.syntax.traverse._

import scala.collection.TraversableOnce
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object VectorOps {
  def groupedByCpus[A](values: Vector[A]) = {
    val batchCount = Runtime.getRuntime.availableProcessors
    values.grouped((values.size.toDouble / batchCount).ceil.toInt).toVector
  }
}

object MapReduce {
  def foldMap[A, B: Monoid](values: TraversableOnce[A])(func: A => B): B = {
    values.map(func).fold(Monoid[B].empty)(Monoid[B].combine)
  }

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val batches = VectorOps.groupedByCpus(values)
    val processedBatches = batches.map(batch => Future(foldMap(batch)(func)))
    Future.sequence(processedBatches)
        .map(foldMap(_)(identity))
  }
}

object MapReduceCats {
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    VectorOps.groupedByCpus(values)
      .traverse(batch => Future(batch.foldMap(func)))
      .map(_.combineAll)
  }
}
