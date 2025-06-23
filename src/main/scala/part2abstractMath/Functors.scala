package part2abstractMath

import scala.util.Try

object Functors {

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor
  import cats.Functor
  import cats.instances.list._  // includes Functor[List]
  val listFunctor: Functor[List] = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1)

  // generalize an API
  def do10x[F[_]](container: F[Int])(using Functor[F]): F[Int] =
    Functor[F].map(container)(_ * 10)

  // same but uses context bound
  def do10x_v2[F[_]: Functor](container: F[Int]): F[Int] =
    Functor[F].map(container)(_ * 10)

  import cats.instances.option._
  import cats.instances.try_._

  //---------------------------------------------------------------------------
  // TODO 1. Define your own functor for a binary tree
  // hint: define an object that extends Functor[Tree]
  sealed trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree

  // Instructor recommends that we use the following "smart constructors"
  // rather than case class constructors for leaf and branch. We are
  // reifing Tree. Here's the companion object.
  object Tree {
    // "smart" constructors, as return type is Tree[T] but RHS returns their subclass.
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] =
      Branch(value, left, right)

    given treeFunctor: Functor[Tree] with {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Leaf(v) => Leaf(f(v))
          case Branch[A] (v, l, r) => Branch (f (v), map (l) (f), map (r) (f) )
        }
    }
  }

  // My own exploration:
  // Implement the tree with an enum instead of sealed trait. Call it BinTree
  // to distinguish it from Tree. This solution is more concise and elegant.
  // No more need to pass the [A] parameter to Branch while pattern matching
  // in the map function; no need to create "smart" constructors.
  enum BinTree[+T] {
    case Leaf(value: T)
    case Branch(value: T, left: BinTree[T], right: BinTree[T])
  }

  given binTreeFunctor: Functor[BinTree] with {
    override def map[A, B](fa: BinTree[A])(f: A => B): BinTree[B] =
      fa match {
        case BinTree.Leaf(v)          => BinTree.Leaf(f(v))
        case BinTree.Branch(v, l, r)  => BinTree.Branch(f(v), map(l)(f), map(r)(f))
      }
  }

  //---------------------------------------------------------------------------
  import cats.syntax.functor._
  val tree: BinTree[Int] =
    BinTree.Branch(
      40,
      BinTree.Branch(5, BinTree.Leaf(10), BinTree.Leaf(30)),
      BinTree.Leaf(20))

  val incrementedTree = tree map (_ + 1)

  // TODO 2: Write a shorter version of the do10x method using extension methods
  def do10x_v3[F[_] : Functor](container: F[Int]): F[Int] = container map (_ * 10)

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(42)))
    println(do10x(Try(35)))

    // test exercise 1
    // Before defining the "smart constructors" for leaf and branch,
    // we had to pass the [Tree] parameter to the do10x method because if we
    // don't the code doesn't compile, as the compiler cannot find a given
    // instance of Functor[Branch] since Cat's typeclasses -- including Functor
    // are invariant.
    // println(do10x[Tree](Branch(3, Leaf(1), Leaf(2))))

    // After defining the smart constructors, we can say Tree.branch, Tree.leaf, etc.
    println(do10x(Tree.branch(3, Tree.leaf(1), Tree.leaf(2))))

    // Try it with the tree defined as an enum instead of trait:
    println(do10x(BinTree.Branch(3, BinTree.Leaf(1), BinTree.Leaf(2))))

  }
}
