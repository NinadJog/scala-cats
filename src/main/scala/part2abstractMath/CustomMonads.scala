package part2abstractMath

import scala.annotation.tailrec

/*
  - Implement the pure and flatMap methods
  - The iteration methods need to be stack-safe.
 */
object CustomMonads {

  import cats.Monad
  given optionMonad: Monad[Option] with {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f

    /**
     * tailRecM runs as follows. Apply the function f to a. If the result is an
     * Either containing an A, run the method again and again until the final
     * result of the function applied to the value of type A obtained from the
     * Either is of type B.
     *
     * Make sure that tailRecM does not stack overflow. (In this implementation
     * it doesn't overflow the stack.) This method has to be private or final,
     * since it's tail recursive, otherwise it's a compiler error in Scala 3.3.6.
     *
     * We hardly use this method much in day-to-day programming.
     */
    @tailrec
    final override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None           => None
        case Some(Left(v))  => tailRecM(v)(f) // loop to next iteration
        case Some(Right(b)) => Some(b)        // final result
      }
  }

  /**
   * Why is the monad's tailRec method needed? Monads are used for sequential
   * computations. So we can represent iteration, which is imperative programming,
   * with data structures of this form. Monads have some methods that allow us
   * to iterate, starting with some value. Methods are: iterateForeverM,
   * iterateUntil, iterateWhile, iterateUntilM, iterateWhileM.
   *
   * These methods return a final value after performing iterations embedded
   * as or represented as immutable data structures. When we create our own
   * custom monad, we have to implement the tailRecM method because all the
   * other iteration methods are defined in terms of the tailRecM method.
   */

  //---------------------------------------------------------------------------
  // TODO 1. Define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  given identityMonad: Monad[Identity] with {
    override def pure[A](x: A): Identity[A] = x
    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

    @tailrec
    final override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] =
      f(a) match {
        case Left(v)  => tailRecM(v)(f)
        case Right(b) => b
      }
  }

  //---------------------------------------------------------------------------
  // TODO 2. Define a monad for the following Tree
  enum Tree[+A] {
    case Leaf(value: A)
    case Branch(left: Tree[A], right: Tree[A])
  }

  import cats.syntax.flatMap._
  given treeMonad: Monad[Tree] with {

    override def pure[A](a: A): Tree[A] = Tree.Leaf(a)

    // Replace the A value in each leaf by the tree f(A)
    // This is stack recursive; might crash the stack. TODO make it tail recursive
    override def flatMap[A, B](ta: Tree[A])(f: A => Tree[B]): Tree[B] =
      ta match {
        case Tree.Leaf(a)             => f(a) // replace leaf by f(a), which is a tree
        case Tree.Branch(left, right) => Tree.Branch(left flatMap f, right flatMap f)  // l flatMap f == flatMap(l)(f)
      }


    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

      // stack recursive solution can cause a stack overflow.
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
          case Tree.Leaf(Left(v))       => stackRec(f(v)) // Left is undesirable value, so apply v to it
          case Tree.Leaf(Right(b))      => Tree.Leaf(b)  // done
          case Tree.Branch(left, right) => Tree.Branch(stackRec(left), stackRec(right)) // recurse
      }

      /**
       * Alternative tail recursive solution. At the end the todo list will be
       * empty, the expanded list will contain a few nodes and the done
       * accumulator will contain a single element which will be the result.
      */
      @tailrec
      def tailRec(todo:     List[Tree[Either[A, B]]], // tree nodes that haven't yet been expanded
                  expanded: List[Tree[Either[A, B]]], // expanded is
                  done:     List[Tree[B]]): Tree[B] = { // done is the accumulator

        if todo.isEmpty then done.head  // return the accumulator
        else todo.head match {
          // iterate on the head node until we get a Leaf(Right(_)) or Branch
          case Tree.Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done)

          case Tree.Leaf(Right(b)) => tailRec(todo.tail, expanded, Tree.Leaf(b) :: done)

          // If the node has not been expanded before, add it to expanded and break it
          // down into its constituent left and right parts and put them into the
          // todo list.
          case node @ Tree.Branch(left, right) =>
            if !expanded.contains(node) then // node has not been expanded before
              tailRec(right :: left :: todo, node :: expanded, done)
            else {
              /*
                Since we are returning to a node that has already been expanded,
                we skip it in the to-do list, which means call todo.tail in tailRec.
                We also need to reconstruct two
                nodes out of the done list, create a branch from them, and put
                them back into the done list.
              */
              val newLeft   = done.head
              val newRight  = done.tail.head
              val newBranch = Tree.Branch(newLeft, newRight)
              // Pop newLeft & newRight from done and prepend newBranch to done.
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
        }
      }
      tailRec(List(f(a)), List(), List())
      // stackRec(f(a)) // Older alternative implementation (can cause stack overflow)
    }
  }

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    // Test the tree monad
    import Tree._
    val tree: Tree[Int] = Tree.Branch(Leaf(10), Leaf(20))
    val changedTree = treeMonad.flatMap(tree)(v => Tree.Branch(Leaf(v - 1), Leaf(v + 1)))
    println(changedTree) // Branch(Branch(Leaf(9),Leaf(11)),Branch(Leaf(19),Leaf(21)))
  }
}
