sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  // 3.7
  // No. At its current implementation, fold right needs to traverse through then entire list

  // 3.8
  // You get the original list back. List construction is implemented exactly like fold right.

  // 3.9
  def length[A](as: List[A]): Int = {

    foldRight(as, 0)((_, count) => count + 1)

  }

  // 3.10
  // z is accumulator
  // f is the function to perform on the list
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // 3.11
  def sum_foldLeft(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def product_foldLeft(l: List[Int]): Int = {
    foldLeft(l, 1)(_ * _)
  }

  def length_foldLeft(l: List[Int]): Int = {
    foldLeft(l, 0)((count, _) => count + 1)
  }

  // 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, el) => Cons(el, acc))
  }

  // 3.13
  def foldLeftWithFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B {
    foldRight(as, z)((a: A, b: B) => f(b, a))
  }

  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B {
    foldLeft(as, z)((b: B, a: A) => f(a, b))
  }

  // 3.14
  def append(l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((a: A, b: List[A]) => Cons(a, b))
  }

  // 3.15
  def concat(l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  // 3.16
  def plusOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((item: Int, acc: List[Int]) => Cons(item + 1, acc))
  }

  // 3.17
  def toStringList(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((item: Double, acc: List[String]) => Cons(item.toString, acc))
  }

  // 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((item: A, acc: List[B]) => Cons(f(item), acc))
  }

  // 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((item: A, acc: List[A]) => { if (f(item)) Cons(item, acc) else acc })
  }

  // 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  // 3.21
  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(item: A => { if (f(A)) List(item) else Nil })
  }

  // 3.22
  def zipAdd(l1: list[Int], l2: list[Int]): List[Int] = (l1, l2) match {

    case (_, Nil) => l1
    case (Nil, _) => l2
    case ( Cons(head1, tail1), Cons(head2, tail2) ) => Cons( (head1 + head2), zipAdd(tail1, tail2))

  }

  // 3.23
  def zipWith[A, B, C](list1: A, list2: B)(f(A, B) => C): List[C] = (list1, list2) {

    case (_, Nil) => list1 // I have this wrong, answer key says Nil, why tho is this an implementation thing?
    case (Nil, _) => list2
    case ( Cons(head1, tail1), Cons(head2, tail2) ) => Cons( f(head1, head2), zipWith(tail1, tail2)(f) )

  }

}