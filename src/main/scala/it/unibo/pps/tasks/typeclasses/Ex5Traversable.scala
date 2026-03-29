package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def traverse[A](t: T[A])(f: A => Unit): Unit

  given Traversable[Sequence] with
    override def traverse[A](s: Sequence[A])(f: A => Unit): Unit = s match
      case Cons(h, t) =>
        f(h)
        traverse(t)(f)
      case _ =>

  given Traversable[Optional] with
    override def traverse[A](o: Optional[A])(f: A => Unit): Unit = o match
      case Optional.Just(a) => f(a)
      case _ =>


  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = summon[Traversable[Sequence]].traverse(seq)(log)

  
