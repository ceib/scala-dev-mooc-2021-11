package module2

import module2.higher_kinded_types.optBindable

object homework_hkt_impllicts extends App {

  /**
   *
   * Доработать сигнатуру tupleF и реализовать его
   * По итогу должны быть возможны подобные вызовы
   * val r1 = println(tupleF(optA, optB))
   * val r2 = println(tupleF(list1, list2))
   *
   */
  //декартово произведение
  def tupleF[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F,B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  //аналог zip
  def tupleZipF[F[_], A, B](fa: Bindable[F, A], fb: F[B]):F[(A, B)]=
    fa.zip(fb)

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]

    def flatMap[B](f: A => F[B]): F[B]

    def zip[B](e: F[B]): F[(A, B)]
  }

  object Bindable {

    implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
      override def map[B](f: A => B): Option[B] = opt.map(f)

      override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)

      override def zip[B](bs: Option[B]): Option[(A, B)] = opt.zip(bs)
    }

    implicit def listBindable[A](opt: List[A]): Bindable[List, A] = new Bindable[List, A] {
      override def map[B](f: A => B): List[B] = opt.map(f)

      override def flatMap[B](f: A => List[B]): List[B] = opt.flatMap(f)

      override def zip[B](bs: List[B]): List[(A, B)] = opt.zip(bs)
    }
  }

  val someVal = print("It is just a value")
  println(someVal)
  println(someVal)

  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)
  val optC: Option[String] = Some("Hi!")
  val optD: Option[String] = None

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)
  val list3 = List("one", "two", "three")
  val list4 = List(None, Some("two"), None)

  import Bindable._

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))
  val r1x = println(tupleF(optA, optC))
  val r2x = println(tupleF(list1, list3))

  val r3 = println(tupleZipF(optA, optB))
  val r4 = println(tupleZipF(list1, list2))
  val r3x = println(tupleZipF(optA, optC))
  val r3y = println(tupleZipF(optA, optD))
  val r4x = println(tupleZipF(list1, list3))
  val r4y = println(tupleZipF(list1, list4))
}