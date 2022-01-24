package me.chuwy.otusbats

import scala.util.Try

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(a => a)
}

object Monad {

  // Summoner (apply)
  implicit def apply[F[_]](implicit ev: Monad[F]): Monad[F] = ev

  implicit val OptionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def point[A](a: A): Option[A] = Option(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val ListMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val FutureMonad: Monad[Try] = new Monad[Try] {
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    override def point[A](a: A): Try[A] = Try(a)

    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
  }
}
