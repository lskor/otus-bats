package me.chuwy.otusbats
import me.chuwy.otusbats.Monad._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Try
import scala.util.Success
import scala.util.Failure

class MonadTest extends AnyFlatSpec {

	"check OptionMonad " should "ok" in {
		assert(Monad[Option].flatMap(Option(1))(a => Option(a.toString)) === Some("1"))
		assert(Monad[Option].flatMap(Option(1))(_ => None) === None)
		assert(Monad[Option].flatMap(Option(null))(a => Option(a.toString)) === None)

		assert(Monad[Option].point(1) === Some(1))
		assert(Monad[Option].point(null) === None)

		assert(Monad[Option].flatten(Option(Option(12.3))) === Some(12.3))
		assert(Monad[Option].flatten(Option(None)) === None)
		assert(Monad[Option].flatten(None) === None)

		assert(Monad[Option].map(Option(1))(_.toString) === Some("1"))
		assert(Monad[Option].map(Option(null))(_.toString) === None)
	}

	"check ListMonad " should "ok" in {
		assert(Monad[List].flatMap(List(1, 2, 3))(a => List(a + 1)) === List(2, 3, 4))
		assert(Monad[List].point(3) === List(3))
		assert(Monad[List].flatten(List(List(1, 2), List(3, 4))) === List(1, 2, 3, 4))
		assert(Monad[List].map(List(1, 2))(_ + 1) === List(2, 3))
	}

	"check TryMonad " should "ok" in {
		val exception = new NullPointerException("uups!")

		assert(Monad[Try].flatMap(Try(1))(a => Try(a + 1)) === Success(2))
		assert(Monad[Try].flatMap(Try(1))(_ => Try(throw exception)) === Failure(exception))

		assert(Monad[Try].point(3) === Success(3))

		assert(Monad[Try].flatten(Try(Try(4))) === Success(4))
		assert(Monad[Try].flatten(Try(Try(throw exception))) === Failure(exception))

		assert(Monad[Try].map(Try(1))(_ + 1) === Success(2))
		assert(Monad[Try].map(Try(1))(_ => throw exception) === Failure(exception))
	}
}