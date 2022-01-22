package me.chuwy.otusbats
import me.chuwy.otusbats.Monad._

import org.scalatest.flatspec.AnyFlatSpec

class MonadTest extends AnyFlatSpec {

	"check OptionMonad " should "ok" in {
		assert(Monad[Option].flatMap(Option(1))(a => Option(a.toString)) === Some("1"))
		assert(Monad[Option].flatMap(Option(1))(_ => None) === None)
		assert(Monad[Option].flatMap(Option(null))(a => Option(a.toString)) === None)

		assert(Monad[Option].point(1) === Some(1))
		assert(Monad[Option].point(null) === None)

		assert(Monad[Option].flatten(Option(Option(12.3))) === Option(12.3))
		assert(Monad[Option].flatten(Option(None)) === None)
		assert(Monad[Option].flatten(None) === None)

		assert(Monad[Option].map(Option(1))(_.toString) === Some("1"))
		assert(Monad[Option].map(Option(null))(_.toString) === None)
	}
}