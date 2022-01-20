package me.chuwy.otusbats

import org.scalatest.flatspec.AnyFlatSpec
import me.chuwy.otusbats.Show._

class ShowTest extends AnyFlatSpec {

	"check fromJvm constructor" should "ok" in  {
		assert( Show.fromJvm.show(2) === "2")
		assert(Show.fromJvm.show(true) === "true")
	}

	"check summoner and instances " should "ok" in  {
		assert(Show[Int].show(9) === "9")
		assert(Show[String].show("bye") === "bye")
		assert(Show[Boolean].show(true) === "true")
		assert(Show[List[Int]].show(List(1, 2, 3)) === "List[1, 2, 3]")
	}

	"check syntax extensions" should "ok" in  {
		assert("9".show === "9")
		assert("bye".show === "bye")
		assert(true.show === "true")
		assert(List(1, 2, 3).show === "List[1, 2, 3]")
	}

	"check mkString_" should "ok" in  {
		assert(Show.mkString_(List(1, 2, 3), "[", "]", ", ") === "[1, 2, 3]")
		assert(Show.mkString_(List[Int](), "[", "]", ", ") === "[]")
	}
}