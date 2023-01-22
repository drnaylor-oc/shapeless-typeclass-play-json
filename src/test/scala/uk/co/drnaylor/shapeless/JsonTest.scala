package uk.co.drnaylor.shapeless

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsError, Json}

class JsonTest extends AnyFreeSpec with Matchers {

  trait JsonConversionTypeClass[A]

  implicit val tc1: JsonConversionTypeClass[Parent]     = new JsonConversionTypeClass[Parent] {}
  implicit val tc1a: JsonConversionTypeClass[ParentOpt] = new JsonConversionTypeClass[ParentOpt] {}
  implicit val tc2: JsonConversionTypeClass[Child]      = new JsonConversionTypeClass[Child] {}

  case class Child(d: String, e: Int)
  case class Parent(a: String, b: Int, c: Child)
  case class ParentOpt(a: String, b: Int, c: Option[Child])

  "test writes" - new ShapelessJsonEncoding[JsonConversionTypeClass] {
    "test" in {
      Json.obj("a" -> "a", "b" -> 1, "c" -> Json.obj("d" -> "d", "e" -> 2)) mustBe Json.toJson(
        Parent("a", 1, Child("d", 2))
      )
    }

    "test optional" in {
      Json.obj("a" -> "a", "b" -> 1, "c" -> Json.obj("d" -> "d", "e" -> 2)) mustBe Json.toJson(
        ParentOpt("a", 1, Some(Child("d", 2)))
      )
    }

    "test optional with None" in {
      Json.obj("a" -> "a", "b" -> 1) mustBe Json.toJson(
        ParentOpt("a", 1, None)
      )
    }
  }

  "test reads" - new ShapelessJsonDecoding[JsonConversionTypeClass] {
    "test" in {
      Json
        .obj("a" -> "a", "b" -> 1, "c" -> Json.obj("d" -> "d", "e" -> 2))
        .as[Parent] mustBe Parent("a", 1, Child("d", 2))
    }

    "testNoneOptional" in {
      Json
        .obj("a" -> "a", "b" -> 1)
        .as[ParentOpt](asReads[ParentOpt]) mustBe ParentOpt("a", 1, None)
    }

    "testSomeOptional" in {
      Json
        .obj("a" -> "a", "b" -> 1, "c" -> Json.obj("d" -> "d", "e" -> 2))
        .as[ParentOpt](asReads[ParentOpt]) mustBe ParentOpt("a", 1, Some(Child("d", 2)))
    }

    "test invalid input should fail" in {
      Json
        .obj("a" -> "a", "b" -> 1, "c" -> "nope")
        .validate[ParentOpt](asReads[ParentOpt]) mustBe a[JsError]
    }
  }

}
