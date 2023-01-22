package uk.co.drnaylor.shapeless

import play.api.libs.json._
import shapeless._
import shapeless.labelled.FieldType

object ShapelessWrites {
  def apply[A](owrites: => OWrites[A]): ShapelessWrites[A] = new ShapelessWrites[A] {
    override lazy val writes: OWrites[A] = owrites
  }
}
trait ShapelessWrites[A] {

  def writes: OWrites[A]

}

trait ShapelessJsonEncoding[P[_]] {

  // ---
  //
  // The following will cause options to not be selected in a given case as we end up with too many implicits
  // to choose from for Option.
  //
  // Adapted from https://blog.rockthejvm.com/anti-implicits/
  trait =![A]
  implicit def neq[A]: =![A]                = null
  implicit def notOption[A]: =![Option[A]]  = null
  implicit def notOption2[A]: =![Option[A]] = null
  // ---

  implicit lazy val hnilShapelessWrites: ShapelessWrites[HNil] = ShapelessWrites(
    OWrites(_ => JsObject.empty)
  )

  // This will be selected for all non-optionals
  implicit def hlistShapelessWrites[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headWrites: Writes[H],
      tailWrites: Lazy[ShapelessWrites[T]],
      evidence: =![H]
  ): ShapelessWrites[FieldType[K, H] :: T] = {
    val fieldName   = witness.value.name
    val tailOwrites = tailWrites.value.writes
    ShapelessWrites { case h :: t =>
      Json.obj(fieldName -> headWrites.writes(h)) ++ tailOwrites.writes(t)
    }
  }

  // This will be selected for all optionals (don't need to play with implicits for this, we specify the Option[H] writes directly)
  implicit def hlistShapelessWritesOpt[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headWrites: Writes[Option[H]],
      tailWrites: Lazy[ShapelessWrites[T]]
  ): ShapelessWrites[FieldType[K, Option[H]] :: T] = {
    val fieldName   = witness.value.name
    val tailOwrites = tailWrites.value.writes
    ShapelessWrites { case h :: t =>
      val hw = headWrites.writes(h) match {
        case JsNull => JsObject.empty
        case x      => Json.obj(fieldName -> x)
      }
      hw ++ tailOwrites.writes(t)
    }
  }

  implicit def genericWrites[A, L](implicit
      genericWrites: LabelledGeneric.Aux[A, L],
      actualWrites: Lazy[ShapelessWrites[L]]
  ): ShapelessWrites[A] =
    ShapelessWrites {
      actualWrites.value.writes.contramap(genericWrites.to)
    }

  implicit def asWrites[A: P](implicit sw: ShapelessWrites[A]): Writes[A] =
    sw.writes
}
