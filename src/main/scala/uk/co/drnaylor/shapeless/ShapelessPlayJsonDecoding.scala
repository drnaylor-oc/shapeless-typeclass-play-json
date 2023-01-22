package uk.co.drnaylor.shapeless

import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsObject, JsSuccess, JsUndefined, Reads, __}
import shapeless._
import shapeless.labelled.{FieldType, field}

object ShapelessReads {
  def apply[A](r: => Reads[A]): ShapelessReads[A] = new ShapelessReads[A] {
    override lazy val reads: Reads[A] = r
  }
}
trait ShapelessReads[A] {

  def reads: Reads[A]

}

trait ShapelessJsonDecoding[P[_]] {

  implicit lazy val hnilShapelessWrites: ShapelessReads[HNil] = ShapelessReads(
    Reads.pure(HNil)
  )

  implicit def hlistShapelessReads[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headReads: Reads[H],
      tailReads: Lazy[ShapelessReads[T]]
  ): ShapelessReads[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    val tReads    = tailReads.value.reads
    ShapelessReads { case jsObject: JsObject =>
      for {
        head <- (jsObject \ fieldName).validate[H].map(field[K].apply)
        tail <- tReads.reads(jsObject)
      } yield head :: tail
    }
  }

  implicit def hlistShapelessOptionReads[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headReads: Reads[H],
      tailReads: Lazy[ShapelessReads[T]]
  ): ShapelessReads[FieldType[K, Option[H]] :: T] = {
    val fieldName = witness.value.name
    val tReads    = tailReads.value.reads
    ShapelessReads { case jsObject: JsObject =>
      for {
        headRead <- (__ \ fieldName).readNullable[H].reads(jsObject)
        head     <- JsSuccess(field[K].apply(headRead))
        tail     <- tReads.reads(jsObject)
      } yield head :: tail
    }
  }

  implicit def genericReads[A, L](implicit
      labelledGeneric: LabelledGeneric.Aux[A, L],
      actualReads: Lazy[ShapelessReads[L]]
  ): ShapelessReads[A] =
    ShapelessReads {
      actualReads.value.reads.reads(_).map(labelledGeneric.from)
    }

  implicit def asReads[A: P](implicit sw: ShapelessReads[A]): Reads[A] =
    sw.reads

}
