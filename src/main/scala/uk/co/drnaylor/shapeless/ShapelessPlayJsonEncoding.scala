package uk.co.drnaylor.shapeless

import play.api.libs.json.{JsObject, Json, OWrites, Writes}
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

  implicit lazy val hnilShapelessWrites: ShapelessWrites[HNil] = ShapelessWrites(
    OWrites(_ => JsObject.empty)
  )

  implicit def hlistShapelessWrites[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headWrites: Lazy[Writes[H]],
      tailWrites: Lazy[ShapelessWrites[T]]
  ): ShapelessWrites[FieldType[K, H] :: T] = {
    val fieldName   = witness.value.name
    val tailOwrites = tailWrites.value.writes
    ShapelessWrites { case h :: t =>
      Json.obj(fieldName -> headWrites.value.writes(h)) ++ tailOwrites.writes(t)
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
