package uk.co.drnaylor.shapeless

import play.api.libs.json._
import shapeless._
import shapeless.labelled.FieldType

object ShapelessWrites {
  def apply[A](owrites: => OWrites[A]): ShapelessWrites[A] = new ShapelessWrites[A] {
    override lazy val writes: OWrites[A] = owrites
  }
}

/** Wrapper trait for [[OWrites]] instances
  *
  * This wrapper trait exists to ensure that we don't accidentally expose an implicit [[OWrites]]
  * instance for every known type via [[ShapelessJsonEncoding.genericWrites]].
  *
  * @tparam A
  *   The type of [[OWrites]] to wrap
  */
trait ShapelessWrites[A] {

  def writes: OWrites[A]

}

/** Trait that enables shapeless assisted [[OWrites]] for classes with typeclass [[P]].
  *
  * A class or object that wishes to write Json for a given set of models that have the associated
  * typeclass implicitly in scope should inherit this trait and use [[Json.toJson]] as normal.
  *
  * @tparam P
  *   the typeclass type that each model requires in order to be processed by this trait
  */
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

  /** The [[Writes]] for an [[HNil]], returning an empty [[JsObject]]
    */
  implicit lazy val hnilShapelessWrites: ShapelessWrites[HNil] = ShapelessWrites(
    OWrites(_ => JsObject.empty)
  )

  /** Supplies the [[Writes]] for a given Generic representation, where the head is not an
    * [[Option]].
    *
    * This will be selected for all non-optionals that have a [[Writes]] for the containing type.
    *
    * @param witness
    *   The type witness, which allows for the name of the type to be extracted.
    * @param headWrites
    *   The writer for the head
    * @param tailWrites
    *   The writer for the tail, which is an [[HList]]
    * @param evidence
    *   Evidence that [[H]] is not an [[Option]]
    * @tparam K
    *   The [[Symbol]] that represents the field name, extracted via the [[Witness]]
    * @tparam H
    *   The type of the head of the list
    * @tparam T
    *   The type of the tail of the list, which is an [[HList]]
    * @return
    *   The appropriate [[Writes]]
    */
  // NON-SCALADOC: This will be selected for all non-optionals
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

  /** Supplies the [[Writes]] for a given Generic representation, where the head is an [[Option]].
    *
    * This will be selected for all optionals that have a [[Writes]] for the containing type.
    *
    * @param witness
    *   The type witness, which allows for the name of the type to be extracted.
    * @param headWrites
    *   The writer for the head, which is an [[Option]]
    * @param tailWrites
    *   The writer for the tail, which is an [[HList]]
    * @tparam K
    *   The [[Symbol]] that represents the field name, extracted via the [[Witness]]
    * @tparam H
    *   The type of the head of the list
    * @tparam T
    *   The type of the tail of the list, which is an [[HList]]
    * @return
    *   The appropriate [[Writes]]
    */
  // NON-SCALADOC: We don't need to play with implicits for this, as we specify the Option[H] writes directly
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

  /** Creates a Shapeless assisted [[Writes]] that backs an object of type [[A]].
    *
    * @param genericWrites
    *   The [[LabelledGeneric.Aux]] that generifies a supplied [[A]]
    * @param actualWrites
    *   The wrapped [[Writes]] that creates Json from the generified [[A]]
    * @tparam A
    *   The type to convert
    * @tparam L
    *   The generified type to convert
    * @return
    *   The wrapped [[ShapelessWrites]]
    */
  implicit def genericWrites[A, L](implicit
      genericWrites: LabelledGeneric.Aux[A, L],
      actualWrites: Lazy[ShapelessWrites[L]]
  ): ShapelessWrites[A] =
    ShapelessWrites {
      actualWrites.value.writes.contramap(genericWrites.to)
    }

  /** Constructs [[Writes]] for models that have a valid typeclass [[P]] as evidence.
    *
    * @param sw
    *   The [[ShapelessWrites]] that wraps this [[Writes]]
    * @tparam A
    *   The type to convert, which must have a typeclass [[P]]
    * @return
    *   The [[Writes]]
    */
  implicit def asWrites[A: P](implicit sw: ShapelessWrites[A]): Writes[A] =
    sw.writes
}
