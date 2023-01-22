package uk.co.drnaylor.shapeless

import play.api.libs.json._
import shapeless._
import shapeless.labelled.{FieldType, field}

object ShapelessReads {
  def apply[A](r: => Reads[A]): ShapelessReads[A] = new ShapelessReads[A] {
    override lazy val reads: Reads[A] = r
  }
}

/** Wrapper trait for [[Reads]] instances
  *
  * This wrapper trait exists to ensure that we don't accidentally expose an implicit [[Reads]]
  * instance for every known type via [[ShapelessJsonDecoding.genericReads]].
  *
  * @tparam A
  *   The type of [[Reads]] to wrap
  */
trait ShapelessReads[A] {

  def reads: Reads[A]

}

/** Trait that enables shapeless assisted [[Reads]] for classes with typeclass [[P]].
  *
  * A class or object that wishes to read Json to a given set of models that have the associated
  * typeclass implicitly in scope should inherit this trait and use [[Json.fromJson]] as normal.
  *
  * @tparam P
  *   the typeclass type that each model requires in order to be processed by this trait
  */
trait ShapelessJsonDecoding[P[_]] {

  /** Reads an HNil from nothing.
    */
  implicit lazy val hnilShapelessWrites: ShapelessReads[HNil] = ShapelessReads(
    Reads.pure(HNil)
  )

  /** Supplies the [[Reads]] for a given Generic representation, where the head is not an
    * [[Option]].
    *
    * This will be selected for all non-optionals that have a [[Reads]] for the containing type.
    *
    * @param witness
    *   The type witness, which allows for the name of the type to be extracted.
    * @param headReads
    *   The reader for the head
    * @param tailReads
    *   The reader for the tail, which is an [[HList]]
    * @tparam K
    *   The [[Symbol]] that represents the field name, extracted via the [[Witness]]
    * @tparam H
    *   The type of the head of the list
    * @tparam T
    *   The type of the tail of the list, which is an [[HList]]
    * @return
    *   The appropriate [[Reads]]
    */
  implicit def hlistShapelessReads[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headReads: Reads[H],
      tailReads: Lazy[ShapelessReads[T]]
  ): ShapelessReads[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    val tReads    = tailReads.value.reads
    ShapelessReads {
      case jsObject: JsObject =>
        for {
          head <- (jsObject \ fieldName).validate[H].map(field[K].apply)
          tail <- tReads.reads(jsObject)
        } yield head :: tail
      case x => JsError(s"Expecting an object, got $x")
    }
  }

  /** Supplies the [[Reads]] for a given Generic representation, where the head is an [[Option]].
    *
    * This will be selected for all non-optionals that have a [[Reads]] for the containing type.
    *
    * @param witness
    *   The type witness, which allows for the name of the type to be extracted.
    * @param headReads
    *   The reader for the head
    * @param tailReads
    *   The reader for the tail, which is an [[HList]]
    * @tparam K
    *   The [[Symbol]] that represents the field name, extracted via the [[Witness]]
    * @tparam H
    *   The type of the head of the list
    * @tparam T
    *   The type of the tail of the list, which is an [[HList]]
    * @return
    *   The appropriate [[Reads]]
    */
  implicit def hlistShapelessOptionReads[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      headReads: Reads[H],
      tailReads: Lazy[ShapelessReads[T]]
  ): ShapelessReads[FieldType[K, Option[H]] :: T] = {
    val fieldName = witness.value.name
    val tReads    = tailReads.value.reads
    ShapelessReads {
      case jsObject: JsObject =>
        for {
          headRead <- (__ \ fieldName).readNullable[H].reads(jsObject)
          head     <- JsSuccess(field[K].apply(headRead))
          tail     <- tReads.reads(jsObject)
        } yield head :: tail
      case x => JsError(s"Expecting an object, got $x")
    }
  }

  /** Creates a Shapeless assisted [[Reads]] that backs an object of type [[A]].
    *
    * @param labelledGeneric
    *   The [[LabelledGeneric.Aux]] that un-generifies a supplied [[A]]
    * @param actualReads
    *   The wrapped [[Reads]] that creates Json from the generified [[A]]
    * @tparam A
    *   The type to convert
    * @tparam L
    *   The generified type to convert
    * @return
    *   The wrapped [[ShapelessReads]]
    */
  implicit def genericReads[A, L](implicit
      labelledGeneric: LabelledGeneric.Aux[A, L],
      actualReads: Lazy[ShapelessReads[L]]
  ): ShapelessReads[A] =
    ShapelessReads {
      actualReads.value.reads.reads(_).map(labelledGeneric.from)
    }

  /** Constructs [[Reads]] for models that have a valid typeclass [[P]] as evidence.
    *
    * @param sw
    *   The [[ShapelessReads]] that wraps this [[Reads]]
    * @tparam A
    *   The type to convert, which must have a typeclass [[P]]
    * @return
    *   The [[Reads]]
    */
  implicit def asReads[A: P](implicit sw: ShapelessReads[A]): Reads[A] =
    sw.reads

}
