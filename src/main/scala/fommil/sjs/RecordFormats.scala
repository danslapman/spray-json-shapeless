package fommil.sjs

import spray.json._
import shapeless._
import shapeless.labelled._

/**
 * Generic marshallers for extensible records
 */
object RecordFormats {
  /**
   * Formats HNil
   */
  implicit val hNilWriter: JsonWriter[HNil] = (_: HNil) => JsObject()

  /**
   * Formats HList representing extensible record
   */
  implicit def recordWriter[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hWriter: Lazy[JsonWriter[H]],
    tWriter: JsonWriter[T]): JsonWriter[FieldType[K, H] :: T] =
    (hl: FieldType[K, H] :: T) => {
      JsObject(
        JsObject(witness.value.name -> hWriter.value.write(hl.head)).fields ++
          tWriter.write(hl.tail).asInstanceOf[JsObject].fields
      )
    }

  /**
   * Reads HNil
   */
  implicit val hNilReader: JsonReader[HNil] =
    (json: JsValue) => json match {
      case JsObject(_) => HNil
      case _           => deserializationError("HNil must be represented as empty object")
    }

  /**
   * Extracts extensible record with given type
   */
  implicit def recordReader[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hReader: Lazy[JsonReader[H]],
    tReader: JsonReader[T]): JsonReader[FieldType[K, H] :: T] =
    (json: JsValue) => {
      val fieldName = witness.value.name
      json match {
        case jso: JsObject =>
          jso.getFields(fieldName).headOption match {
            case Some(jsValue) =>
              val hv = hReader.value.read(jsValue)
              val tv = tReader.read(jso)
              field[K](hv) :: tv
            case None =>
              deserializationError(s"Field not found", fieldNames = fieldName :: Nil)
          }
        case _ =>
          deserializationError(s"${json.getClass} can't be deserialized into record type")
      }
    }
}
