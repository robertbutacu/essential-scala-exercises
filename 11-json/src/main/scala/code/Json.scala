package code

// Exercise :
//
// Design and implement:
// - a JsonValue ADT representing a JSON datum
// - a Json object containing a stringify method
//   that renders a JsonValue as a compact string

sealed trait JsonValue
case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
case class JsonArray(items: List[JsonValue]) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case class JsonNumber(value: Double) extends JsonValue
case class JsonString(value: String) extends JsonValue
case object JsonNull extends JsonValue

object Json {
  def stringify(json: JsonValue): String =
    json match {
      case JsonObject(fields) => fields.map(stringifyField).mkString("{", ",", "}")
      case JsonArray(items)   => items.map(stringify).mkString("[", ",", "]")
      case JsonBoolean(value) => value.toString
      case JsonNumber(value)  => value.toString
      case JsonString(value)  => quoteString(escapeString(value))
      case JsonNull           => "null"
    }

  private def stringifyField(field: (String, JsonValue)): String = {
    val (name, value) = field
    quoteString(escapeString(name)) + ":" + stringify(value)
  }

  private def quoteString(str: String): String =
    s""""$str""""

  private def escapeString(str: String): String =
    str
      .replaceAll("""\"""", """\\""")
      .replaceAll(""""""", """\"""")

}
