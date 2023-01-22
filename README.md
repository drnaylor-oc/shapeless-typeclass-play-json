# Encoding and Decoding Json using Shapeless and Typeclasses

A project I'm working on needs to convert between XML and Json models, and we use scalaxb to generate the models.
This quick project shows that we can use a typeclass to mark models as being eligible for shapeless generification,
and use it to convert the models to/from Json without having to specify them all using `Json.format[A]`.

The classes that invoke the conversion should inherit the traits `ShapelessPlayJsonEncoding` and `ShapelessPlayJsonDecoding`,
specifying the typeclass as the type parameter. You can then use `Json.toJson[A]` and `Json.fromJson()` normally.

Note this is a compile-time thing, rather than a runtime thing, so you may experience increased compile times. 