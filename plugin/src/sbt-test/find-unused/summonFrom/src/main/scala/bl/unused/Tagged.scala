package bl.unused

import io.circe.Decoder
import io.circe.derivation.{Configuration, ConfiguredDecoder}

trait Show[A] {
  def show(a: A): String
}

object Tagged {
  opaque type Name = String

  object Name {
    // Only circe's derivation macro summons this, and circe is compiled with Scala 3.3
    inline given nameDecoder(using d: Decoder[String]): Decoder[Name] = d
    inline given nameShow: Show[Name] = (n: Name) => n
  }
}

given Configuration = Configuration.default

case class Person(name: Tagged.Name) derives ConfiguredDecoder

val personDecoder = Decoder[Person]
