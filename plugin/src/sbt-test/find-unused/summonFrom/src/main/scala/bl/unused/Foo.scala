package bl.unused

import io.circe.Decoder

case class Foo(int: Int) derives Decoder
case class Bar(foo: Foo) derives Decoder

val barDecoder = Decoder[Bar]
