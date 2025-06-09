package bl.unused

object types {
  opaque type ->>[K, V] = V

  def label[K]: [V] => V => (K ->> V) = [V] => v => v

  extension[K, V](@annotation.nowarn("msg=unused") kv: K ->> V) def label(using k: ValueOf[K]): K = k.value
}

object foo {
  // The `label` extension method is not used anywhere, but the standard `label` method is used
  // This test confirms that both are considered used because of that
  export types.{->>, label}
}

import foo.*

@annotation.nowarn("msg=unused") val i: "i" ->> Int = label["i"](1)
