package bl.unused

// Nothing calls the two forwarders the export generates for these overloads, which share the selector's position, so
// they're listed once. The forwarders call the overloads, so those aren't reported
object Padding {
  def pad(s: String): String = s
  def pad(s: String, n: Int): String = s.padTo(n, ' ')
}

object PaddingApi {
  export Padding.pad
}
