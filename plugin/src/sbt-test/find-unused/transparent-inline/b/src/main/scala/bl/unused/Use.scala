package bl.unused

object Use {
  val n = 5

  val all = (
    Defs.plain(),
    Defs.withParam(1),
    Defs.callSiteOnly(n),
    Defs.viaMacro,
    Defs.viaQuotedArg[Int],
    Defs.Nested.nested(),
  )
}
