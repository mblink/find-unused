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
    Defs.compound(n.toString),
    Defs.nestedCall(n.toString),
    // The expansion doesn't refer to the object it's called on
    WithHelpers,
    WithHelpers.inTrait(),
    topLevel(),
  )
}
