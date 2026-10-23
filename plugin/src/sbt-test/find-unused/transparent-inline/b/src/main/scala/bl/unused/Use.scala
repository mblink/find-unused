package bl.unused

object Use {
  val n = 5

  val all = (
    Defs.plain(),
    Defs.withParam(1),
    Defs.callSiteOnly(n),
    Defs.viaMacro,
    Defs.viaQuotedArg[Int],
    ConstantMacros.lifted,
    DirectConstant.base(2),
    ConstantMacros.reflected,
    Defs.Nested.nested(),
    Defs.compound(n.toString),
    Defs.nestedCall(n.toString),
    Defs.constantCall(),
    Defs.deepConstantCall(),
    Defs.otherConstantCall(),
    Defs.withLocalClass(),
    // The expansion of a method it inherits doesn't refer to the object it's called on
    WithHelpers,
    WithHelpers.inTrait(),
    topLevel(),
  )
}
