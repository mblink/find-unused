package bl.unused

// Only `base` is called, but its expansion is the constant positioned in `base`, which a call to `wrapper` expands to
// too, so `wrapper` can't be told apart from it and is kept as well
object DirectConstant {
  transparent inline def base(x: Int): Int = x + 1
  transparent inline def wrapper(): Int = base(1)
}
