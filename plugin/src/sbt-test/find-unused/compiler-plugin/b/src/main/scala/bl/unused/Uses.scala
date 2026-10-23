package bl.unused

import Instances.given

object Uses {
  def used: String = Show.show(/~\.one)
  def unused: String = Show.show(true)
}

@main def main(): Unit = println(Uses.used)
