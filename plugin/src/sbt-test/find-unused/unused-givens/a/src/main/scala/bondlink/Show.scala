package bondlink

trait Show[A]

object Show {
  given str: Show[String] = new Show[String] {}
  given int: Show[Int] = new Show[Int] {}
  given bool: Show[Boolean] = new Show[Boolean] {}
}
