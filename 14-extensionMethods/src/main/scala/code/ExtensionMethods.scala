package code

// Exercise:
//
// - create an implicit class HelloOps that adds this method to String:
//   - hello
//     - accepts a String name as a parameter
//     - returns a greeting for name
//
// - create an implicit class TimesOps that adds this method to Int:
//   - times
//     - accepts a function from Int to A as a parameter
//     - calls the function N times, collecting the results into a list


object ExtensionMethods extends Exercise {
  implicit class StringOps(s: String) {
    def hello: String = "Hello " + s
  }

  implicit class TimesOps(n: Int) {
    def times[A](f: Int => A) = f(n)
  }



  override def main(args: Array[String]): Unit = {
    println("HELLO")
    println("Dave".hello)
    println()
    println("1" + 2)
    println(1 + "2")

     println("TIMES")
     println(3.times(n => s"It's the number $n!"))
     println()
  }
}
