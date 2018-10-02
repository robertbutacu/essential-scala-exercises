package code

// Exercise:
//
// In the body of "Expressions" implement the following methods:
//
// - greet
//   - accepts a String name as a parameter
//   - returns a friendly greeting (e.g. "Hello name")
//
// - isPalindrome
//   - accepts a String as a parameter
//   - returns true if it is a palindrome, false otherwise
//
//   Tip: String has a .reverse method: "hello".reverse == "olleh"
//
// - factorial
//   - accepts an Int n as a parameter
//   - returns the factorial of n (1 * 2 * 3 * ... * n)
//
//   Tip: Try multiplying the numbers in reverse order, from n to 1


object Expressions extends Exercise {

  def greet(s: String): String = "Hello " + s

  def isPalindrome(s: String): Boolean = s.reverse == s

  def factorial(n: Int): Int = {
    require(n >= 0)
    (1 to n).product
  }

  def reverseFactorial(n: Int): Int = {
    require(n >= 0)
    (n to 1 by -1).product
  }
}
