package Basics


object Basics extends App{
  def lcm(a: Int, b: Int): Int = (a / gcd(a, b)) * b
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  println(s"lcm(83, 33) == 2739\t${lcm(83,33) == 2739}")

}
