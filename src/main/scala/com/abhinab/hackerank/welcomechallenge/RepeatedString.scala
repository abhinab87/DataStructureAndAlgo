package com.abhinab.hackerank.welcomechallenge

object RepeatedString {
  // Complete the repeatedString function below.
  def repeatedString(s: String, n: Long): Long = {
    if(n%s.length == 0 )
      s.count(_ == 'a').toLong * (n/s.length)
    else{
      val remainder = n%s.length
      s.count(_ == 'a').toLong * (n/s.length) + (s.substring(0,remainder.toInt).count(_ == 'a'))
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn
    //val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))
    val s = "abc"
    val n = 20
    val result = repeatedString(s, n)
    println(result)
    //printWriter.println(result)
    //printWriter.close()
  }
}
