package com.abhinab.hackerank.welcomechallenge

import java.io._

import scala.io._

object CountingValley {

}

object Result {

  /*
   * Complete the 'countingValleys' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts following parameters:
   *  1. INTEGER steps
   *  2. STRING path
   */

  def countingValleys(steps: Int, path: String): Int = {
    // Write your code here
    var valleys = 0
    var seaLevel = 0
    path.foreach(c => {
      if(c == 'D') {
        seaLevel -= 1
      } else {
        seaLevel += 1
      }
      if(seaLevel == 0 && c == 'U') {
        valleys += 1
      }
    })
    valleys
  }

}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val steps = StdIn.readLine.trim.toInt

    val path = StdIn.readLine

    val result = Result.countingValleys(steps, path)

    printWriter.println(result)

    printWriter.close()
  }
}
