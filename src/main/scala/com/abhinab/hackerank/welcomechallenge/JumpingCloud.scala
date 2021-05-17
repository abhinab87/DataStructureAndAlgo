package com.abhinab.hackerank.welcomechallenge

object JumpingCloud {
  // Complete the jumpingOnClouds function below.
  def jumpingOnClouds(c: Array[Int]): Int = {
    var current_position = 0
    var number_of_jumps = 0
    val last_cloud_postion = c.length-1
    val last_second_postion = c.length-2

    while (current_position < last_second_postion) {
      if (c(current_position + 2) == 0) {
        current_position += 2
        number_of_jumps += 1
      } else {
        current_position += 1
        number_of_jumps += 1
      }
    }
    if(current_position != last_cloud_postion) {
      current_position += 1
      number_of_jumps += 1
    }
    number_of_jumps
  }

  def main(args: Array[String]) {
    //val stdin = scala.io.StdIn

    //val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    //val n = stdin.readLine.trim.toInt

    //val c = stdin.readLine.split(" ").map(_.trim.toInt)
    val ar = Array(0, 0, 0, 1, 0, 0)
    //val ar = Array(0, 0, 1, 0, 1, 0)
    val result = jumpingOnClouds(ar)

    println(result)

    //printWriter.println(result)

    //printWriter.close()
  }
}
