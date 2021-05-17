package com.abhinab.hackerank.array

import java.io._

import scala.collection.generic.MutableMapFactory
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object HourglassSum {







  // Complete the hourglassSum function below.
  def hourglassSum(arr: Array[Array[Int]]): Int = {
    val sums = ListBuffer[ListBuffer[Int]]()
    for(i <- 0 until arr.length - 2){
      var ab = ListBuffer[Int]()
      for(j <- 0 until arr.length - 2) {
        val sum = arr(i)(j) + arr(i)(j + 1) + arr(i)(j + 2) + arr(i + 1)(j + 1) + arr(i + 2)(j) + arr(i + 2)(j + 1) + arr(i + 2)(j + 2)
        ab += sum
      }
      sums += ab
    }
    println(sums)
    val maxSums = sums.map(_.sorted).map(x => x(x.length - 1))
    println(maxSums)
    maxSums(maxSums.length - 1)
  }

  def main(args: Array[String]) {
    val t = 2
    val pw = new PrintWriter(sys.env("OUTPUT_PATH"))
    for (tItr <- 1 to 1) {
      //val n = stdin.readLine.trim.toInt
      //val q = Array(2,1,5,3,4)

      //val q = Array(2,5,1,3,4)
      //val q = "1 2 5 3 7 8 6 4".split(" ").map(_.toInt)
      //val q = "5 1 2 3 7 8 6 4".split(" ").map(_.toInt)
      val  q = "4 3 1 2".split(" ").map(_.toInt)
      //println(minimumSwaps(q))
    }

    /*val ar = Array(Array(1,1,1,0,0,0), Array(0,1,0,0,0,0), Array(1,1,1,0,0,0), Array(0,9,2,-4,-4,0),Array(0,0,0,-2,0,0),Array(0,0,-1,-2,-4,0))
    val result = hourglassSum(ar)
    println(ar.map(_.length))
    println(result)*/

    //printWriter.println(result)

    //printWriter.close()
  }
}
