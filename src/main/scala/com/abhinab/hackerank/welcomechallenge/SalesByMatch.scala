package com.abhinab.hackerank.welcomechallenge

object SalesByMatch {
  def sockMerchant(n: Int, ar: Array[Int]): Int = ar.map(x => (x, 1)).groupBy(_._1).map(x => x._2.map(_._2).sum).map(x => x/2).sum

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    //val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = stdin.readLine.trim.toInt

    val ar = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = sockMerchant(n, ar)

    //printWriter.println(result)

    //printWriter.close()
  }
}
