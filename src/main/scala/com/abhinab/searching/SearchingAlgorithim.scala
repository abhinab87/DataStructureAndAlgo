package com.abhinab.searching

import scala.None
import scala.annotation.tailrec

object SearchingAlgorithim {
  def main(args: Array[String]): Unit = {
    println("Using Exponential Search "+ exponentialSearch(Array(2,4,6,8,10,12,14,16,18,20,22,24,26), 28))
  }

  def linearSearch(array: Array[Int], target: Int): Option[Int]={
    @tailrec
    def recursion(low:Int, high:Int): Option[Int]= low match{
      case _ if(high < low) => None
      case mid if(array(mid) != target) => recursion(low+1, array.size - 1)
      case mid => Some(mid)
    }
    recursion(0, array.size - 1)
  }

  def binarySearch(array: Array[Int], target: Int): Option[Int]={
    @tailrec
    def recursion(low:Int, high:Int): Option[Int]= (low+high)/2 match{
      case _ if(high < low) => None
      case mid if(array(mid) > target) => recursion(low, mid - 1)
      case mid if(array(mid) < target) => recursion(mid+1, high)
      case mid => Some(mid)
    }
    recursion(0, array.size - 1)
  }

  def jumpSearch(array: Array[Int], target: Int): Option[Int]={
    @tailrec
    def recursion(low:Int, high:Int): Option[Int]= (math.sqrt(high).toInt + low) match{
      case _ if(high < low) => None
      case numOfJump if(array(numOfJump) > target) => recursion(numOfJump, array.length)
      case numOfJump if(array(numOfJump) < target) => recursion(numOfJump - math.sqrt(array.length).toInt, numOfJump)
      case numOfJump if(array(numOfJump) == target) => Some(numOfJump)
    }
    recursion(0, array.size)
  }

  def interpolationSearch(array: Array[Int], target: Int): Option[Int]={
    @tailrec
    def recursion(low:Int, high:Int): Option[Int]= (low+((target - array(low)) * (high - low) / (array(high) - array(low)))) match{
      case _ if(high < low) => None
      case pos if(pos < 0 || pos >= array.length) => None
      case pos if(array(pos) > target) => recursion(low, pos - 1)
      case pos if(array(pos) < target) => recursion(pos+1, high)
      case pos => Some(pos)
    }
    recursion(0, array.size - 1)
  }

  def exponentialSearch(array: Array[Int], target: Int): Option[Int]={
    @tailrec
    def recursion(low:Int, high:Int): Option[Int]= high match {
      case _ if(high < low - 1) => None
      case mid if (mid < 0 || mid >= array.length) => None
      case mid if(array(mid) > target) => recursion(low, (low+mid)/2)
      case mid if(array(mid) < target) => recursion(high, high * 2)
      case mid => Some(mid)
    }
    recursion(0, array.size - 1)
  }
}
