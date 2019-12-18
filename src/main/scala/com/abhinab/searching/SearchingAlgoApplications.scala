package com.abhinab.searching

import scala.annotation.tailrec

object SearchingAlgoApplications {

  def main(args: Array[String]): Unit = {
    println("Missing Number is:" +missingNumber(Array(1,2,3,5,6)))
    println("Element in rotated Array is: " + searchElementInRotatedArray(Array(3,4,5,1,2), 1))
  }

  def missingNumber(array: Array[Int]):Int = ((array.length +2)*(array.length+1)/2) - array.sum

  def searchElementInRotatedArray(array: Array[Int], target:Int):Option[Int] = {
    @tailrec
    def pivotRecursion(low:Int, high:Int):Option[Int] = (low + high)/2 match{
      case _ if high < low => None
      case mid if (array(mid) > array(mid + 1) && mid < high ) => Some(mid)
      case mid if (array(mid) < array(mid - 1) && mid > low ) => Some(mid)
      case mid if array(low) >= array(mid) => pivotRecursion(low, mid - 1)
      case mid => pivotRecursion(mid + 1, high)
    }

    @tailrec
    def binarySearchRecursion(low:Int, high:Int):Option[Int] = (low + high)/2 match{
      case _ if high < low => None
      case mid if array(mid) > target => binarySearchRecursion(low, mid - 1)
      case mid if array(mid) < target => binarySearchRecursion(mid + 1, high)
      case mid => Some(mid)
    }

    val pivot = pivotRecursion(0, array.length - 1)
    val index = if(pivot.isDefined){
      if(array(0) > target) binarySearchRecursion(pivot.get + 1, array.length - 1)
      else if(array(0) < target) binarySearchRecursion(0, pivot.get - 1)
      else if(array(0) == target) Some(0)
      else None
    } else None

    index
  }

}
