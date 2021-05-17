package com.abhinab.searching

import scala.annotation.tailrec

object SearchingAlgoApplications {

  def main(args: Array[String]): Unit = {
    println("Missing Number is:" +missingNumber(Array(1,2,3,5,6)))
    println("Element in rotated Array is: " + searchElementInRotatedArray(Array(1,3 ), 1))
  }

  def missingNumber(array: Array[Int]):Int = ((array.length +2)*(array.length+1)/2) - array.sum


  /**
   *
   * @param nums
   * @param target
   * @return Index of Target
   *         time complexity O(log(n)) and space complexity O(1)
   *         Reference: https://www.youtube.com/watch?v=e-84rG-c8AE
   *         https://leetcode.com/problems/search-in-rotated-sorted-array/
   */
  def searchElementInRotatedArray(nums: Array[Int], target: Int): Int = if(nums.size == 0) -1 else{
    var left = 0
    var right = nums.size - 1
    var mid = 0

    while(left <= right){
      mid = left + (right - left) / 2
      if(nums(mid) == target) return mid
      if(nums(left) <= nums(mid)){
        if(target >= nums(left) && target <= nums(mid)){
          right = mid - 1
        }else{
          left = mid + 1
        }
      } else{
        if(target >= nums(mid) && target <= nums(right)){
          left = mid + 1
        }else{
          right = mid - 1
        }
      }
    }
    -1
  }

}
