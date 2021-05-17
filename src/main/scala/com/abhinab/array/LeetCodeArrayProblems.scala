package com.abhinab.array

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object LeetCodeArrayProblems {

  /**
  A left rotation operation on an array shifts each of the array's elements  unit to the left. For example, if  left rotations are performed on array , then the array would become .
  Note that the lowest index item moves to the highest index in a rotation. This is called a circular array.
  Given an array  of  integers and a number, , perform  left rotations on the array. Return the updated array to be printed as a single line of space-separated integers.
  Function Description
  Complete the function rotLeft in the editor below.
  rotLeft has the following parameter(s):
  int a[n]: the array to rotate
  int d: the number of rotations
  Returns
  int a'[n]: the rotated array
   */
  def rotLeft(a: Array[Int], d: Int): Array[Int] = {
    val (left, right) = a.splitAt(d)
    right ++ left
  }

  /**
  Complete the minimumBribes function below.
  Reference: https://www.thecodingshala.com/2020/02/hackerrank-new-year-chaos-solution.html
  """
  It is New Year's Day and people are in line for the Wonderland rollercoaster ride. Each person wears a sticker indicating their initial position in the queue from  to .
  Any person can bribe the person directly in front of them to swap positions, but they still wear their original sticker. One person can bribe at most two others.
  Determine the minimum number of bribes that took place to get to a given queue order. Print the number of bribes, or, if anyone has bribed more than two people,
  print Too chaotic.

  Example:
  If person  bribes person , the queue will look like this: . Only  bribe is required. Print 1.
  Person  had to bribe  people to get to the current position. Print Too chaotic.
  """
   */
  def minimumBribes(q: Array[Int]):Unit={
    var ans = 0
    for(i <- q.length - 1 to 0 by -1){
      var ch_pos = q(i) - (i + 1)
      if(ch_pos > 2){
        println("Too chaotic")
        return
      } else{
        var st = math.max(0, q(i) - 2)
        for(j <- st until i){
          if(q(j) > q(i))
            ans += 1
        }
      }
    }
    println(ans)
  }



  /**
  * Find out the intersection between two different unsorted arrays.
  *
  * Solution-1: Iterate over the smaller array and do a binary search on  the bigger array.
  * The time complexity of binary search method to find intersection is O(nlogn) for sorting and then O(mlogn) for searching.
  * Effective time complexity becomes O((n+m)logn) which is not optimal.
  *
  * Solution-2: Create a hash with key as elements from the smaller array (saves space). Then scan through other array and see if the element is present in hash.
  * If yes, put into intersection array else do not.
  * This method will be having o(n) time complexity.
   */
  def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] = if(nums1.length == 0 || nums2.length == 0) Array.empty[Int] else {
    var map = Map.empty[Int,Boolean]
    val result = ArrayBuffer[Int]()
    for(i <- 0 until nums2.length)
      map += (nums2(i) -> true)

    for(i <- 0 until nums1.length)
      if(map.contains(nums1(i)))
        result += nums1(i)

    result.toArray
  }

  /**
  * Given an array of string words. Return all strings in words which is substring of another word in any order.
  * String words[i] is substring of words[j], if can be obtained removing some characters to left and/or right side of words[j].
  *
  * Example: Input: words = ["mass","as","hero","superhero"]
  * Output: ["as","hero"]
  * Explanation: "as" is substring of "mass" and "hero" is substring of "superhero". ["hero","as"] is also a valid answer.
   */

  def stringMatching(words: Array[String]): List[String] = if(words.length <= 1) List("") else {
    val lb = ListBuffer[String]()
    for(i <- 0 until words.length){
      for(j <- i+1 until words.length){
        if(words(i).indexOf(words(j))!= -1)
          lb += words(j)
        else if(words(j).indexOf(words(i))!= -1)
          lb += words(i)
        else
          lb
      }
    }
    lb.distinct.toList
  }

  /**
  * We are given two strings, A and B.
  * A shift on A consists of taking string A and moving the leftmost character to the rightmost position. For example, if A = 'abcde',
  * then it will be 'bcdea' after one shift on A. Return True if and only if A can become B after some number of shifts on A.
  *
  * Example 1: Input: A = 'abcde', B = 'cdeab'
  * Output: true
  *
  * Example 2: Input: A = 'abcde', B = 'abced'
  * Output: false
   */
  def rotateString(A: String, B: String): Boolean = if(A.length == 0 || B.length == 0) false else {
    val indexOfBinA = A.zipWithIndex.filter(x => x._1 == B(0)).map(_._2)
    val resultArray = ArrayBuffer[Boolean]()
    indexOfBinA.map { x =>
      val (left, right) = A.splitAt(x)
      val rotatedArray = right ++ left
      val result = if (rotatedArray == B) true else false
      resultArray +=result
    }
    resultArray.contains(true)
  }

  /**
  * Given an integer array nums, find the contiguous subarray (containing at least one number) which has the largest sum and return its sum.
  *
  * Example - 1: Input: nums = [-2,1,-3,4,-1,2,1,-5,4]
  * Output: 6
  * Explanation: [4,-1,2,1] has the largest sum = 6.
  *
  * Example - 2: Input: nums = [1]
  * Output: 1
  *
  * Follow up: If you have figured out the O(n) solution, try coding another solution using the divide and conquer approach, which is more subtle.
  * Two ways, dp space complexity is O(n) then space O(1)
  *
  * Reference : https://eavis.gitbooks.io/leetcode/content/maximum-subarray.html?q=


  def maxSubArraySum(nums:Array[Int]): Int={
    var max_so_far = nums(0)
    var max_ending_here = ArrayBuffer[Int]()
    max_ending_here += nums(0)
    for(i <- 1 until nums.length){
      max_ending_here += nums(i) + (if(max_ending_here(i - 1) > 0) max_ending_here(i - 1) else 0)
      max_so_far = math.max(max_so_far, max_ending_here(i))
    }
    max_so_far
  }
*/

  def maxSubArraySum(nums:Array[Int]): Int={
    def maxSubArraySum(nums:Array[Int], low: Int, high: Int): Int={
      def maxCrossingSum(nums:Array[Int], low: Int, mid: Int, high: Int): Int={
        var sum = 0
        var left_sum = Int.MinValue
        for(i <- mid to low by -1){
          sum = sum + nums(i)
          if(sum > left_sum) left_sum = sum
        }

        sum = 0
        var right_sum = Int.MinValue
        for(i <- mid + 1 until high+1){
          sum = sum + nums(i)
          if(sum > right_sum) right_sum = sum
        }

        left_sum + right_sum
      }
      if(low == high) nums(low) else{
        val mid = (low + high)/2
        math.max(math.max(maxSubArraySum(nums, low, mid), maxSubArraySum(nums, mid+1, high)), maxCrossingSum(nums, low, mid, high));
      }
    }
    maxSubArraySum(nums, 0, nums.length - 1)
  }

  /**
  * Three sum Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]] such that i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.
  *
  * Example: Input: nums = [-1,0,1,2,-1,-4]
  * Output: [[-1,-1,2],[-1,0,1]]
  *
  * Input: nums = []
  * Output: []
  *
  * Example: Input: nums = [0]
  * Output: []
  *
  * Firstly we count the count for each distinct number in the incoming array.
  * Then we start iterating a and b throgh the numbers in ascending order.
  * We also should skip the case when a == b and the amount of as in the initial array is == 1.
  * After that we know that c should be -a - c. Try to find it in our numbersCount and check that the amount of cs in the initial array fits the need (c can be equal to a or b or both).
  *
   */
  def threeSum(nums: Array[Int]): List[List[Int]] = if(nums.length < 3) List.empty[List[Int]] else {
    val numbersCount = nums.foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { (count, nextNumber) =>
      count + (nextNumber -> (count(nextNumber) + 1))
    }

    val uniqueNumbers = numbersCount.keys.toList.sorted

    for {
      a <- uniqueNumbers
      b <- uniqueNumbers
      if a <= b
      if a != b || numbersCount(a) > 1
      c = -a - b
      if b <= c
      candidate = List(a, b, c)
      if numbersCount(c) >= candidate.count(_ == c)
    } yield candidate
  }

  /**
  * Find minimum swaps: You are given an unordered array consisting of consecutive integers  [1, 2, 3, ..., n] without any duplicates. You are allowed to swap any two elements.
  * Find the minimum number of swaps required to sort the array in ascending order.
  * Reference: https://www.geeksforgeeks.org/minimum-number-swaps-required-sort-array/
  * Time Complexity: O(n Log n)
  * Auxiliary Space: O(n)
   */
  def findMinimumSwaps(arr: Array[Int]): Int = {
    var swaps = 0
    val arrpos = arr.zipWithIndex.sorted
    var vis = Array.ofDim[Boolean](arr.length)

    for(i <- 0 until arr.length){
      if(vis(i) == false || arrpos(i)._2 != i){
        var cycle_size = 0
        var j = i
        while(!vis(j)){
          vis(j) = true
          j = arrpos(j)._2
          cycle_size += 1
        }

        if(cycle_size > 0){
          swaps += (cycle_size - 1)
        }
      }
    }
    return swaps
  }

  /**
   *Given an array of intervals intervals where intervals[i] = [starti, endi], return the minimum number of intervals you need to
   * remove to make the rest of the intervals non-overlapping.
   *
   * Example: 1
   * Input: intervals = [[1,2],[2,3],[3,4],[1,3]]
   * Output: 1
   * Explanation: [1,3] can be removed and the rest of the intervals are non-overlapping.
   *
   * @param intervals
   * @return number of elements to be removed to make the list non overlapping
   * Reference : https://leetcode.com/problems/non-overlapping-intervals/
   * https://www.youtube.com/watch?v=3oDvuHCTFmY
   */
  def eraseOverlapIntervals(intervals: Array[Array[Int]]): Int = {
    val arr = intervals.map(x => (x(0),x(1))).sortBy(_._2)
    var count = 0
    var close = arr(0)._2
    for(i <- 1 until arr.length){
      if(close > arr(i)._1) count += 1 else close = arr(i)._2
    }
    count
  }

  /**
   * Given two integers a and b, return the sum of the two integers without using the operators + and -
   * Example: 1
   * Input: a = 1, b = 2
   * Output: 3
   *
   * Example: 2
   * Input: a = 2, b = 3
   * Output: 5
   *
   * Reference: https://leetcode.com/problems/sum-of-two-integers/
   * https://www.youtube.com/watch?v=kIXhc8nZKIo
   */
  def getSum(a: Int, b: Int): Int = {
    @tailrec def loop(x: Int, y: Int, acc: Int = 0): Int = {
      if ((x & y) == 0) {
        println("(x & y) "+ (x & y))
        (x ^ y) | acc
      }
      else {
        println("x ^ y:: "+(x ^ y)+ " (x & y)<<1:: "+ ((x & y)<<1))
        loop(x ^ y, (x & y)<<1, (x ^ y) | (x & y)<<1)
      }
    }
    loop(a, b)
  }

  /**
   * Given an integer array nums and an integer k, return the k most frequent elements. You may return the answer in any order.
   * Example 1:
   * Input: nums = [1,1,1,2,2,3], k = 2
   * Output: [1,2]
   *
   * Example: 2
   * Input: nums = [1], k = 1
   * Output: [1]
   *
   * @param nums
   * @param k
   * @return
   */
  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    nums.groupBy(x => x).map(x => (x._1, x._2.length)).toArray.sortWith(_._2 > _._2).map(_._1).take(k)
  }
  def main(args: Array[String]): Unit = {
    val ar1 = Array(1,2,2,1)
    val ar2 = Array(2,2)
    val ar4 = Array("mass","as","hero","superhero")
    var ar5 = Array(-2,1,-3,4,-1,2,1,-5,4)
    val ar3 = Array(1,2,3,4)
    ar3.map(x => x+ar3.tail.sum)
    ar1.map(x => x*x).sortWith(_ > _)
    //println("Intersection of two array is "+intersect(ar1, ar2).toList)
    //println("Intersection of two array is "+stringMatching(ar4))
    //println("max sub array sum "+ maxSubArraySum(ar5))
    println("get Sum "+ getSum(2,-3))
  }

}
