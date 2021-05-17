package com.abhinab.hackerank

import java.util

object App {

  def twoSum(nums:Array[Int], target:Int): Array[Int] ={
    nums.combinations(2).collect {
      case couple if couple.sum == target => couple.map(nums.indexOf)
    }.flatMap(x => x).toArray
  }

  def lengthOfLongestSubstring(s: String): Int = if(s.length == 0 || s.length == 1 ) s.length else {
    var map = new util.HashMap[Char,Int]()
    var i = 0
    var ans = 0
    for(j <- 0 until s.length){
      if(map.containsKey(s.charAt(j))) i = math.max(map.get(s.charAt(j)), i)
      ans = Math.max(ans, j - i + 1)
      map.put(s.charAt(j), j + 1)
    }
    ans
  }

  /**
   * Checks whether the given string 's' is palindrome or not.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def isPalindrome(s: String): Boolean = {
    def loop(i: Int, j: Int): Boolean =
      if (i >= j) true
      else if (s.charAt(i) == s.charAt(j)) loop(i + 1, j - 1)
      else false

    loop(0, s.length - 1)
  }

  /**
   * Searches for the longest palindrome in given string 's'.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-12-longest-palindromic-subsequence/
   *
   * Time - O(n^2)
   * Space - O(n)
   */
  def longestPalindrome(s: String): String = {
    def check(i: Int, j: Int): Boolean =
      if (i >= j) true
      else if (s.charAt(i) == s.charAt(j)) check(i + 1, j - 1)
      else false

    def search(i: Int, l: Int, j: Int, m: Int): String =
      if (i == s.length) s.substring(j - m + 1, j + 1)
      else if (i - l > 0 && check(i - l - 1, i)) {
        if (l + 2 > m) search(i + 1, l + 2, i, l + 2)
        else search(i + 1, l + 2, j, m)
      } else if (i - l >= 0 && check(i - l, i)) {
        if (l + 1 > m) search(i + 1, l + 1, i, l + 1)
        else search(i + 1, l + 1, j, m)
      } else {
        search(i + 1, 1, j, m)
      }

    if (s.isEmpty) s
    else search(1, 1, 1, 1)
  }

  /** Container with most amount of Water
   *Reference: https://www.geeksforgeeks.org/container-with-most-water/
   */
    def maxArea(height:Array[Int]):Int ={
      var area = 0
      for(i <- 0 until height.length){
        for(j <- i+1 until height.length){
          area = math.max(area, math.min(height(i), height(j)) * (j-i))
        }
      }
      area
    }

  def main(args: Array[String]): Unit = {
    println("two sum of Array is "+twoSum(Array(1,2,3,4),5).toList)
    println("Longest Palindrome is "+longestPalindrome("bananas"))
    println("Container with Most Water "+ maxArea(Array(1, 5, 4, 3)))
    println("Container with Most Water "+ maxArea(Array(3, 1, 2, 4, 5)))
  }
}
