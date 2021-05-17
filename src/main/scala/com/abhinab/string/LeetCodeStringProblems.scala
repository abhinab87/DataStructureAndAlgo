package com.abhinab.string

import java.util

import scala.collection.mutable.Map
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object LeetCodeStringProblems {

  /**
  *Given a string s, find the length of the longest substring without repeating characters.
  *
  * Example:1 Input: s = "abcabcbb"
  * Output: 3
  * Explanation: The answer is "abc", with the length of 3.
  *
  * Example: 2- Input: s = "bbbbb"
  * Output: 1
  * Explanation: The answer is "b", with the length of 1.
  *
   */

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
  * Valid Parenthesis: Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.
  * An input string is valid if:
  * Open brackets must be closed by the same type of brackets.
  * Open brackets must be closed in the correct order.
  *
  * Example: 1 - Input: s = "()"
  * Output: true
   */
  def validParenthisis(str: String): Boolean={
    val stk = new util.Stack[Char]()
    val charArray = str.toCharArray
    for(i <- 0 until charArray.length){
      if(charArray(i) == '(') stk.push(')')
      else if(charArray(i) == '{') stk.push('}')
      else if(charArray(i) == '[') stk.push(']')
      else if(stk.isEmpty || stk.pop() != charArray(i)) return false
    }
    return stk.isEmpty
  }

  /**
  *Longest Common Prefix: Write a function to find the longest common prefix string amongst an array of strings.
  * If there is no common prefix, return an empty string "".
  *
  * Example 1: Input: strs = ["dog","racecar","car"]
  * Output: ""
  * Explanation: There is no common prefix among the input strings.
  *
  * Example: 2 Input: strs = ["flower","flow","flight"]
  * Output: "fl"
  *
  * Reference: https://leetcode.com/problems/longest-common-prefix/
   */
  def longestCommonPrefix(str:Array[String]):String={
    val lb = new ListBuffer[Int]
    val sb = new StringBuilder
    val sortedArray = str.sorted
    val firstCharArray = sortedArray(0).toCharArray
    val lastCharArray = sortedArray(sortedArray.length - 1).toCharArray
    for(i <- 0 until firstCharArray.length){
      if(lastCharArray.length > i && lastCharArray(i) == firstCharArray(i)){
        sb.append(lastCharArray(i))
      } else {
        sb
      }
    }
    sb.toString
  }

  /**
  * Longest Common Subsequence: Given two strings text1 and text2, return the length of their longest common subsequence. If there is no common subsequence, return 0.
  *  A subsequence of a string is a new string generated from the original string with some characters (can be none) deleted without changing the relative order of the remaining characters.
  *   For example, "ace" is a subsequence of "abcde".
  * A common subsequence of two strings is a subsequence that is common to both strings.
  *
  * Example 1: Input: text1 = "abcde", text2 = "ace"
  * Output: 3
  * Explanation: The longest common subsequence is "ace" and its length is 3. DP
  * reference: https://leetcode.com/problems/longest-common-subsequence/
   */
  def longestCommonSubsequence(text1: String, text2: String): Int = {
    val arr= Array.ofDim[Int](text1.length+1, text2.length+1)
    var max=0
    for(i <- 1 to text1.length) {
      for(j <- 1 to text2.length){
        arr(i)(j) = if(text1.charAt(i-1) == text2.charAt(j-1))  arr(i-1)(j-1) + 1 else  math.max(arr(i-1)(j), arr(i)(j-1))
        if(arr(i)(j) > max) max = arr(i)(j)
      }
    }
    max
  }

  /**
  * Longest Palindrome Sequence: Given a string s, return the longest palindromic substring in s.
  * Example:1 Input: s = "babad"
  * Output: "bab"
  *
  * Reference: https://leetcode.com/problems/longest-palindromic-substring/
  * Reference: https://www.youtube.com/watch?v=XYQecbcd6_c
  * time Complexity O(n^2)
   */
  def longestPalindrome(s: String): String = {
    var res = ""
    var resLen = 0
    for(i <- 0 until s.length){
      //odd length
      var (leftOdd, rightOdd) = (i, i)
      while(leftOdd >= 0 && rightOdd < s.length && s(rightOdd) == s(leftOdd)){
        if((rightOdd - leftOdd + 1) > resLen){
          res = s.substring(leftOdd, rightOdd+1)
          resLen = rightOdd - leftOdd + 1
        }
        leftOdd -= 1
        rightOdd += 1
      }
      //even length
      var (leftEven, rightEven) = (i, i + 1)
      while(leftEven >= 0 && rightEven < s.length && s(rightEven) == s(leftEven)){
        if((rightEven - leftEven + 1) > resLen){
          res = s.substring(leftEven, rightEven+1)
          resLen = rightEven - leftEven + 1
        }
        leftEven -= 1
        rightEven += 1
      }
    }

    res
  }

  /**
   * Given a string s, return the number of palindromic substrings in it.
   * A string is a palindrome when it reads the same backward as forward.  A substring is a contiguous sequence of characters within the string.
   *
   * Example: 1
   * Input: s = "abc"
   * Output: 3
   * Explanation: Three palindromic strings: "a", "b", "c".
   *
   * Example: 2
   *
   * Input: s = "aaa"
   * Output: 6
   * Explanation: Six palindromic strings: "a", "a", "a", "aa", "aa", "aaa".
   *
   * Reference: https://www.youtube.com/watch?v=gI1771HyXu0
   *
   *
   */
  def countSubstrings(s: String): Int = {
    var result = 0
    for(i <- 0 until s.length){
      cnt(i, i)
      cnt(i , i + 1)
    }
    def cnt(start: Int, end: Int):Unit={
      var strtCnt = start
      var endCnt = end
      while(strtCnt >= 0 && endCnt <= s.length - 1 && s.charAt(strtCnt) == s.charAt(endCnt)){
        result += 1
        strtCnt -= 1
        endCnt += 1
      }
    }
    result
  }

  /**
   * You are given a string s and an integer k. You can choose any character of the string and change it to any other uppercase English character.
   * You can perform this operation at most k times. Return the length of the longest substring containing the same letter you can get after performing the
   * above operations.
   *
   * Example: 1
   * Input: s = "ABAB", k = 2
   * Output: 4
   * Explanation: Replace the two 'A's with two 'B's or vice versa.
   *
   * Example: 2
   * Input: s = "AABABBA", k = 1
   * Output: 4
   * Explanation: Replace the one 'A' in the middle with 'B' and form "AABBBBA". The substring "BBBB" has the longest repeating letters, which is 4.
   *
   * Reference: https://www.youtube.com/watch?v=00FmUN1pkGE
   * https://leetcode.com/problems/longest-repeating-character-replacement/
   */
  def characterReplacement(s: String, k: Int): Int = {
    val charCounts = scala.collection.mutable.Map[Char, Int]().empty
    var win_start = 0
    var max_length = 0
    var max_count = 0

    for(i <- 0 until s.length){
      val currentCount = charCounts.getOrElse(s.charAt(i), 0)
      charCounts += (s.charAt(i) -> (currentCount + 1))
      val current_char_count = charCounts.get(s.charAt(i)).get
      max_count = math.max(max_count, current_char_count)

      while(i - win_start - max_count + 1 > k){
        charCounts += (s.charAt(win_start) -> (charCounts.get(s.charAt(win_start)).get - 1))
        win_start += 1
      }
      max_length = math.max(max_length, i - win_start + 1)
    }
    max_length
  }

  def main(args: Array[String]): Unit = {
    println(characterReplacement("AABABBA", 1))
  }
}
