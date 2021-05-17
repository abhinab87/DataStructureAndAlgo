package com.abhinab.hackerank.string

object MakingAnagrams {
  def makeAnagram(a: String, b: String): Int = {
    val commonChars = a.intersect(b)
    (a.length - commonChars.length) + (b.length - commonChars.length)
  }

  def alternatingCharacters(s: String): Int = {
    var deletions = 0
    for(i <- 0 until s.length - 1){
      if(s.charAt(i) == 'A'){
        if(s.charAt(i + 1) == 'A')
          deletions += 1
      } else{
        if(s.charAt(i + 1) == 'B')
          deletions += 1
      }
    }
    deletions
  }

  // Complete the isValid function below.
  def isValid(s: String): String = {
    val counts = s.toCharArray.map(x => (x,1)).groupBy(_._1).map(x => x._2.map(_._2).sum).toList.sorted
    println(counts)
    if(counts.distinct.length == 1) "YES"
      else {
        if(counts.distinct.length >= 3) "NO"
        else if (counts(counts.length - 1) - counts(counts.length - 2) == 1) "YES"
        else if (counts(counts.length - 1) == counts(counts.length - 2)) "NO"
        else "NO"
      }
  }

  /**
  Find whether string s is divisible by string t. A string s divisible by string t if string t can be concatenated some number of times to obtain the string s. If s is divisible, find the smallest string u such that
  it can be concatenated some number of times to obtain both s and t. If it is not divisible, set the return value to -1. Finally, return the length of the string u or -1.

  Example 1:
  s = "bcdbcdbcdbcd"
  t = "bcdbcd"

  If string t is concatenated twice, the result is "bcdbcdbcdbcd" which is equal to the string s. The string s is divisible by string t. Since it passes the first test, look for the smallest string u that can be
  concatenated to create both strings s and t.

  The string "bcd" is the smallest string that can be concatenated to create both strings s and t. The length of the string u is 3, the integer value to return.

  Example 2:
  s = "bcdbcdbcd"
  t = "bcdbcd"

  If string t is concatenated twice, the result is "bcdbcdbcdbcd" which is greater than string s. There is an extra "bcd" in the concatenated string. The string s is not divisible by string t, so return -1.
   */
  def stringDivisible(s:String, t: String):Int= if(s.length % t.length > 0) -1 else {
    var sb = new StringBuilder
    for(i <- 0 until s.length/t.length){
      sb.append(t)
    }
    if(!sb.toString.equals(s)) {
      return -1
    }

    var divisible = 1

    for(i <- 1 to t.length){
      if(t.length % divisible == 0) {
        sb = new StringBuilder
        val substr = t.substring(0, i)
        while (sb.length < t.length) {
          sb.append(substr)
        }
        println(substr+" : "+sb+" : "+divisible)
        if (sb.toString.equals(t)) {
          return i
        }
      }
      divisible += 1
    }
    return -1
  }

  def main(args: Array[String]): Unit = {
    println(alternatingCharacters("AAAA"))
    println(isValid("aabbccddeefghi"))
    println("stringDivisible "+stringDivisible("bcdbcdbcdbcdbcdbcd","bcdbcd"))
  }
}
