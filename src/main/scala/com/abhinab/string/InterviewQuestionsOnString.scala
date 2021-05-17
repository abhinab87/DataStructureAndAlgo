package com.abhinab.string

import java.util
import java.util.HashMap
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

object InterviewQuestionsOnString {

  def romanLiteralToNumberConversion(str:String): Int={
    var sum = 0
    if(str.indexOf("IV") != -1) sum = sum - 2
    if(str.indexOf("IX") != -1) sum = sum - 2
    if(str.indexOf("XL") != -1) sum = sum - 20
    if(str.indexOf("XC") != -1) sum = sum - 20
    if(str.indexOf("CD") != -1) sum = sum - 200
    if(str.indexOf("CM") != -1) sum = sum - 200

    val char = str.toCharArray
    for(i <- 0 until str.length){
      if(char(i) == 'M') sum = sum + 1000
      if(char(i) == 'D') sum = sum + 500
      if(char(i) == 'C') sum = sum + 100
      if(char(i) == 'L') sum = sum + 50
      if(char(i) == 'X') sum = sum + 10
      if(char(i) == 'V') sum = sum + 5
      if(char(i) == 'I') sum = sum + 1
    }
    sum
  }

  def removeDuplicates(nums: Array[Int]): Array[Int] = if(nums.length == 0 || nums.length == 1) nums else nums.filter(x => nums.count(_ == x) == 1)

  def longestCommonPrefixBetweenStrings(str1:String, str2:String):String = str1.zip(str2).takeWhile(Function.tupled(_ == _)).map(_._1).mkString

  def longestSuffix(str1: String, str2: String) = str1.reverseIterator.zip(str2.reverseIterator).takeWhile( c => c._1 == c._2).toList.reverseMap(c => c._1) mkString ""

  def lengthOfLastWord(str:String):Int = str.trim.length - str.trim.lastIndexOf(" ") - 1

  def binaryAddition(str1:String, str2:String):String={
    var str = ""
    var carry = 0
    val len1 = str1.length
    val len2 = str2.length
    val maxLength = math.max(len1,len2)
    for(i <- 0 until maxLength){
      val p = if(i < len1) str1.charAt(len1 - i - 1) - '0' else '0'
      val q = if(i < len2) str2.charAt(len2 - i - 1) - '0' else '0'
      val tmp = p + q + carry
      carry = tmp/2
      str = tmp%2 + str
    }
    str
  }

  def removeDuplicates1(nums: Array[Int]): Array[Int] = if(nums.length < 2) nums else {
    var len = 0
    val lb = new ListBuffer[Int]
    for(i <- 1 until nums.length){
      if(nums(i) != nums(len)) {
        lb += nums(len)
        len = len + 1
      }
    }
    //if(nums(nums.length - 1) != nums(nums.length - 2)) lb += nums(nums.length-1) else lb
    lb.toArray
  }

  def reverseString(str:String):String =  (for(i <- str.length - 1 to 0 by -1) yield str(i)).mkString

  def reverseVowels(str:String):String ={

    str
  }

  def firstUniqueChar(str:String):Int ={
    val ab = new StringBuffer()
    breakable {
      for (i <- 0 until str.length) {
        if (str.count(_ == str(i)) == 1) {
          ab.append(str(i))
          break()
        }
      }
    }
    val nonRepStr = ab.toString.toCharArray
    str.indexOf(nonRepStr(0))
  }

  def countSegments(str:String):Int = str.split(" ").length

  def repeatedSubstringPatteren(str:String):Boolean ={
    val length = str.length
    for(i <- length/2 until 0 by -1){
      if(length % i == 0){
        val sb = new StringBuilder
        val mid = length/i
        val sub = str.substring(0,i)
        for(j <- 0 until mid){
          sb.append(sub)
        }
        if(sb.toString().equalsIgnoreCase(str))
          return true
      }
    }
    return false
  }

  def timeConversion(str:String):String =
    if(str.substring(str.length-2, str.length).equalsIgnoreCase("AM")) {
      str
    } else if(str.substring(str.length-2, str.length).equalsIgnoreCase("PM")){
      val hour = str.substring(0,2).toInt + 12
      hour.toString+str.substring(2,str.length)
    }else{
      "Not valid Input"
    }

  def superReducedString(acc:String, c:Char):String = if(acc.length > 0 && acc.charAt(acc.length - 1) == c) acc.substring(0,acc.length - 1) else acc + c

  def main(args: Array[String]): Unit = {
    println("Longest Common Prefix between two strings are: "+longestCommonPrefixBetweenStrings("Abhinab is OK","Abhishek is OK"))
    println("Longest suffux between two strings are: "+longestSuffix("Abhinab is OK","Abhishek OK"))
    println("Roman integer to Integer conversion is "+romanLiteralToNumberConversion("IV"))
    //println("longest common prefix is "+longestCommonPrefix(Array("abc","abcde","ab","abcd")))
    //println("valid parenthesis "+validParenthisis("()[{}[]"))
    println("Length of last word is "+lengthOfLastWord("Abhinab Is OK"))
    println("binary Addition is "+binaryAddition("1010","10"))
    println("String Reversal is "+reverseString("man"))
    println("Index of First unique charecter in the string is "+firstUniqueChar("eetlCode"))
    println("number of segments in the string are "+countSegments("This is Testing..."))
    println("The string is having repeated patteren "+repeatedSubstringPatteren("aba"))
    println("The new time is "+timeConversion("03:12:23PM"))
    println("Super Reduced String is "+"baabd".foldLeft("")(superReducedString))
    //println("Length of Longest subString without repeating charecter is "+lengthOfLongestSubstringWithoutRepeatingCharecter("pwwkew"))
    //println("longest common sub sequence is "+longestCommonSubsequence("oxcpqrsvwf", "shmtulqrypy"))
    println(removeDuplicates1(Array(0,1,1,2,3,3)).toList)
  }
}


