package com.abhinab.string

import java.util

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

  def longestCommonPrefix(str:Array[String]):String={
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

  def reverseString(str:String):String =  (for(i <- str.length - 1 to 0 by -1) yield str(i)).mkString

  def reverseVowels(str:String):String ={

    str
  }

  def firstUniqueChar(str:String):Int ={
    val ab = new StringBuffer()
    for(i <- 0 until str.length){
      if(str.count(_ == str(i)) == 1) ab.append(str(i))
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
    println("Roman integer to Integer conversion is "+romanLiteralToNumberConversion("IV"))
    println("longest common prefix is "+longestCommonPrefix(Array("abc","b","ab","abcd")))
    println("valid parenthesis "+validParenthisis("()[{}[]"))
    println("Length of last word is "+lengthOfLastWord("Abhinab Is OK"))
    println("binary Addition is "+binaryAddition("1010","10"))
    println("String Reversal is "+reverseString("man"))
    println("Index of First unique charecter in the string is "+firstUniqueChar("eetlCode"))
    println("number of segments in the string are "+countSegments("This is Testing..."))
    println("The string is having repeated patteren "+repeatedSubstringPatteren("aba"))
    println("The new time is "+timeConversion("03:12:23LM"))
    println("Super Reduced String is "+"baabd".foldLeft("")(superReducedString))
  }
}
