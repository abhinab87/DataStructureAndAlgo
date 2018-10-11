package com.abhinab.array

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

  def main(args: Array[String]): Unit = {
    println("Roman integer to Integer conversion is "+romanLiteralToNumberConversion("IV"))
    println("longest common prefix is "+longestCommonPrefix(Array("abc","b","ab","abcd")))
    println("valid parenthesis "+validParenthisis("()[{}[]"))
  }
}