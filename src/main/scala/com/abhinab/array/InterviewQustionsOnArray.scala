package com.abhinab.array

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object InterviewQustionsOnArray {
  def longestConSeq(num:Array[Int]): Int ={
    var res = 0
    var mp = Map[Int, Int]()
    for(i <- 0 until num.length) {
      breakable {
        if(mp.contains(i)) break()
        else {
          val left = if (mp.contains(i - 1)) mp.get(i - 1).get else 0
          val right = if (mp.contains(i + 1)) mp.get(i + 1).get else 0

          val sum = left + right + 1
          mp += (i -> sum)
          println(left + "::" + right + "::" + sum)
          //println(mp)
          res = math.max(res, sum)

          mp += (i - left -> sum)
          mp += (i + right -> sum)
          println(mp)
        }
      }
    }
    res
  }

  def longestNonRepeatativeSequence(str:String): String={
    var strMap = Map.empty[Char,Int]
    var array = ArrayBuffer[String]()
    var sb = new StringBuffer()
    for(i <- 0 until str.length){
      if(strMap.contains(str(i)) || i == str.length - 1) {
        if(i != str.length - 1) {
          array += sb.toString
          sb = new StringBuffer()
          sb.append(str(i))
        } else {
          array += sb.toString + str(i)
          sb = new StringBuffer()
        }
      } else {
        sb.append(str(i))
      }
    }
    array(array.map(_.length).indexOf(array.map(_.length).reduceLeft(_ max _)))
  }

  def intReverse(x:Int):Int={
    val flg = if(x < 0) true else false
    var num = if(x < 0) math.abs(x) else x
    var res = 0
    while(num > 0){
      val remainder = num % 10
      num = num / 10
      if (res > Integer.MAX_VALUE/10 || (res == Integer.MAX_VALUE / 10 && remainder > 7)) return 0
      if (res < Integer.MIN_VALUE/10 || (res == Integer.MIN_VALUE / 10 && remainder < -8)) return 0
      res = res * 10 + remainder
    }
    val result = if(flg) -(res) else res
    result
  }

  def isPalindrome(x: Int): Boolean = {
    val flg = if(x < 0) true else false
    var num = if(x < 0) math.abs(x) else x
    var res = 0
    while(num > 0){
      val remainder = num % 10
      num = num / 10
      if (res > Integer.MAX_VALUE/10 || (res == Integer.MAX_VALUE / 10 && remainder > 7)) 0
      if (res < Integer.MIN_VALUE/10 || (res == Integer.MIN_VALUE / 10 && remainder < -8)) 0
      res = res * 10 + remainder
    }
    val result = if(flg) -(res) else res
    val flag = if(result == x) true else false
    flag
  }

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    if(l1 == null)
      return l2
    if(l2 == null)
      return l1
    if(l1._x <= l2._x){
      l1.next = mergeTwoLists(l1.next, l2)
      return l1
    } else {
      l2.next = mergeTwoLists(l2.next, l1)
      return l2
    }
  }

  def removeDuplicates(nums: Array[Int]): Int = {
    var count = 0
    for(i <- 1 until nums.length){
      if(nums(i) != nums(count)) {
        nums(count + 1) = nums(i)
      }
    }
    count+1
  }

  def removeElement(nums: Array[Int], `val`: Int): Int = {
    println(nums.filter(! _.equals(`val`)).toList)
    nums.filter(! _.equals(`val`)).length
  }

  def longestCommonPrefix(strs:Array[String]): String = {
    val sb = new StringBuilder
    if(strs.length >= 1){
      for(i <- 0 until strs.length){

      }
    }
    sb.toString
  }

  def main(args: Array[String]): Unit = {
    val l1 = new ListNode()
    val num = Array(100,4,200,1,3,2)
    val num1 = Array(1,2,3)
    val num2 = Array(3,2,2,3)
    println("Removing element from array :"+ removeElement(num2,3))
    println("@@@@@"+removeDuplicates(num1))
    println(intReverse(1534236469))
    println(isPalindrome(1534236469))
    println(longestConSeq(num))
    println(longestNonRepeatativeSequence("abcahjlu"))
  }
}
class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}
