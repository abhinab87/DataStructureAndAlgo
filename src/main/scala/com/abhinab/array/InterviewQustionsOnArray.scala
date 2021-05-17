package com.abhinab.array

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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

  def mergeTwoLists(l1: ListNode1, l2: ListNode1): ListNode1 = {
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

  def missingNumber(num:Array[Int]): Int={
    var missingNum = 0
    var counter = num(0)
    for(i <- 1 until num.length){
      if(counter+1 != num(i)) missingNum = num(i-1)+1
    }
    missingNum
  }

  def alternateThirdMax(num:Array[Int]):Int ={
    var num1 = 0
    var num2 = 0
    var num3 = 0
    for(i <- 0 until num.length){
      if(num(i) > num1){
        num3 = num2
        num2 = num1
        num1 = num(i)
      } else if(num(i) > num2){
        num3 = num2
        num2 = num(i)
      } else if(num(i) > num3)num3 = num(i)
    }
    if(num3 == 0) num1 else num3
  }

  def thirdMax(num:Array[Int]): Int={
    val sortedArray = num.sortWith(_ > _)
    val thirdMax =   if(sortedArray.length < 3) sortedArray(0) else sortedArray(2)
    thirdMax
  }

  def rotatingArray(num:Array[Int], position:Int):Array[Int] ={
    var rotatedArray = num
    for(i <- 0 until position){
      var value = num(i)
      //rotatedArray = value
      rotatedArray = rotatedArray :+ value
    }
    rotatedArray
  }

  /*def moveZerosToRight(num:Array[Int]):Array[Int] ={
    var rotatedArray = new ArrayBuffer[Int]
    for(i <- 0 until num.length){
      if(num(i) == 0)
        rotatedArray += 0
      else
        rotatedArray =+ num(i)
    }
    rotatedArray.toArray
  }*/

  def maxProfit(prices:Array[Int]): Int={
    var currentMax = 0
    var maxSoFar = 0
    for(i <- 1 until prices.length){
      currentMax += prices(i) - prices(i - 1)
      currentMax = math.max(0, currentMax)
      maxSoFar = math.max(currentMax, maxSoFar)
    }
    maxSoFar
  }

  def maxProfitII(prices:Array[Int]):Int ={
    var total = 0
    for(i <- 0 until prices.length - 1){
      if(prices(i + 1)>prices(i))
        total += prices(i + 1) - prices(i)
    }
    total
  }

  def pascalTriangle(row: Int):Unit = {
    for (i <- 0 to row){
      for (j <- 0 to i) {
        print(pascal(j, i) + " ")
      }
      println()
    }
  }

  def pascalTriangleII(row:Int):List[Int]={
    var lst = new ListBuffer[Int]()
    for(i <- 0 to row)
      lst = lst :+ pascal(i,row)
    lst.toList
  }

  def pascal(c:Int, r:Int):Int =  if (c == 0 || c == r) 1   else pascal(c - 1, r - 1) + pascal(c, r - 1)

  def merge(num1:Array[Int], num2:Array[Int]):Array[Int] ={
    var result = Array.fill(num1.length + num2.length)(0)

    var i: Int = 0
    var j: Int = 0

    while (i < num1.length || j < num2.length){
      if (i >= num1.length){
        result(i+j) = num2(j)
        j+=1
      } else if (j >= num2.length || num1(i) <= num2(j)) {
        result(i+j) = num1(i)
        i+=1
      } else {
        result(i+j) = num2(j)
        j+=1
      }
    }
    return result
  }

  def hasCycle(head:ListNode1):Boolean ={
    if(head == null) {
      return false
    } else{
      var walker = head
      var runner = head
      while(runner.next != null && runner.next.next != null){
        walker = walker.next
        runner = runner.next.next
        if(walker == runner) return true
      }
      return false
    }
  }

  def twoSum(nums: List[Int], target: Int): List[List[Int]] =
    nums.combinations(2).collect {
      case couple if couple.sum == target =>
        couple.map(nums.indexOf)
    }.toList

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val ab = new ArrayBuffer[Int]()
    val len1 = nums1.length
    val len2 = nums2.length
    var i = 0
    var j = 0
    while(i < len1 && j < len2){
      if(nums1(i) < nums2(j)){
        ab.append(nums1(i))
        i = i + 1
      } else {
        ab.append(nums2(j))
        j = j + 1
      }
    }
    while(i < len1){
      ab.append(nums1(i))
      i = i + 1
    }
    while(j < len2){
      ab.append(nums2(j))
      j = j + 1
    }
    println("!!!!!!!!"+ab.toList)
    if(ab.length%2 != 0) ab(ab.length/2).toDouble
    else (ab(ab.length/2) + ab(ab.length/2 - 1)) / 2.toDouble
  }

  def rotate(nums: Array[Int], k: Int): Unit = {
    var temp = 0
    for(i <- 0 to k-1){
      temp = nums(nums.length - 1)
      for(j <- nums.length - 1 until 0 by -1){
        nums(j) = nums(j-1)
      }
      nums(0) = temp
    }
  }

  /*TODO
  * Find a triplet that sum to a given value
  * 4SUM

 */

  def main(args: Array[String]): Unit = {
    val l1 = new ListNode1()
    val num = Array(100,4,200,1,3,2)
    val missingNumArray = Array(1,2,3,5)
    val num2 = Array(3,2,2,3)
    println("Removing element from array :"+ removeElement(num2,3))
    println("@@@@@"+removeDuplicates(num2))
    println("Reversal of the number is "+intReverse(1534236469))
    println("The number is palindrome "+isPalindrome(1534236469))
    println("Longest Consecutive Sequence is "+longestConSeq(num))
    println("Longest Non-Repeatative Sequence is "+longestNonRepeatativeSequence("abcahjlu"))
    println("Third Max number in an Array is : "+thirdMax(num))
    println("Third Max number in an Array is : "+alternateThirdMax(num))
    println("Missing number in the array is :"+missingNumber(missingNumArray))
    println("Max profit from selling stocks is: "+ maxProfit(num))
    println("Max profit from selling stocks is: "+ maxProfitII(num))
    println("pascal's Triangle"+pascalTriangle(5))
    println("pascal's Triangle ith row "+pascalTriangleII(5))
    println("merged array is "+merge(Array(1,2,3),Array(0,2,3,4)).toList)
    println("list node has cycle")
    println("two sum on sorted array is "+twoSum(List(1,45,46,78,79),47))
    //println("intersection of two elements are "+intersection(Array(1,2,3),Array(2,2)).toList)
    println("Median of sorted array "+ findMedianSortedArrays(Array(1,2), Array(3,4)))
    println("rotating an Array"+rotate(Array(1,2,3,4,5,6,7),3))
    //println("threeSum of an array is "+threeSum(Array(1,0,-1,2,-4,-1).toList))
    //println(moveZerosToRight(Array(0,1,2,0)).toList)
  }
}