package com.abhinab.linkedlist

object InterviewQuestionOnLinkedList {

  def removeElements(lnkdlst:ListNode[Int],target:Int):ListNode[Int] = if(lnkdlst.h == target) lnkdlst.t else lnkdlst

  def main(args: Array[String]): Unit = {
    val ln = new ListNode[Int](2,ListNode[Int](3, null))
    println(ln.toString)
    val ln1 = ListNode("")
    println("After removing element from listNode "+removeElements(ln,2).toString)
  }
}
