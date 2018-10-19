package com.abhinab.array

class ListNode1(var _x: Int = 0) {
  var next: ListNode1 = null
  var x: Int = _x
  override def toString(): String = "(" + x +")"
}
