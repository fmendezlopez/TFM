package es.uam.eps.tfm.fmendezlopez


import es.uam.eps.tfm.fmendezlopez.dao.DatabaseDAO
import es.uam.eps.tfm.fmendezlopez.dto.UserDTO

import scala.collection.mutable
import scala.util.Try

/**
  * Created by franm on 17/06/2018.
  */
class FrontierTest(
                implicit override val ord: Ordering[UserDTO],
                implicit val priority: Int
              )
  extends mutable.PriorityQueue[UserDTO]{

  private var max_priority: Int = _

  def peek(): UserDTO = super.head

  override def enqueue(elems: UserDTO*): Unit = super.enqueue(elems :_*)

  override def dequeue: UserDTO = super.dequeue()

}
