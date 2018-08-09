package es.uam.eps.tfm.fmendezlopez.extraction

import es.uam.eps.tfm.fmendezlopez.dao.DatabaseDAO
import es.uam.eps.tfm.fmendezlopez.dto.UserDTO
import es.uam.eps.tfm.fmendezlopez.utils.Logging
import org.json.JSONObject

import scala.collection.mutable
import scala.util.Try

/**
  * Created by franm on 17/06/2018.
  */
class Frontier(implicit override val ord: Ordering[UserDTO])
  extends mutable.PriorityQueue[UserDTO]
    with Logging
    with StateFull{

  var state: JSONObject = _

  private var current_priority: Int = _
  private var last_priority: Int = _

  override def initialize(state: JSONObject): Unit = {
    val current_priority = state.getInt("current_priority")
    val last_priority = state.getInt("current_priority")
    super.enqueue(DatabaseDAO.getUsers(current_priority).map(user => UserDTO(user, current_priority)) :_*)
    this.current_priority = current_priority
    this.last_priority = last_priority
    this.state = state
  }

  override def getState(): JSONObject = {
    state.put("current_priority", current_priority)
    state.put("last_priority", last_priority)
    state
  }

  override def isEmpty: Boolean = {
    super.isEmpty && DatabaseDAO.emptyQueue
  }
  override def nonEmpty: Boolean = !isEmpty

  @throws[NoSuchElementException]
  def peek: UserDTO = {
    val next_priority = DatabaseDAO.getHeadPriority
    val elem: Either[UserDTO, Seq[UserDTO]] = Try(
      Left(super.head)).getOrElse(Right(Try(DatabaseDAO.getUsers(next_priority).map(user => UserDTO(user, next_priority))).getOrElse(Seq())))
    val e: UserDTO =
      if(elem.isLeft){
      current_priority = elem.left.get.priority
      elem.left.get
      } //todo añadir columna de timestamp y al llamar DatabaseDAO.getUsers ordenar los de esa prioridad por el timestamp
        //todo reducir a 25 followers y 25 following
      else {
        if(elem.right.get.nonEmpty){
          super.enqueue(elem.right.get: _*)
          current_priority = elem.right.get.head.priority
          super.head
        }
        else{
          throw new NoSuchElementException("frontier is empty")
        }
      }
    logger.debug(s"Peeked element $e")
    e
  }

  override def enqueue(elems: UserDTO*): Unit = {

    val e = UserDTO(-1, last_priority)
    elems.foreach(elem => {
      if(ord.lt(elem, e)) last_priority = elem.priority
      DatabaseDAO.insertUser(elem.id, true, elem.priority)
      logger.debug(s"Enqueued element $elem")
    })
    if(super.isEmpty) super.enqueue(elems :_*)
  }

  override def dequeue: UserDTO ={
    val head = peek
    val e = super.dequeue()
    DatabaseDAO.extractUser(head.id)
    logger.debug(s"Dequeued element $e")
    e
  }
}
