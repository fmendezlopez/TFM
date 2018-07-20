package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by franm on 23/06/2018.
  */
case class UserDTO(id: Long, priority: Int) {
  override def toString: String = s"ID: $id; Priority: $priority"
}
