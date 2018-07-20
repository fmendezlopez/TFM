package es.uam.eps.tfm.fmendezlopez

import es.uam.eps.tfm.fmendezlopez.dto.UserDTO

/**
  * Created by franm on 10/07/2018.
  */
object Tests {

  def main(args: Array[String]): Unit = {
    //Load frontier
    class FrontierOrdering extends Ordering[UserDTO] {
      def compare(x: UserDTO, y: UserDTO): Int = y.priority compare x.priority
    }
    implicit var priority = 0
    implicit val sorter: FrontierOrdering = new FrontierOrdering
    val frontier: FrontierTest = new FrontierTest

    val e0 = UserDTO(1, 0)
    val e1 = UserDTO(2, 1)
    val e2 = UserDTO(3, 2)
    val e3 = UserDTO(4, 3)
    val e4 = UserDTO(5, 4)
    val e5 = UserDTO(6, 3)

    frontier.enqueue(e0, e1, e2, e3)

    println(frontier.peek().id)
    println(frontier.dequeue().id)
  }
}
