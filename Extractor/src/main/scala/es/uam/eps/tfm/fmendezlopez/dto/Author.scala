package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by Francisco on 22/04/2017.
  */
class Author {
  private var _id : Long = -1
  private var _url : String = ""

  def id = _id
  def url = _url

  def id_= (value : Long) : Unit = _id = value
  def url_= (value : String) : Unit = _url = value

  def toSeq() : Seq[Any] = {
    Seq(_id, _url)
  }
}
