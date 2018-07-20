package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by franm on 09/07/2017.
  */
class RecipeCategory {

  private var _id : Long = -1
  private var _url : String = ""

  def id = _id
  def url = _url

  def id_= (value : Long) = _id = value
  def url_= (value : String) = _url = value


}
