package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by franm on 10/07/2017.
  */
class UserRecipe {

  private var _id : Long = -1
  private var _webURL : String = ""
  private var _apiURL : String = ""

  def id = _id
  def webURL = _webURL
  def apiURL = _apiURL

  def id_=(value : Long) = _id = value
  def webURL_=(value : String) = _webURL = value
  def apiURL_=(value : String) = _apiURL = value
}
