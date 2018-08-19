package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by Francisco on 24/04/2017.
  */
class User {

  private var _id : Long = -1
  private var _name : String = ""
  private var _followerCount : Int = 0
  private var _followingCount : Int = 0
  private var _madeitCount : Int = 0
  private var _favCount : Int = 0
  private var _ratingCount : Int = 0
  private var _recipeCount : Int = 0
  private var _reviewCount : Int = 0
  private var _city : String = ""
  private var _region : String = ""
  private var _country : String = ""
  private var _handle : String = ""
  private var _profileUrl : String = ""

  def id = _id
  def name = _name
  def followerCount = _followerCount
  def followingCount = _followingCount
  def madeitCount = _madeitCount
  def favCount = _favCount
  def ratingCount = _ratingCount
  def recipeCount = _recipeCount
  def reviewCount = _reviewCount
  def city = _city
  def region = _region
  def country = _country
  def handle = _handle
  def profileUrl = _profileUrl

  def id_= (value : Long) = _id = value
  def name_=(value : String) = _name = value
  def followerCount_= (value : Int) = _followerCount = value
  def followingCount_= (value : Int) = _followingCount = value
  def madeitCount_= (value : Int) = _madeitCount = value
  def favCount_= (value : Int) = _favCount = value
  def ratingCount_= (value : Int) = _ratingCount = value
  def recipeCount_= (value : Int) = _recipeCount = value
  def reviewCount_= (value : Int) = _reviewCount = value
  def city_= (value : String) = _city = value
  def region_= (value : String) = _region = value
  def country_= (value : String) = _country = value
  def handle_= (value : String) = _handle = value
  def profileUrl_= (value : String) = _profileUrl = value

  def toSeq : Seq[Any] = {
    Seq(_id, _name, _city, _region, _country, _handle, _profileUrl, _followerCount, _followingCount, _madeitCount, _favCount, _ratingCount, _recipeCount, _reviewCount)
  }
}
