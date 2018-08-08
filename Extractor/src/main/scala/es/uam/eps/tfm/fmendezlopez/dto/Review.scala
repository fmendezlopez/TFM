package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by Francisco on 22/04/2017.
  */
class Review {

  private var _id : Long = -1
  private var _rating : Int = -1
  private var _text : String = ""
  private var _author : Author = null
  private var _date : String = ""
  private var _helpfulCount : Int = -1
  private var _recipe : Recipe = null

  def id = _id
  def rating = _rating
  def text = _text
  def author = _author
  def date = _date
  def helpfulCount = _helpfulCount
  def recipe = _recipe

  def id_= (value : Long) = _id = value
  def rating_= (value : Int) = _rating = value
  def text_= (value : String) = _text = value
  def author_= (value : Author) = _author = value
  def date_= (value : String) = _date = value
  def helpfulCount_= (value : Int) = _helpfulCount = value
  def recipe_= (value : Recipe) = _recipe = value

  def toSeq() : Seq[Any] = {
    Seq(_id, _recipe.id, _author.id, _rating, _text, _date, _helpfulCount)
  }

  def canEqual(a: Any) = a.isInstanceOf[Review]
  override def equals(that: Any): Boolean =
    that match {
      case that: Review => that.canEqual(this) && this.id == that.id
      case _ => false
    }
}
