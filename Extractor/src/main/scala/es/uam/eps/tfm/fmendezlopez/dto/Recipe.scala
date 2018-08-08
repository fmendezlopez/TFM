package es.uam.eps.tfm.fmendezlopez.dto

import java.util.Properties

import scala.collection.mutable

/**
  * Created by Francisco on 14/04/2017.
  */
class Recipe {

  private var _id : Long = -1
  private var _title : String = ""
  private var _apiurl : String = ""
  private var _weburl : String = ""
  private var _author : Author = new Author
  private var _rating : Double = 0.0
  private var _ratingCount : Int = 0
  private var _reviewCount : Int = 0
  private var _madeit : Int = 0
  private var _description : String = ""
  private var _servings : Int = 0
  private var _nutritionInfo : Option[NutritionInfo] = _
  private var _ingredients : Seq[Ingredient] = Seq()
  private var _prepTime : Int = 0
  private var _cookTime : Int = 0
  private var _totalTime : Int = 0
  private var _steps : Seq[(Int, String)] = Seq()
  private var _notes : Seq[String] = Seq()
  private var _reviews : Seq[Review] = Seq()
  private var _category : RecipeCategory = null
  private var _similarRecipes: Seq[Long] = Seq()

  def id = _id
  def title = _title
  def apiurl = _apiurl
  def weburl = _weburl
  def author = _author
  def rating = _rating
  def ratingCount = _ratingCount
  def reviewCount = _reviewCount
  def madeit = _madeit
  def description = _description
  def servings = _servings
  def nutritionInfo = _nutritionInfo
  def ingredients = _ingredients
  def prepTime = _prepTime
  def cookTime = _cookTime
  def totalTime = _totalTime
  def steps = _steps
  def notes = _notes
  def reviews = _reviews
  def category = _category
  def similarRecipes = _similarRecipes

  def id_= (value : Long) : Unit = _id = value
  def title_= (value : String) : Unit = _title = value
  def apiurl_= (value : String) : Unit = _apiurl = value
  def weburl_= (value : String) : Unit = _weburl = value
  def author_= (value : Author) : Unit = _author = value
  def rating_= (value : Double) : Unit = _rating = value
  def ratingCount_= (value : Int) : Unit = _ratingCount = value
  def reviewCount_= (value : Int) : Unit = _reviewCount = value
  def madeit_= (value : Int) : Unit = _madeit = value
  def description_= (value : String) : Unit = _description = value
  def servings_= (value : Int) : Unit = _servings = value
  def nutritionInfo_= (value : Option[NutritionInfo]) : Unit = _nutritionInfo = value
  def ingredients_= (value : Seq[Ingredient]) : Unit = _ingredients = value
  def prepTime_= (value : Int) : Unit = _prepTime = value
  def cookTime_= (value : Int) : Unit = _cookTime = value
  def totalTime_= (value : Int) : Unit = _totalTime = value
  def steps_= (value : Seq[(Int, String)]) : Unit = _steps = value
  def notes_= (value : Seq[String]) : Unit = _notes = value
  def reviews_= (value : Seq[Review]) : Unit = _reviews = value
  def category_= (value : RecipeCategory) : Unit = _category = value
  def similarRecipes_=(value: Seq[Long]) : Unit = _similarRecipes = value

  def toSeq() : Seq[Any] = {
    var result : Seq[Any] = Seq(_id, category.id, _title, _rating, _ratingCount, _reviewCount, _madeit, _description, _servings, _prepTime, _cookTime, _totalTime, _weburl)
    result ++ notes
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Recipe]

  override def equals(other: Any): Boolean = other match {
    case that: Recipe =>
      (that canEqual this) &&
        _id == that._id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(_id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Recipe{
  def isValidRecipeID(id : Long) : Boolean = {
    !Seq(0.toLong, -1.toLong).contains(id)
  }
}
