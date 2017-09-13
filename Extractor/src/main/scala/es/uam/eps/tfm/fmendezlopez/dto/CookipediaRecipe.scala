package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by franm on 11/09/2017.
  */
class CookipediaRecipe {

  private var _id : Long = -1
  private var _name : String = ""
  private var _ingredients : Seq[String] = Seq()
  private var _ingredientsnumber : Int = 0
  private var _stepsnumber : Int = 0
  private var _steps : Seq[(Int, String)] = Seq()
  private var _prepTime : Int = 0
  private var _cookTime : Int = 0
  private var _totalTime : Int = 0
  private var _difficulty : String = ""

  def id = _id
  def name = _name
  def ingredients = _ingredients
  def ingredientsnumber = _ingredientsnumber
  def stepsnumber = _stepsnumber
  def steps = _steps
  def prepTime = _prepTime
  def cookTime = _cookTime
  def totalTime = _totalTime
  def difficulty = _difficulty

  def id_= (value : Long) : Unit = _id = value
  def name_=(value : String) : Unit = _name = value
  def ingredients_= (value : Seq[String]) : Unit = _ingredients = value
  def ingredientsnumber_= (value : Int) : Unit = _ingredientsnumber = value
  def steps_= (value : Seq[(Int, String)]) : Unit = _steps = value
  def stepsnumber_=(value : Int) : Unit = _stepsnumber = value
  def prepTime_= (value : Int) : Unit = _prepTime = value
  def cookTime_= (value : Int) : Unit = _cookTime = value
  def totalTime_= (value : Int) : Unit = _totalTime = value
  def difficulty_= (value : String) : Unit = _difficulty = value

  def toSeq() : Seq[Any] = {
    Seq(_id, _name, _prepTime, _cookTime, _totalTime, _ingredientsnumber, _stepsnumber, _difficulty)
  }
}
