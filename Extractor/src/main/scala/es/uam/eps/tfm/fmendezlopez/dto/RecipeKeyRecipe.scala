package es.uam.eps.tfm.fmendezlopez.dto

import scala.collection.mutable

/**
  * Created by franm on 23/07/2017.
  */
class RecipeKeyRecipe {

  private var _id : Long = -1
  private var _name : String = ""
  private var _mealtype : String = ""
  private var _cuisine : String = ""
  private var _ingredientsnumber : Int = 0
  private var _stepsnumber : Int = 0
  private var _steps : Seq[(Int, String)] = Seq()
  private var _prepTime : Int = 0
  private var _cookTime : Int = 0
  private var _totalTime : Int = 0
  private var _difficulty : String = ""
  private var _method : String = ""

  def id = _id
  def name = _name
  def mealtype = _mealtype
  def cuisine = _cuisine
  def ingredientsnumber = _ingredientsnumber
  def stepsnumber = _stepsnumber
  def steps = _steps
  def prepTime = _prepTime
  def cookTime = _cookTime
  def totalTime = _totalTime
  def difficulty = _difficulty
  def method = _method

  def id_= (value : Long) : Unit = _id = value
  def name_=(value : String) : Unit = _name = value
  def mealtype_= (value : String) : Unit = _mealtype = value
  def cuisine_= (value : String) : Unit = _cuisine = value
  def ingredientsnumber_= (value : Int) : Unit = _ingredientsnumber = value
  def steps_= (value : Seq[(Int, String)]) : Unit = _steps = value
  def stepsnumber_=(value : Int) : Unit = _stepsnumber = value
  def prepTime_= (value : Int) : Unit = _prepTime = value
  def cookTime_= (value : Int) : Unit = _cookTime = value
  def totalTime_= (value : Int) : Unit = _totalTime = value
  def difficulty_= (value : String) : Unit = _difficulty = value
  def method_=(value : String) : Unit = _method = value

  def toSeq() : Seq[Any] = {
    Seq(_id, _name, _mealtype, _prepTime, _cookTime, _totalTime, _cuisine, _ingredientsnumber, _stepsnumber, _difficulty, _method)
  }
}
