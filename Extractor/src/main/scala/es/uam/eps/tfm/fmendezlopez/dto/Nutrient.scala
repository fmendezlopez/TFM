package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by Francisco on 23/04/2017.
  */
class Nutrient {

  private var _name : String = _
  private var _amount : Double = _
  private var _unit : String = _
  private var _percent : Double = _

  def name = _name
  def amount = _amount
  def unit = _unit
  def percent = _percent

  def name_= (value : String) : Unit = _name = value
  def amount_= (value : Double) : Unit = _amount = value
  def unit_= (value : String) : Unit = _unit = value
  def percent_= (value : Double) : Unit = _percent = value

  def toSeq() : Seq[Any] = {
    Seq(_amount)
  }
}
object Nutrient{
  def defaultNutrient (name : String) : Nutrient = {
    val result = new Nutrient
    result.name_=(name)
    result.amount_=(0.0)
    result.unit_=("none")
    result.percent_=(0.0)
    result
  }
}
