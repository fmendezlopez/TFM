package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by Francisco on 23/04/2017.
  */
class Ingredient {
  private var _id : Int = _
  private var _displayValue : String = _
  private var _amount : Double = _

  def id = _id
  def displayValue = _displayValue
  def amount = _amount

  def id_= (value : Int) : Unit = _id = value
  def displayValue_= (value : String) : Unit = _displayValue = value
  def amount_= (value : Double) : Unit = _amount = value

  def toSeq() : Seq[Any] = {
    Seq(_id, _displayValue, _amount)
  }
}
