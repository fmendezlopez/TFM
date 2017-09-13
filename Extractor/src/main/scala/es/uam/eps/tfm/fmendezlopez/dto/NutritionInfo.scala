package es.uam.eps.tfm.fmendezlopez.dto

/**
  * Created by Francisco on 23/04/2017.
  */
class NutritionInfo {
  private var _nutrients : Map[String, Nutrient] = _

  def nutrients = _nutrients

  def nutrients_= (value : Map[String, Nutrient]) : Unit = _nutrients = value

  def toSeq() : Seq[Any] = {
    var result : Seq[Seq[Any]] = Seq()
    _nutrients.values.foreach(value => result :+= value.toSeq())
    result
  }
}
