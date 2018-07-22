package es.uam.eps.tfm.fmendezlopez.extraction

import org.json.JSONObject

/**
  * Created by franm on 20/07/2018.
  */
trait StateFull {

  def state: JSONObject

  def initialize(state: JSONObject)

  def getState: JSONObject

}
