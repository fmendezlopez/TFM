package es.uam.eps.tfm.fmendezlopez.utils

import es.uam.eps.tfm.fmendezlopez.dao.DatasetSQLDAO

/**
  * Created by franm on 01/08/2018.
  */
object CSVtoSQL {

  def main(args: Array[String]): Unit = {

    if(args.length != 2){
      printHelp
      System.exit(1)
    }
    val configPath = args(0)
    val datasetPath = args(1)

    //Load properties
    val properties = PropertiesManager.loadProperties(configPath, PropertiesManager.EXTRACTION_PROPERTIES_FILE)

    val dataset = DatasetSQLDAO

    dataset.initialize(properties)
    dataset.connect()

    val filesToTableMap: Map[String, String] = Map(

    )

    dataset.disconnect()
  }

  def printHelp = {
    println("Usage:")
    println("\targ1: configuration path")
    println("\targ2: dataset path")
  }
}
