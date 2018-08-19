package es.uam.eps.tfm.fmendezlopez.utils

import java.io.File
import java.sql.SQLException

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
    val datasetPath = args(1) + File.separator

    //Load properties
    val properties = PropertiesManager.loadProperties(configPath, PropertiesManager.EXTRACTION_PROPERTIES_FILE)

    val dataset = DatasetSQLDAO

    dataset.initialize(properties)
    dataset.connect()
    val prop: String => String = str => properties.getString(str)
    val csvDelimiter = prop("stage4.stage1.output.csv.delimiter").charAt(0)

    val readers = Map(
      "categories" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.categories.filename")}"), csvDelimiter),
      "categories_rel" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.category_hierarchy.filename")}"), csvDelimiter),
      "favourites" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.favourites.filename")}"), csvDelimiter),
      "fellowship" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.fellowship.filename")}"), csvDelimiter),
      "ingredients" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.ingredients.filename")}"), csvDelimiter),
      "madeit" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.madeit.filename")}"), csvDelimiter),
      "nutrition" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.nutrition.filename")}"), csvDelimiter),
      "publications" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.publications.filename")}"), csvDelimiter),
      "recipes" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.recipes.filename")}"), csvDelimiter),
      "reviews" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.reviews.filename")}"), csvDelimiter),
      "similar" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.similar.filename")}"), csvDelimiter),
      "steps" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.steps.filename")}"), csvDelimiter),
      "users" -> CSVManager.openCSVReader(new  File(s"$datasetPath${prop("stage4.stage1.dataset.users.filename")}"), csvDelimiter)
    )
    readers.values.foreach(reader => CSVManager.skipLines(reader, 1))

    val tables: Map[String, String] = Map(
      "categories" -> prop("stage4.stage1.database.categories.tablename"),
      "categories_rel" -> prop("stage4.stage1.database.category_hierarchy.tablename"),
      "favourites" -> prop("stage4.stage1.database.favourites.tablename"),
      "fellowship" -> prop("stage4.stage1.database.fellowship.tablename"),
      "ingredients" -> prop("stage4.stage1.database.ingredients.tablename"),
      "madeit" -> prop("stage4.stage1.database.madeit.tablename"),
      "nutrition" -> prop("stage4.stage1.database.nutrition.tablename"),
      "publications" -> prop("stage4.stage1.database.publications.tablename"),
      "recipes" -> prop("stage4.stage1.database.recipes.tablename"),
      "reviews" -> prop("stage4.stage1.database.reviews.tablename"),
      "similar" -> prop("stage4.stage1.database.similar.tablename"),
      "steps" -> prop("stage4.stage1.database.steps.tablename"),
      "users" -> prop("stage4.stage1.database.users.tablename")
    )

    try {
      dataset.beginTransaction()
      dataset.insertData(tables("categories"), readers("categories").all(),
        properties.getString("stage4.stage1.database.categories.stringfields").split(',').map(_.toInt))
      /*
      readers("recipes").all()
        .foreach(line => dataset.insertData(tables("recipes"), line, properties.getString("stage4.stage1.database.recipes.stringfields").split(',').map(_.toInt)))
      readers("users").all()
        .foreach(line => dataset.insertData(tables("users"), line, properties.getString("stage4.stage1.database.users.stringfields").split(',').map(_.toInt)))
      readers("favourites").all()
        .foreach(line =>
          dataset.insertData(tables("favourites"), line)
        )
      readers("madeit").all()
        .foreach(line => dataset.insertData(tables("madeit"), line))
      readers("publications").all()
        .foreach(line => dataset.insertData(tables("publications"), line))
      readers("reviews").all()
        .foreach(line => dataset.insertData(tables("reviews"), line, properties.getString("stage4.stage1.database.reviews.stringfields").split(',').map(_.toInt)))
      readers("steps").all()
        .foreach(line => dataset.insertData(tables("steps"), line, properties.getString("stage4.stage1.database.steps.stringfields").split(',').map(_.toInt)))
      readers("similar").all()
        .foreach(line => dataset.insertData(tables("similar"), line))
      readers("nutrition").all()
        .foreach(line => dataset.insertData(tables("nutrition"), line))
      readers("ingredients").all()
        .foreach(line => dataset.insertData(tables("ingredients"), line, properties.getString("stage4.stage1.database.ingredients.stringfields").split(',').map(_.toInt)))
      readers("fellowship").all()
        .foreach(line => dataset.insertData(tables("fellowship"), line))
        */
      dataset.rollback()
    } catch {
      case sql: SQLException => {
        dataset.rollback()
      }
    }
    dataset.endTransaction()
    readers.foreach(_._2.close())
    dataset.disconnect()
  }

  def printHelp = {
    println("Usage:")
    println("\targ1: configuration path")
    println("\targ2: dataset path")
  }
}
