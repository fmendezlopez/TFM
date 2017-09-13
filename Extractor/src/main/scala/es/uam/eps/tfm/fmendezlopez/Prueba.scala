package es.uam.eps.tfm.fmendezlopez

import java.io.File
import java.sql.{Date, SQLException}
import java.text.{DateFormat, SimpleDateFormat}
import java.util.regex.Pattern
import java.util.{Calendar, Locale, StringTokenizer}

import org.json.{JSONArray, JSONObject}
import es.uam.eps.tfm.fmendezlopez.dao._
import es.uam.eps.tfm.fmendezlopez.utils.CSVManager


/**
  * Created by Francisco on 13/04/2017.
  */
object Prueba {

  def main(args: Array[String]): Unit = {
    //println(detectRecipeID("http://allrecipes.com/recipe/255587/naked-cherry-tomato-salad/"))
    //println(detectRecipeID("http://allrecipes.com/recipe/naked-cherry-tomato-salad/44534543"))
    val s1 = Set(1, 3, 4)
    val s2 = Set(1, 3, 5)
    val s3 = s1 ++ s2
    s3.foreach(println)
  }

  def detectRecipeID(url : String) : Option[Long] = {
    val tokenizer = new StringTokenizer(url)
    tokenizer.nextToken(".")
    var continue = true
    var number : Option[Long] = None
    do {
      val token = tokenizer.nextToken("/")

      try{
        number = Some(token.toLong)
      } catch{
        case nfe : NumberFormatException =>
          number = None
      }
      continue = number.isEmpty && tokenizer.hasMoreTokens
    } while(continue)
    number
  }
  @throws(classOf[NumberFormatException])
  def extractRecipeID2(url : String) : Long = {
    val tokenizer = new StringTokenizer(url)
    tokenizer.nextToken(".")
    tokenizer.nextToken("/")
    tokenizer.nextToken("/")
    tokenizer.nextToken("/")
    tokenizer.nextToken("/").toLong
  }

  def insertRecipes = {
    val dao = DatabaseDAO.getInstance()
    dao.connect()
    val csv = CSVManager.openCSVReaderLines(
      new File("C:\\Users\\franm\\Desktop\\exe\\extractor_jar\\Extractor\\output\\stage4\\1\\FRAN-LAPTOP\\recipes.csv"),
      '|',
      1
    )
    var inserts = 0
    var repeated = 0
    var minus = 0
    csv.foreach(line => {
      val id = line(2)
      try {
        if(id == "-1")
          minus += 1
        if(dao.existsRecipe(id)){
          println(s"Recipe ${id} already exists")
          repeated += 1
        }
        else{
          dao.insertRecipe(id)
          inserts += 1
        }
      } catch {
          case sql : SQLException =>
          println(sql.getMessage)
      }
    })
    println(s"${inserts} insertions")
    println(s"${repeated} repeated")
    println(s"${minus} recipes with id -1")
  }

  def pruebaDB = {
    val dao = DatabaseDAO.getInstance()
    import java.io.IOException
    import java.sql.SQLException
    val id = "8601924"

    try {
      dao.connectAndCreate
      println(dao.existsUser(id))
      dao.insertUser(id)
    } catch {
      case e@(_: InstantiationException | _: IllegalAccessException | _: ClassNotFoundException | _: SQLException) =>
        println(e.getMessage)
        try
          dao.disconnect
        catch {
          case e1@(_: SQLException | _: IOException) =>
            println(e1.getMessage)
        }
    }

    try {
      dao.connectAndCreate
      println(dao.existsRecipe("241310"))
    } catch {
      case e@(_: InstantiationException | _: IllegalAccessException | _: ClassNotFoundException | _: SQLException) =>
        println(e.getMessage)
        try
          dao.disconnect
        catch {
          case e1@(_: SQLException | _: IOException) =>
            println(e1.getMessage)
        }
    }
  }

  def prueba1 = {
    val str = "<strong>1.</strong> Day 1: \n<br>1. Cut up oranges, grapefruit and lemon in small pieces. \n<br>2. Leave in pot of water for 24 hours. \n<br>\n<br>\n<strong>2.</strong>Day 2: \n<br>1. Put pot on stove and boil for half an hour. \n<br>2. Set aside for 24 hours. \n<br>\n<br>\n<strong>3.</strong>Day 3: \n<br>1. Put pot back on stove and boil for half an hour. \n<br>2. Add sugar and leave for 24 hours. \n<br>\n<br>\n<strong>4.</strong>Day 4: \n<br>1. Simmer on low flame until marmalade is thick (3-4 hours). \n<br>2. Pour into jars. Seal lids. \n<br>\n<br>This stuff is great to roast chicken with."
    val regex = Pattern.compile("[a-zA-Z-]+")
    var continue = true
    var prev = 0
    var curr = 0
    while(continue){
      curr = str.indexOf("<br>", prev)
      curr match {
        case -1 => continue = false
        case _ =>
          val token = str.substring(prev, curr)
          if(!token.contains("strong")){
            val str1 = token.replaceAll("\n", "").trim
            if(!str1.isEmpty){
              println(s"valid token: ${str1}")
            }
          }
          prev = curr + 4
      }
    }
  }

  def prueba2 = {

  }
}
