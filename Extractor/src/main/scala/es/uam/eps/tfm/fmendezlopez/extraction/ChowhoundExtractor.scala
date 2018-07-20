package es.uam.eps.tfm.fmendezlopez.extraction

import java.io.{File, FilenameFilter}
import java.util.Properties

import com.github.tototoshi.csv.CSVWriter
import es.uam.eps.tfm.fmendezlopez.dto.CookipediaRecipe
import es.uam.eps.tfm.fmendezlopez.scraping.Scraper
import es.uam.eps.tfm.fmendezlopez.utils._
import org.apache.commons.configuration2.Configuration

/**
  * Created by franm on 10/09/2017.
  */
object ChowhoundExtractor extends Logging{

  private var connectionProperties : Properties = _
  private var properties : Configuration = _
  private var csvRecipes : CSVWriter = _
  private var csvSteps : CSVWriter = _
  private var csvIngredients : CSVWriter = _

  def main(args: Array[String]): Unit = {
    properties = PropertiesManager.loadProperties(args(0), PropertiesManager.EXTRACTION_PROPERTIES_FILE)
    connectionProperties = new Properties()
    connectionProperties.put("timeout", properties.getProperty("stage8.scraping.delay.timeout"))
    connectionProperties.put("max-body-size", properties.getProperty("stage8.scraping.maxBodySize"))
    connectionProperties.put("follow-redirects", properties.getProperty("stage8.scraping.followRedirects"))
    connectionProperties.put("host", properties.getProperty("chowhound.host"))
    connectionProperties.put("referrer", "")
    connectionProperties.put("attempts", properties.getProperty("stage8.scraping.maxAttempts"))
    connectionProperties.put("delay-detection", properties.getProperty("stage8.scraping.time.detection"))
    connectionProperties.put("base-host", properties.getProperty("chowhound.url.base"))
    connectionProperties.put("base-recipe", properties.getProperty("chowhound.url.base.recipe"))

    HttpManager.setProperties(properties)

    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    val outputDir = Utils.resolvePath(7, hostname)

    val outCSVDelimiter = properties.getString("stage8.output.csv.delimiter")
    val csvRecipesName : String = properties.getString("stage8.output.csv.recipes.name")
    val csvStepsName : String = properties.getString("stage8.output.csv.steps.name")
    val csvIngredientsName : String = properties.getString("stage8.output.csv.ingredients.name")

    csvRecipes = CSVManager.openCSVWriter(outputDir, csvRecipesName, outCSVDelimiter.charAt(0), true, "UTF-8")
    csvSteps = CSVManager.openCSVWriter(outputDir, csvStepsName, outCSVDelimiter.charAt(0), true, "UTF-8")
    csvIngredients = CSVManager.openCSVWriter(outputDir, csvIngredientsName, outCSVDelimiter.charAt(0), true, "UTF-8")

    csvRecipes.writeRow(Utils.headerToSeq(properties.getString("stage8.output.csv.recipes.header"), outCSVDelimiter.charAt(0)))
    csvSteps.writeRow(Utils.headerToSeq(properties.getString("stage8.output.csv.steps.header"), outCSVDelimiter.charAt(0)))
    csvIngredients.writeRow(Utils.headerToSeq(properties.getString("stage8.output.csv.ingredients.header"), outCSVDelimiter.charAt(0)))
    val diffMap = getFiles(args(1))

    val urlMap = extractURLs(diffMap)
    extractRecipes(outCSVDelimiter, urlMap)
  }

  def getFiles(path : String) : Map[String, Seq[File]] = {
    val dir = new File(path)
    def list(diff : String) : Seq[File] = dir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.contains(diff)
    })
    Map(
      "HARD" -> list("hard"),
      "MEDIUM" -> list("medium"),
      "EASY" -> list("easy")
    )
  }

  def extractURLs(diffMap : Map[String, Seq[File]]) : Map[String, Seq[String]] = {
    var result : Map[String, Seq[String]] = Map()
    val maxrecipes = properties.getInt("stage8.scraping.recipesperdifficulty")
    var id = 0
    diffMap.foreach({case (difficulty, list) =>
      logger.info(s"Processing category ${difficulty}")
      logger.info("Extracting URLs...")
      var nrecipes = maxrecipes
      var continue = true
      var urls : Seq[String] = Seq()
      val it = list.iterator
      do{
        val file = it.next()
        val list = Scraper.scrapeChowhoundURLs(file, nrecipes)
        urls ++= list
        nrecipes -= list.length
        continue = nrecipes > 0 && it.hasNext
      } while(continue)
      result += (difficulty -> urls)
    })
    result
  }

  def extractRecipes(outCSVDelimiter : String, urls : Map[String, Seq[String]]) : Unit = {
    var id = 0
    val diffWordMap = Map(
      "HARD" -> properties.getString("general.attribute.recipe.difficulty.hard"),
      "MEDIUM" -> properties.getString("general.attribute.recipe.difficulty.medium"),
      "EASY" -> properties.getString("general.attribute.recipe.difficulty.easy")
    )
    urls.foreach({case(difficulty, urlList) =>
      logger.info(s"Processing category ${difficulty}")
      logger.info("Extracting recipes...")
      val diffWord = diffWordMap(difficulty)
      val recipeList : Seq[CookipediaRecipe] = urlList.flatMap(url => {
        val ret = HttpManager.requestURL(url, connectionProperties)
        if(ret.isEmpty){
          logger.error(s"Could not retrieve recipe list with URL ${url}")
          beforeExit
          System.exit(1)
        }
        val page = ret.get
        logger.info(s"Scraping recipe with URL ${url}")
        val potRecipe = Scraper.scrapeChowhoundRecipe(page, '|')
        if(potRecipe.isEmpty){
          logger.error(s"Could not scrape recipe with URL ${url}")
          Seq()
        }
        else {
          val recipe = potRecipe.get
          recipe.id_=(id)
          recipe.difficulty_=(diffWord)
          id += 1
          Seq(recipe)
        }
      })
      logger.info(s"Extracted ${recipeList.length} recipes")
      printRecipeList(recipeList, (csvRecipes, csvSteps, csvIngredients))
      logger.info(s"Processed category ${difficulty}")
    })
    beforeExit
  }

  def printRecipeList(recipes : Seq[CookipediaRecipe], writers : (CSVWriter, CSVWriter, CSVWriter)) : Unit = {

    recipes.foreach(recipe => {
      writers._1.writeRow(recipe.toSeq())
      recipe.steps.foreach({case (number, text) => writers._2.writeRow(Seq(recipe.id, number, text))})
      recipe.ingredients.foreach({case (text) => writers._3.writeRow(Seq(recipe.id, text))})
    })
  }

  def beforeExit = {
    CSVManager.closeCSVWriter(csvRecipes)
    CSVManager.closeCSVWriter(csvSteps)
    CSVManager.closeCSVWriter(csvIngredients)
  }
}
