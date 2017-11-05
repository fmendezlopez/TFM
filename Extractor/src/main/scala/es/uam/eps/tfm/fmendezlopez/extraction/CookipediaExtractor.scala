package es.uam.eps.tfm.fmendezlopez.extraction

import java.util.Properties

import com.github.tototoshi.csv.CSVWriter
import es.uam.eps.tfm.fmendezlopez.dto.CookipediaRecipe
import es.uam.eps.tfm.fmendezlopez.scraping.Scraper
import es.uam.eps.tfm.fmendezlopez.utils._
import org.apache.commons.configuration2.Configuration

/**
  * Created by franm on 10/09/2017.
  */
object CookipediaExtractor extends Logging{

  private var connectionProperties : Properties = _
  private var properties : Configuration = _
  private var csvRecipes : CSVWriter = _
  private var csvSteps : CSVWriter = _
  private var csvIngredients : CSVWriter = _

  def main(args: Array[String]): Unit = {
    properties = PropertiesManager.loadProperties(args(0), PropertiesManager.EXTRACTION_PROPERTIES_FILE)
    connectionProperties = new Properties()
    connectionProperties.put("timeout", properties.getProperty("stage6.scraping.delay.timeout"))
    connectionProperties.put("max-body-size", properties.getProperty("stage6.scraping.maxBodySize"))
    connectionProperties.put("follow-redirects", properties.getProperty("stage6.scraping.followRedirects"))
    connectionProperties.put("host", properties.getProperty("cookipedia.host"))
    connectionProperties.put("referrer", "")
    connectionProperties.put("attempts", properties.getProperty("stage6.scraping.maxAttempts"))
    connectionProperties.put("delay-detection", properties.getProperty("stage6.scraping.time.detection"))
    connectionProperties.put("base-host", properties.getProperty("cookipedia.url.base"))
    connectionProperties.put("base-difficulty", properties.getProperty("cookipedia.url.base.difficulty"))
    connectionProperties.put("base-recipe", properties.getProperty("cookipedia.url.base"))

    HttpManager.setProperties(properties)

    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    val outputDir = Utils.resolvePath(6, hostname)

    val outCSVDelimiter = properties.getString("stage6.output.csv.delimiter")
    val csvRecipesName : String = properties.getString("stage6.output.csv.recipes.name")
    val csvStepsName : String = properties.getString("stage6.output.csv.steps.name")
    val csvIngredientsName : String = properties.getString("stage6.output.csv.ingredients.name")

    csvRecipes = CSVManager.openCSVWriter(outputDir, csvRecipesName, outCSVDelimiter.charAt(0), true, "UTF-8")
    csvSteps = CSVManager.openCSVWriter(outputDir, csvStepsName, outCSVDelimiter.charAt(0), true, "UTF-8")
    csvIngredients = CSVManager.openCSVWriter(outputDir, csvIngredientsName, outCSVDelimiter.charAt(0), true, "UTF-8")

    csvRecipes.writeRow(Utils.headerToSeq(properties.getString("stage6.output.csv.recipes.header"), outCSVDelimiter.charAt(0)))
    csvSteps.writeRow(Utils.headerToSeq(properties.getString("stage6.output.csv.steps.header"), outCSVDelimiter.charAt(0)))
    csvIngredients.writeRow(Utils.headerToSeq(properties.getString("stage6.output.csv.ingredients.header"), outCSVDelimiter.charAt(0)))
    val diffMap = Map(
      "HARD" -> properties.getString("cookipedia.url.hard"),
      "MEDIUM" -> properties.getString("cookipedia.url.medium"),
      "EASY" -> properties.getString("cookipedia.url.easy")
    )

    val urlMap = extractURLs(diffMap)
    extractRecipes(outCSVDelimiter, urlMap)
  }

  def extractURLs(diffMap : Map[String, String]) : Map[String, Seq[String]] = {
    var result : Map[String, Seq[String]] = Map()
    val maxrecipes = properties.getInt("stage6.scraping.recipesperdifficulty")
    var id = 0
    val base_recipe_url = connectionProperties.getProperty("base-recipe")
    diffMap.foreach({case (difficulty, url) =>
      logger.info(s"Processing category ${difficulty}")
      logger.info("Extracting URLs...")
      var nrecipes = maxrecipes
      var continue = true
      val page = url
      var urls : Seq[String] = Seq()
      do{
        val html = HttpManager.requestURL(page, connectionProperties)
        if(html.isEmpty){
          logger.error(s"Could not retrieve recipe list with URL ${page}")
          beforeExit
          System.exit(1)
        }
        val list = Scraper.scrapeCookipediaURLs(html.get, base_recipe_url, nrecipes)
        urls ++= list
        nrecipes -= list.length
        continue = nrecipes > 0
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
        val potRecipe = Scraper.scrapeCookipediaRecipe(page, '|')
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
