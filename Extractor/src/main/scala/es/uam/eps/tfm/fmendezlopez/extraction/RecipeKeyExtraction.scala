package es.uam.eps.tfm.fmendezlopez.extraction

import java.util.Properties

import com.github.tototoshi.csv.CSVWriter
import es.uam.eps.tfm.fmendezlopez.dto.RecipeKeyRecipe
import es.uam.eps.tfm.fmendezlopez.scraping.Scraper
import es.uam.eps.tfm.fmendezlopez.utils._
import org.apache.commons.configuration2.Configuration

/**
  * Created by franm on 22/07/2017.
  */
object RecipeKeyExtraction extends Logging{

  private var connectionProperties : Properties = _
  private var properties : Configuration = _
  private var csvRecipes : CSVWriter = _
  private var csvSteps : CSVWriter = _

  def main(args: Array[String]): Unit = {
    properties = PropertiesManager.loadProperties(args(0), PropertiesManager.EXTRACTION_PROPERTIES_FILE)
    connectionProperties = new Properties()
    connectionProperties.put("timeout", properties.getProperty("stage5.scraping.delay.timeout"))
    connectionProperties.put("max-body-size", properties.getProperty("stage5.scraping.maxBodySize"))
    connectionProperties.put("follow-redirects", properties.getProperty("stage5.scraping.followRedirects"))
    connectionProperties.put("host", properties.getProperty("recipekey.host"))
    connectionProperties.put("referrer", "")
    connectionProperties.put("attempts", properties.getProperty("stage5.scraping.maxAttempts"))
    connectionProperties.put("delay-detection", properties.getProperty("stage5.scraping.time.detection"))
    connectionProperties.put("base-host", properties.getProperty("recipekey.url.base"))

    HttpManager.setProperties(properties)

    val cookie = HttpManager.requestRecipeKeyCookie(properties.getString("recipekey.url.base"), connectionProperties)
    connectionProperties.put("cookie", cookie.getOrElse(""))
    HttpManager.setConnectionProperties(connectionProperties)

    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    val outputDir = Utils.resolvePath(5, hostname)

    val outCSVDelimiter = properties.getString("stage5.output.csv.delimiter")
    val csvRecipesName : String = properties.getString("stage5.output.csv.recipes.name")
    val csvStepsName : String = properties.getString("stage5.output.csv.steps.name")

    csvRecipes = CSVManager.openCSVWriter(outputDir, csvRecipesName, outCSVDelimiter.charAt(0), true, "UTF-8")
    csvSteps = CSVManager.openCSVWriter(outputDir, csvStepsName, outCSVDelimiter.charAt(0), true, "UTF-8")

    csvRecipes.writeRow(Utils.headerToSeq(properties.getString("stage5.output.csv.recipes.header"), outCSVDelimiter.charAt(0)))
    csvSteps.writeRow(Utils.headerToSeq(properties.getString("stage5.output.csv.steps.header"), outCSVDelimiter.charAt(0)))
    extractRecipes(outCSVDelimiter)
  }

  def extractRecipes(outCSVDelimiter : String) = {
    val diffMap = Map(
      "HARD" -> properties.getString("recipekey.url.hard"),
      "MEDIUM" -> properties.getString("recipekey.url.medium"),
      "EASY" -> properties.getString("recipekey.url.easy")
    )

    val pagesize = properties.getInt("stage5.scraping.pagesize")
    val nrecipes = properties.getInt("stage5.scraping.recipesperdifficulty")
    val npages = (math.ceil(nrecipes.toDouble / pagesize.toDouble)).toInt

    var id = 0
    diffMap.foreach({case(difficulty, url) =>
      logger.info(s"Processing category ${difficulty}")
      logger.info("Extracting URLs...")
      val urlList : Seq[String] = (1 to npages).flatMap(i => {
        val newURL = url.format(i.toString)
        val ret = HttpManager.requestRecipeKeyURL(newURL, connectionProperties)
        if(ret.isEmpty){
          logger.error(s"Could not retrieve recipe list with URL ${newURL}")
          beforeExit
          System.exit(1)
        }
        val page = ret.get
        val potURLS = Scraper.scrapeRecipeKeyList(page, pagesize)
        if(potURLS.isEmpty){
          logger.error(s"Could not scrape recipe list with URL ${newURL}")
          beforeExit
          System.exit(1)
        }
        val lst = potURLS.get
        logger.info(s"Extracted ${lst.length} URLs for category ${difficulty}")
        lst
      })
      logger.info("Extracting recipes...")
      val recipeList : Seq[RecipeKeyRecipe] = urlList.flatMap(url => {
        val potRecipe = HttpManager.requestRecipeKeyURL(url, connectionProperties)
        if(potRecipe.isEmpty){
          logger.error(s"Could not retrieve recipe with URL ${url}")
          Seq()
        }
        else {
          val ret = Scraper.scrapeRecipeKeyRecipe(potRecipe.get)
          if (ret.isEmpty) {
            logger.error(s"Could not scrape recipe with URL ${url}")
            Seq()
          }
          else {
            val recipe = ret.get
            recipe.id_=(id)
            id += 1
            Seq(recipe)
          }
        }
      })
      logger.info(s"Extracted ${recipeList.length} recipes")
      printRecipeList(recipeList, (csvRecipes, csvSteps))
      logger.info(s"Processed category ${difficulty}")
    })
    beforeExit
  }

  def printRecipeList(recipes : Seq[RecipeKeyRecipe], writers : (CSVWriter, CSVWriter)) : Unit = {

    recipes.foreach(recipe => {
      writers._1.writeRow(recipe.toSeq())
      recipe.steps.foreach({case (number, text) => writers._2.writeRow(Seq(recipe.id, number, text))})
    })
  }

  def beforeExit = {
    CSVManager.closeCSVWriter(csvRecipes)
    CSVManager.closeCSVWriter(csvSteps)
  }

}
