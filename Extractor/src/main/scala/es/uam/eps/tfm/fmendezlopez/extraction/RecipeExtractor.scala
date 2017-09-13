package es.uam.eps.tfm.fmendezlopez.extraction

import java.io.File
import java.util.Properties

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import es.uam.eps.tfm.fmendezlopez.dto.{Recipe, Review}
import es.uam.eps.tfm.fmendezlopez.exception.ScrapingDetectionException
import es.uam.eps.tfm.fmendezlopez.scraping.Scraper
import es.uam.eps.tfm.fmendezlopez.utils._
import es.uam.eps.tfm.fmendezlopez.dto.{Recipe, Review}
import es.uam.eps.tfm.fmendezlopez.exception.ScrapingDetectionException
import es.uam.eps.tfm.fmendezlopez.scraping.Scraper
import es.uam.eps.tfm.fmendezlopez.utils._
import org.apache.commons.configuration2.Configuration
import org.json.{JSONArray, JSONObject}

/**
  * Created by Francisco on 14/04/2017.
  */
object RecipeExtractor extends Logging{

  private var properties : Configuration = _
  private var isAuthenticated : Boolean = false
  private var connectionProperties : Properties = _
  private var csvSDelimiter : String = _

  private var csvUsers : CSVWriter = _
  private var csvRecipes : CSVWriter = _
  private var csvIngredients : CSVWriter = _
  private var csvReviews : CSVWriter = _
  private var csvSteps : CSVWriter = _
  private var csvNutrition : CSVWriter = _

  def main(args: Array[String]): Unit = {
    if(args.length < 3){
      printHelp
      System.exit(1)
    }
    properties = PropertiesManager.loadProperties(args(0), PropertiesManager.EXTRACTION_PROPERTIES_FILE)
    connectionProperties = new Properties()
    connectionProperties.put("user-agent", properties.getProperty("general.scraping.userAgent"))
    connectionProperties.put("timeout", properties.getProperty("stage2.scraping.delay.timeout"))
    connectionProperties.put("follow-redirects", properties.getProperty("stage2.scraping.followRedirects"))
    connectionProperties.put("delay-detection", properties.getProperty("stage2.scraping.time.detection"))
    connectionProperties.put("referrer", properties.getProperty("general.scraping.referrer"))
    connectionProperties.put("max-body-size", properties.getProperty("stage2.scraping.maxBodySize"))
    connectionProperties.put("delay", properties.getProperty("stage2.scraping.time.delay"))
    connectionProperties.put("attempts", properties.getProperty("stage2.scraping.maxAttempts"))
    connectionProperties.put("host", properties.getProperty("allrecipes.host"))
    connectionProperties.put("delay-nrecipes", properties.getProperty("stage2.scraping.delay.nrecipes"))
    connectionProperties.put("delay-recipes", properties.getProperty("stage2.scraping.delay.recipes"))
    connectionProperties.put("delay-auth", properties.getProperty("stage2.scraping.delay.auth"))
    connectionProperties.put("delay-npages", properties.getProperty("stage2.scraping.delay.pages"))
    connectionProperties.put("delay-pages", properties.getProperty("stage2.scraping.delay.npages"))

    HttpManager.setProperties(properties)
    HttpManager.setConnectionProperties(connectionProperties)

    val files = getInputFiles(args(1))
    val fromScratch = args(2).toBoolean

    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    val outputDir = Utils.resolvePath(2, hostname)
    val csvURLsName : String = properties.getString("stage2.output.csv.url.filename")
    val csvRecipesName : String = properties.getString("stage2.output.csv.recipes.filename")
    val csvIngredientsName : String = properties.getString("stage2.output.csv.ingredients.filename")
    val csvReviewsName : String = properties.getString("stage2.output.csv.reviews.filename")
    val csvStepsName : String = properties.getString("stage2.output.csv.steps.filename")
    val csvNutritionName : String = properties.getString("stage2.output.csv.nutrition.filename")
    csvSDelimiter = properties.getString("stage2.output.csv.delimiter")

    val res = HttpManager.requestAuthToken()
    if(res.isEmpty){
      logger.fatal("Cannot retrieve auth token\nFinishing...")
      beforeExit
      System.exit(1)
    }
    isAuthenticated = true
    connectionProperties.put("auth-token", res.get)
    HttpManager.setConnectionProperties(connectionProperties)
    Scraper.setProperties(properties)

    var status : JSONObject = new JSONObject()
    if(fromScratch){
      status.put("currentFile", files.head)
      status.put("lastLine", 1)
      status.put("processedFiles", new JSONArray())
    }
    else{
      val statusFile : String = args(3)
      status = JSONManager.jsonFromFile(statusFile)
    }

    csvUsers = CSVManager.openCSVWriter(outputDir, csvURLsName, csvSDelimiter.charAt(0), !fromScratch)
    csvRecipes = CSVManager.openCSVWriter(outputDir, csvRecipesName, csvSDelimiter.charAt(0), !fromScratch)
    csvIngredients = CSVManager.openCSVWriter(outputDir, csvIngredientsName, csvSDelimiter.charAt(0), !fromScratch)
    csvReviews = CSVManager.openCSVWriter(outputDir, csvReviewsName, csvSDelimiter.charAt(0), !fromScratch)
    csvSteps = CSVManager.openCSVWriter(outputDir, csvStepsName, csvSDelimiter.charAt(0), !fromScratch)
    csvNutrition = CSVManager.openCSVWriter(outputDir, csvNutritionName, csvSDelimiter.charAt(0), !fromScratch)

    if(fromScratch){
      csvUsers.writeRow(headerToSeq(properties.getString("stage2.output.csv.url.header"), csvSDelimiter.charAt(0)))
      csvRecipes.writeRow(headerToSeq(properties.getString("stage2.output.csv.recipes.header"), csvSDelimiter.charAt(0)))
      csvIngredients.writeRow(headerToSeq(properties.getString("stage2.output.csv.ingredients.header"), csvSDelimiter.charAt(0)))
      csvReviews.writeRow(headerToSeq(properties.getString("stage2.output.csv.reviews.header"), csvSDelimiter.charAt(0)))
      csvSteps.writeRow(headerToSeq(properties.getString("stage2.output.csv.steps.header"), csvSDelimiter.charAt(0)))
      csvNutrition.writeRow(headerToSeq(properties.getString("stage2.output.csv.nutrition.header"), csvSDelimiter.charAt(0)))
    }

    val processedFiles = getProcessedFiles(status)

    files.foreach(file => {
      if(!processedFiles.contains(file)){
        status.put("currentFile", file.getName)
        processFile(file, status, fromScratch)
      }
    })
    beforeExit
  }

  def headerToSeq(header : String, delimiter : Char) : Seq[String] = {
    var result : Seq[String] = Seq()
    header.split(delimiter).toSeq
  }

  def getProcessedFiles(status : JSONObject) : Seq[String] = {
    var result :Seq[String] = Seq()
    var i = 0
    val arr = status.getJSONArray("processedFiles")
    while(i < arr.length()){
      result :+= arr.getJSONObject(i).getString("name")
      i += 1
    }
    result
  }

  def printHelp : Unit = {
    System.err.println("Wrong parameters: try RecipeExtractor <config_path> <files_path> <from_scractch> [<status_file>]")
  }

  def getInputFiles(path : String) : Seq[File] = {
    val dir = new File(path)
    dir.listFiles().toSeq
  }

  def processFile(file : File, status : JSONObject, fromScratch : Boolean): Unit ={
    val delimiter:Char = properties.getString("stage1.output.csv.delimiter").charAt(0)
    val lines = status.getInt("lastLine")
    val csvReader = CSVManager.openCSVReaderLines(file, delimiter, lines) //Header
    val times = connectionProperties.getProperty("delay-nrecipes").toInt
    val delay = connectionProperties.getProperty("delay-recipes").toLong
    var i = lines + 1
    csvReader.foreach(line => {
      processLine(line, status)
      status.put("lastLine", i)
      logger.info(s"Line $i processed")
      logger.info(s"${status.toString}")
      if(i % times == 0){
        logger.info(s"$times lines processed. Sleeping...")
        Thread.sleep(delay)
      }
      i += 1
    })
    logger.info(s"File ${file.getName} processed")
    val arr = status.getJSONArray("processedFiles")
    arr.put(new JSONObject().put("name", file.getName))
    status.put("processedFiles", arr)
    status.put("lastLine", 0)
    logger.info(s"${status.toString}")
    CSVManager.closeCSVReader(csvReader)
  }
  def processLine(line : Seq[String], status : JSONObject): Unit ={
    val category = line(0)
    val url = line(1)

    val ret1 = extractRecipeAPI(url)
    if(ret1.isEmpty){
      logger.fatal("Could not scrape recipe from API\nFinishing...")
      beforeExit
      System.exit(1)
    }
    val recipe = ret1.get
    val id = recipe.id
    val reviewCount = recipe.reviewCount
    if(reviewCount > 0){
      Thread.sleep(1000)
      val ret2 = extractReviewsAPI(id, reviewCount)
      if(ret2.isEmpty){
        logger.fatal("Could not scrape recipe reviews from API\nFinishing...")
        beforeExit
        System.exit(1)
      }
      recipe.reviews_=(ret2.get)
    }

    printRecipe(recipe)
    Thread.sleep(connectionProperties.getProperty("delay").toInt)
  }

  def extractRecipeAPI(url : String): Option[Recipe] ={
    try{
      val html_str = HttpManager.requestRecipeWeb(url)
      val json_str = HttpManager.requestRecipeAPI(Utils.extractRecipeIDFromWebURL(url).toLong)
      if(json_str.isEmpty || html_str.isEmpty){
        logger.error(s"Could not retrieve recipe with url ${url}")
        return None
      }
      //Some(Scraper.scrapeRecipe(json_str.get, html_str.get, csvSDelimiter))
      None
    } catch {
      case sde : ScrapingDetectionException =>
        System.exit(1)
        return None
    }
  }
  def extractReviewsAPI(id : Long, reviewCount : Int) : Option[Seq[Review]] = {
    var page = 1
    val npages_delay = connectionProperties.getProperty("delay-npages").toInt
    val pages_delay = connectionProperties.getProperty("delay-pages").toLong
    val maxpagesize = properties.getInt("allrecipes.api.maxpagesize")
    val maxreviews = properties.getInt("stage2.scraping.reviews.maxperrecipe")
    val reviews = maxreviews.min(reviewCount)
    val pagesize = reviews.min(maxpagesize)
    val maxpages = scala.math.ceil(reviews / pagesize).toInt
    var continue = true
    var result : Seq[Review] = Seq()
    var err = false
    while(continue){
      val json_str = HttpManager.requestRecipeReviewsAPI(id = id, pagesize = pagesize, pageNumber = page)
      if(json_str.isEmpty){
        logger.error(s"Could not retrieve reviews for page ${page}")
        continue = false
        err = true
      }
      if(!err){
        //val res = Scraper.scrapeReviewsList(json_str.get, csvSDelimiter)
        val res = Seq()
        result ++= res
        page += 1
        continue = page <= maxpages
        if(page % npages_delay == 0){
          logger.info(s"Processed ${page} review pages. Sleeping...")
          Thread.sleep(pages_delay)
        }
      }
    }
    if(!err) Some(result) else None
  }

  def printRecipe(recipe : Recipe) : Unit = {

    csvUsers.writeRow(Utils.flatten(Seq(recipe.id, recipe.author.toSeq(), "author")))
    recipe.reviews.foreach(review => {
      csvReviews.writeRow(Utils.flatten(Seq(recipe.id, review.toSeq())))
      csvUsers.writeRow(Utils.flatten(Seq(recipe.id, review.author.toSeq(), "reviewer")))
    })

    csvRecipes.writeRow(recipe.toSeq())

    recipe.ingredients.foreach(ingredient => csvIngredients.writeRow(Utils.flatten(Seq(recipe.id, ingredient.toSeq()))))

    recipe.steps.foreach({case (number, text) => csvSteps.writeRow(Seq(recipe.id, number, text))})

    if(recipe.nutritionInfo.isDefined)
      csvNutrition.writeRow(Utils.flatten(Seq(recipe.id, recipe.nutritionInfo.get.toSeq())))
  }

  def beforeExit(): Unit = {
    CSVManager.closeCSVWriter(csvUsers)
    CSVManager.closeCSVWriter(csvRecipes)
    CSVManager.closeCSVWriter(csvIngredients)
    CSVManager.closeCSVWriter(csvReviews)
    CSVManager.closeCSVWriter(csvSteps)
    CSVManager.closeCSVWriter(csvNutrition)
  }
}
