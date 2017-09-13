package es.uam.eps.tfm.fmendezlopez.extraction

import java.io.File
import java.util.Properties

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import es.uam.eps.tfm.fmendezlopez.exception.ScrapingDetectionException
import es.uam.eps.tfm.fmendezlopez.scraping.Scraper
import es.uam.eps.tfm.fmendezlopez.utils._
import es.uam.eps.tfm.fmendezlopez.utils.{CSVManager, HttpManager, PropertiesManager, Utils}
import org.apache.commons.configuration2.Configuration
import org.json.{JSONArray, JSONObject}

/**
  * Created by Francisco on 24/04/2017.
  */
object UserExtractor extends Logging{

  private var properties : Configuration = _
  private var connectionPropertiesRecipe : Properties = _
  private var connectionPropertiesUser : Properties = _

  private var csvAuthorsIn : CSVReader = _

  private var csvAuthorsOut : CSVReader = _
  private var csvUsers : CSVWriter = _
  private var csvRecipes : CSVWriter = _
  private var csvIngredients : CSVWriter = _
  private var csvReviews : CSVWriter = _
  private var csvSteps : CSVWriter = _
  private var csvNutrition : CSVWriter = _

  private var csvUsersIn : CSVWriter = _
  private var csvUsersOut : CSVWriter = _
  private var csvRelOut : CSVWriter = _

  def main(args: Array[String]): Unit = {
    if(args.length < 3){
      printHelp
      System.exit(1)
    }
    properties = PropertiesManager.loadProperties(args(0), PropertiesManager.EXTRACTION_PROPERTIES_FILE)

    connectionPropertiesRecipe = new Properties()
    connectionPropertiesRecipe.put("timeout", properties.getProperty("stage2.scraping.delay.timeout"))
    connectionPropertiesRecipe.put("follow-redirects", properties.getProperty("stage2.scraping.followRedirects"))
    connectionPropertiesRecipe.put("delay-detection", properties.getProperty("stage2.scraping.time.detection"))
    connectionPropertiesRecipe.put("referrer", properties.getProperty("general.scraping.referrer"))
    connectionPropertiesRecipe.put("max-body-size", properties.getProperty("stage2.scraping.maxBodySize"))
    connectionPropertiesRecipe.put("delay", properties.getProperty("stage2.scraping.time.delay"))
    connectionPropertiesRecipe.put("attempts", properties.getProperty("stage2.scraping.maxAttempts"))
    connectionPropertiesRecipe.put("host", properties.getProperty("allrecipes.host"))
    connectionPropertiesRecipe.put("api-host", properties.getProperty("allrecipes.api.host"))
    connectionPropertiesRecipe.put("base-host", properties.getProperty("allrecipes.url.base"))
    connectionPropertiesRecipe.put("delay-nrecipes", properties.getProperty("stage2.scraping.delay.nrecipes"))
    connectionPropertiesRecipe.put("delay-recipes", properties.getProperty("stage2.scraping.delay.recipes"))
    connectionPropertiesRecipe.put("delay-auth", properties.getProperty("stage2.scraping.delay.auth"))
    connectionPropertiesRecipe.put("delay-npages", properties.getProperty("stage2.scraping.delay.pages"))
    connectionPropertiesRecipe.put("delay-pages", properties.getProperty("stage2.scraping.delay.npages"))

    connectionPropertiesUser = new Properties()
    connectionPropertiesUser.put("timeout", properties.getProperty("stage2.scraping.delay.timeout"))
    connectionPropertiesUser.put("follow-redirects", properties.getProperty("stage2.scraping.followRedirects"))
    connectionPropertiesUser.put("delay-detection", properties.getProperty("stage2.scraping.time.detection"))
    connectionPropertiesUser.put("referrer", properties.getProperty("general.scraping.referrer"))
    connectionPropertiesUser.put("max-body-size", properties.getProperty("stage2.scraping.maxBodySize"))
    connectionPropertiesUser.put("delay", properties.getProperty("stage2.scraping.time.delay"))
    connectionPropertiesUser.put("attempts", properties.getProperty("stage2.scraping.maxAttempts"))
    connectionPropertiesUser.put("host", properties.getProperty("allrecipes.host"))
    connectionPropertiesUser.put("api-host", properties.getProperty("allrecipes.api.host"))
    connectionPropertiesUser.put("base-host", properties.getProperty("allrecipes.url.base"))
    connectionPropertiesUser.put("delay-nrecipes", properties.getProperty("stage2.scraping.delay.nrecipes"))
    connectionPropertiesUser.put("delay-recipes", properties.getProperty("stage2.scraping.delay.recipes"))
    connectionPropertiesUser.put("delay-auth", properties.getProperty("stage2.scraping.delay.auth"))
    connectionPropertiesUser.put("delay-npages", properties.getProperty("stage2.scraping.delay.pages"))
    connectionPropertiesUser.put("delay-pages", properties.getProperty("stage2.scraping.delay.npages"))

    HttpManager.setProperties(properties)

    val res = HttpManager.requestAuthToken()
    if(res.isEmpty){
      logger.fatal("Cannot retrieve auth token\nFinishing...")
      beforeExit
      System.exit(1)
    }
    connectionPropertiesUser.put("auth-token", res.get)
    connectionPropertiesRecipe.put("auth-token", res.get)
    Scraper.setProperties(properties)

    val files = getInputFiles(args(1))
    val fromScratch = args(2).toBoolean

    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    //val outputDir = Utils.resolvePath(2, hostname
  }

  def printHelp : Unit = {
    System.err.println("Wrong parameters: try RecipeExtractor <config_path> <files_path> <from_scractch> [<status_file>]")
  }

  def getInputFiles(path : String) : Seq[File] = {
    val dir = new File(path)
    dir.listFiles().toSeq
  }

  def stage1(inputPath : String, fromScratch : Option[JSONObject]) = {
    val files = getInputFiles(inputPath)
    val csvDelimiter = properties.getString("stage3.input.csv.delimiter")

    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    val outputDir = Utils.resolvePath(3, 1, hostname)

    val csvURLsName : String = properties.getString("stage2.output.csv.url.filename")
    val csvRecipesName : String = properties.getString("stage2.output.csv.recipes.filename")
    val csvIngredientsName : String = properties.getString("stage2.output.csv.ingredients.filename")
    val csvReviewsName : String = properties.getString("stage2.output.csv.reviews.filename")
    val csvStepsName : String = properties.getString("stage2.output.csv.steps.filename")
    val csvNutritionName : String = properties.getString("stage2.output.csv.nutrition.filename")


    csvUsers = CSVManager.openCSVWriter(outputDir, csvURLsName, csvDelimiter.charAt(0), fromScratch.isEmpty)
    csvRecipes = CSVManager.openCSVWriter(outputDir, csvRecipesName, csvDelimiter.charAt(0), fromScratch.isEmpty)
    csvIngredients = CSVManager.openCSVWriter(outputDir, csvIngredientsName, csvDelimiter.charAt(0), fromScratch.isEmpty)
    csvReviews = CSVManager.openCSVWriter(outputDir, csvReviewsName, csvDelimiter.charAt(0), fromScratch.isEmpty)
    csvSteps = CSVManager.openCSVWriter(outputDir, csvStepsName, csvDelimiter.charAt(0), fromScratch.isEmpty)
    csvNutrition = CSVManager.openCSVWriter(outputDir, csvNutritionName, csvDelimiter.charAt(0), fromScratch.isEmpty)

    var status : JSONObject = new JSONObject()
    if(fromScratch.isEmpty){
      status.put("currentFile", files.head)
      status.put("lastLine", 1)
      status.put("processedFiles", new JSONArray())
    }
    else{
      status = fromScratch.get
    }

    val processedFiles = getProcessedFiles(status)

    files.foreach(file => {
      if (!processedFiles.contains(file)) {
        status.put("currentFile", file.getName)
        processFileStage1(file, status)
      }
    })
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

  def processFileStage1(file : File, status : JSONObject) = {
    val delimiter = properties.getString("stage3.output.csv.delimiter").charAt(0)
    val lines = status.getInt("lastLine")
    val csvReader = CSVManager.openCSVReaderLines(file, delimiter, lines) //Header
    val times = connectionPropertiesUser.getProperty("delay-nrecipes").toInt
    val delay = connectionPropertiesUser.getProperty("delay-recipes").toLong
    var i = lines + 1
    csvReader.foreach(line => {
      processLineStage1(line, status, s"$delimiter")
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

  def processLineStage1(line: Seq[String], status: JSONObject, csvDelimiter : String) = {
    val threshold = properties.getInt("stage3.1.threshold.recipes")
    val ret = Extractor.extractUser(line(1).toLong, connectionPropertiesUser, csvDelimiter)
    if(ret.isEmpty){
      System.exit(1)
    }
    val user = ret.get

    var n = line(4).toInt
    while(n < threshold){

      n += 1
    }
  }

  def processFile(file : File, status : JSONObject, fromScratch : Boolean): Unit ={
    val delimiter:Char = properties.getString("stage3.output.csv.delimiter").charAt(0)
    val lines = status.getInt("lastLine")
    val csvReader = CSVManager.openCSVReaderLines(file, delimiter, lines + 1) //Header
    val times = connectionPropertiesUser.getProperty("delay-nrecipes").toInt
    val delay = connectionPropertiesUser.getProperty("delay-recipes").toLong
    var i = 0
    csvReader.foreach(line => {
      //processLine(line, status)
      i += 1
      status.put("lastLine", i)
      logger.info(s"Line $i processed")
      logger.info(s"${status.toString}")
      if(i % times == 0){
        logger.info(s"$times lines processed. Sleeping...")
        Thread.sleep(delay)
      }
    })
    logger.info(s"File ${file.getName} processed")
    val arr = status.getJSONArray("processedFiles")
    arr.put(new JSONObject().put("name", file.getName))
    status.put("processedFiles", arr)
    status.put("lastLine", 0)
    logger.info(s"${status.toString}")
    CSVManager.closeCSVReader(csvReader)
  }
  def extractUser(id : Long) : Unit = {
    try{
      val ret = HttpManager.requestUserAPI(id)
      if(ret.isEmpty){
        logger.error(s"Could not retrieve user with id ${id}")
        None
      }
      //Some(Scraper.scrapeRecipe(json_str.get, html_str.get, csvSDelimiter))
    } catch {
      case sde : ScrapingDetectionException =>
        System.exit(1)
        None
    }

  }
  /*
  def processLine(line : Seq[String], status : JSONObject): Unit = {
    val ret1 = extractUserAPI(url)
    if(ret1.isEmpty){
      logger.fatal("Could not scrape recipe from API\nFinishing...")
      beforeExit
      System.exit(1)
    }
    val recipe = ret1.get
  }

  def extractUserAPI(url : String) = Option[User] = {

  }

  def beforeExit(): Unit = {

  }

*/
  def beforeExit(): Unit = {
    CSVManager.closeCSVWriter(csvUsers)
    CSVManager.closeCSVWriter(csvRecipes)
    CSVManager.closeCSVWriter(csvIngredients)
    CSVManager.closeCSVWriter(csvReviews)
    CSVManager.closeCSVWriter(csvSteps)
    CSVManager.closeCSVWriter(csvNutrition)
  }
}
