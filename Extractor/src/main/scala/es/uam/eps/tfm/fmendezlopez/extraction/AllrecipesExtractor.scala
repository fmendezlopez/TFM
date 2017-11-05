package es.uam.eps.tfm.fmendezlopez.extraction

import java.io.{File, IOException}
import java.sql.SQLException
import java.util.Properties

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import es.uam.eps.tfm.fmendezlopez.dao.DatabaseDAO
import es.uam.eps.tfm.fmendezlopez.dto._
import es.uam.eps.tfm.fmendezlopez.exception.{APIAuthorizationException, APILoginException, ScrapingDetectionException}
import es.uam.eps.tfm.fmendezlopez.scraping.Extractor.logger
import es.uam.eps.tfm.fmendezlopez.scraping.{Extractor, Scraper}
import es.uam.eps.tfm.fmendezlopez.utils._
import org.apache.commons.configuration2.Configuration
import org.json.{JSONArray, JSONObject}

import scala.collection.mutable

/**
  * Created by franm on 25/06/2017.
  */
object AllrecipesExtractor extends Logging{

  private var properties : Configuration = _
  private var connectionProperties : Properties = _

  private var csvUsers : CSVWriter = _
  private var csvRecipes : CSVWriter = _
  private var csvIngredients : CSVWriter = _
  private var csvReviews : CSVWriter = _
  private var csvSteps : CSVWriter = _
  private var csvNutrition : CSVWriter = _
  private var csvRelationshipUser : CSVWriter = _
  private var csvRelationshipRecipe : CSVWriter = _
  private var csvUsersUrls : CSVWriter = _
  private var csvAuthorship : CSVWriter = _
  private var csvSimilarity : CSVWriter = _

  def main(args: Array[String]): Unit = {

    if(args.length < 2){
      System.exit(1)
    }
    properties = PropertiesManager.loadProperties(args(0), PropertiesManager.EXTRACTION_PROPERTIES_FILE)

    HttpManager.setProperties(properties)
    Scraper.setProperties(properties)

    args(1).toInt match {
      case 1 =>
        val fromScratch = args(2).toBoolean
        val inputPath = args(3)

        var state : JSONObject = new JSONObject()
        if(fromScratch){
          state.put("currentFile", "")
          state.put("lastLine", 0)
          state.put("processedFiles", new JSONArray())
        }
        else{
          val statusFile : String = args(4)
          state = JSONManager.jsonFromFile(statusFile)
        }
        stage1(inputPath, state, fromScratch)

      case 2 =>
        val fromScratch = args(2).toBoolean
        val inputFileName = args(3)

        var state : JSONObject = new JSONObject()
        if(fromScratch){
          state.put("lastLine", 0)
        }
        else{
          val statusFile : String = args(4)
          state = JSONManager.jsonFromFile(statusFile)
        }
        stage2(inputFileName, state, fromScratch)
    }
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

  def stage1(inputPath : String, state : JSONObject, fromScratch : Boolean) = {

    connectionProperties = new Properties()
    connectionProperties.put("timeout", properties.getProperty("stage4.stage1.scraping.delay.timeout"))
    connectionProperties.put("max-body-size", properties.getProperty("stage4.stage1.scraping.maxBodySize"))
    connectionProperties.put("follow-redirects", properties.getProperty("stage4.stage1.scraping.followRedirects"))
    connectionProperties.put("api-host", properties.getProperty("allrecipes.api.host"))
    connectionProperties.put("base-host", properties.getProperty("allrecipes.url.base"))
    connectionProperties.put("host", properties.getProperty("allrecipes.host"))
    connectionProperties.put("attempts", properties.getProperty("stage4.stage1.scraping.attempts"))
    connectionProperties.put("delay-detection", properties.getProperty("stage4.stage1.scraping.delay.detection"))
    connectionProperties.put("delay-auth", properties.getProperty("stage4.stage1.scraping.delay.auth"))
    connectionProperties.put("referrer", properties.getProperty("general.scraping.referrer"))
    connectionProperties.put("max-pagesize", properties.getProperty("stage4.stage1.scraping.maxpagesize"))
    connectionProperties.put("delay-recipe", properties.getProperty("stage4.stage1.scraping.delay.recipe"))
    connectionProperties.put("delay-nrecipes", properties.getProperty("stage4.stage1.scraping.delay.nrecipes"))
    connectionProperties.put("nrecipes", properties.getProperty("stage4.stage1.scraping.nrecipes"))
    connectionProperties.put("delay-recipe-list", properties.getProperty("stage4.stage1.scraping.delay.recipe_list"))
    connectionProperties.put("max-times-301", properties.getProperty("general.extraction.http.max.times.301"))
    connectionProperties.put("default-category", properties.getProperty("stage4.stage1.scraping.recipe.category.default"))
    connectionProperties.put("similar-threshold", properties.getProperty("stage4.stage1.scraping.recipe.similar-threshold"))
    connectionProperties.put("base-recipe-web", properties.getString("allrecipes.url.recipe"))
    connectionProperties.put("use-cookies", properties.getString("stage4.stage1.scraping.use-cookies"))

    HttpManager.setProperties(properties)
    HttpManager.setConnectionProperties(connectionProperties)
    if(!HttpManager.resetToken){
      logger.fatal("Cannot retrieve auth token\nFinishing...")
      System.exit(1)
    }

    //DB access
    val daoDB = DatabaseDAO.getInstance()
    var total_users = 0
    var total_recipes = 0
    try {
      daoDB.connectAndCreate
      daoDB.configure()
      total_users = daoDB.countUsers()
      total_recipes = daoDB.countRecipes()
    } catch {
      case ie : InstantiationException =>
        logger.fatal(ie.getMessage)
      case iae : IllegalAccessException =>
        logger.fatal(iae.getMessage)
      case cnfe : ClassNotFoundException =>
        logger.fatal(cnfe.getMessage)
      case sql : SQLException =>
        logger.fatal(sql.getMessage)
    }

    logger.info(s"Starting with ${total_users} users and ${total_recipes} recipes")

    val inCSVDelimiter = properties.getString("stage4.stage1.input.csv.delimiter")
    val outCSVDelimiter = properties.getString("stage4.stage1.output.csv.delimiter")

    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    val outputDir = Utils.resolvePath(4, 1, hostname)

    val csvUsersName : String = properties.getString("stage4.stage1.output.csv.users.filename")
    val csvRecipesName : String = properties.getString("stage4.stage1.output.csv.recipes.filename")
    val csvIngredientsName : String = properties.getString("stage4.stage1.output.csv.ingredients.filename")
    val csvReviewsName : String = properties.getString("stage4.stage1.output.csv.reviews.filename")
    val csvStepsName : String = properties.getString("stage4.stage1.output.csv.steps.filename")
    val csvNutritionName : String = properties.getString("stage4.stage1.output.csv.nutrition.filename")
    val csvRelationshipUserName : String = properties.getString("stage4.stage1.output.csv.relationship_user.filename")
    val csvRelationshipRecipeName : String = properties.getString("stage4.stage1.output.csv.relationship_recipe.filename")
    val csvUserURLsName : String = properties.getString("stage4.stage1.output.csv.users_urls.filename")
    val csvAuthorshipName : String = properties.getString("stage4.stage1.output.csv.authorship.filename")
    val csvSimilarityName : String = properties.getString("stage4.stage1.output.csv.similarity.filename")

    csvUsers = CSVManager.openCSVWriter(outputDir, csvUsersName, inCSVDelimiter.charAt(0), !fromScratch)
    csvRecipes = CSVManager.openCSVWriter(outputDir, csvRecipesName, inCSVDelimiter.charAt(0), !fromScratch)
    csvIngredients = CSVManager.openCSVWriter(outputDir, csvIngredientsName, inCSVDelimiter.charAt(0), !fromScratch)
    csvReviews = CSVManager.openCSVWriter(outputDir, csvReviewsName, inCSVDelimiter.charAt(0), !fromScratch)
    csvSteps = CSVManager.openCSVWriter(outputDir, csvStepsName, inCSVDelimiter.charAt(0), !fromScratch)
    csvNutrition = CSVManager.openCSVWriter(outputDir, csvNutritionName, inCSVDelimiter.charAt(0), !fromScratch)
    csvRelationshipUser = CSVManager.openCSVWriter(outputDir, csvRelationshipUserName, inCSVDelimiter.charAt(0), !fromScratch)
    csvRelationshipRecipe = CSVManager.openCSVWriter(outputDir, csvRelationshipRecipeName, inCSVDelimiter.charAt(0), !fromScratch)
    csvUsersUrls = CSVManager.openCSVWriter(outputDir, csvUserURLsName, inCSVDelimiter.charAt(0), !fromScratch)
    csvAuthorship = CSVManager.openCSVWriter(outputDir, csvAuthorshipName, inCSVDelimiter.charAt(0), !fromScratch)
    csvSimilarity = CSVManager.openCSVWriter(outputDir, csvSimilarityName, inCSVDelimiter.charAt(0), !fromScratch)

    if(fromScratch) {
      csvUsers.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.users.header"), outCSVDelimiter.charAt(0)))
      csvRecipes.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.recipes.header"), outCSVDelimiter.charAt(0)))
      csvIngredients.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.ingredients.header"), outCSVDelimiter.charAt(0)))
      csvReviews.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.reviews.header"), outCSVDelimiter.charAt(0)))
      csvSteps.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.steps.header"), outCSVDelimiter.charAt(0)))
      csvNutrition.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.nutrition.header"), outCSVDelimiter.charAt(0)))
      csvRelationshipUser.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.relationship_user.header"), outCSVDelimiter.charAt(0)))
      csvRelationshipRecipe.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.relationship_recipe.header"), outCSVDelimiter.charAt(0)))
      csvUsersUrls.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.users_urls.header"), outCSVDelimiter.charAt(0)))
      csvAuthorship.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.authorship.header"), outCSVDelimiter.charAt(0)))
      csvSimilarity.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.similarity.header"), outCSVDelimiter.charAt(0)))
    }
    var csvReader : CSVReader = null

    def beforeExit = {
      CSVManager.closeCSVReader(csvReader)
      CSVManager.closeCSVWriter(csvUsers)
      CSVManager.closeCSVWriter(csvRecipes)
      CSVManager.closeCSVWriter(csvIngredients)
      CSVManager.closeCSVWriter(csvReviews)
      CSVManager.closeCSVWriter(csvSteps)
      CSVManager.closeCSVWriter(csvNutrition)
      CSVManager.closeCSVWriter(csvRelationshipUser)
      CSVManager.closeCSVWriter(csvRelationshipRecipe)
      CSVManager.closeCSVWriter(csvUsersUrls)
      CSVManager.closeCSVWriter(csvAuthorship)
      CSVManager.closeCSVWriter(csvSimilarity)
    }

    val files = Utils.getInputFiles(inputPath)
    val processedFiles = getProcessedFiles(state)

    val delay = properties.getLong("stage4.stage1.scraping.delay.afterLine")
    val nlines = properties.getLong("stage4.stage1.scraping.nlines")
    val delay_nlines = properties.getLong("stage4.stage1.scraping.delay.nlines")

    val max_userreviews = properties.getInt("stage4.stage1.scraping.max.user.reviews")
    val max_userfollowing = properties.getInt("stage4.stage1.scraping.max.user.following")
    val max_userfollower = properties.getInt("stage4.stage1.scraping.max.user.follower")
    val maxrecipes = properties.getInt("stage4.stage1.scraping.max_recipes_user")
    val minrecipes = properties.getInt("stage4.stage1.scraping.minrecipes")
    val minreviews = properties.getInt("stage4.stage1.scraping.minreviews")

    files.foreach(file => {
      if(!processedFiles.contains(file)) {
        var lines = state.getInt("lastLine")
        state.put("currentFile", file.getName)
        csvReader = CSVManager.openCSVReader(file, inCSVDelimiter.charAt(0))
        CSVManager.skipLines(csvReader, lines + 1, (ret) => if(ret.isEmpty)false else true)

        var nreviews = 0
        var lines_processed = 0
        csvReader.foreach(line => {
          val id = line(1).toLong
          logger.info(s"Processing user ${id}")

          var exists = false

          //DB check

          try {
            exists = daoDB.existsUser(id.toString)
          } catch {
            case e@(_: InstantiationException | _: IllegalAccessException | _: ClassNotFoundException | _: SQLException) =>
              logger.fatal(e.getMessage)
              try
                daoDB.disconnect
              catch {
                case e1@(_: SQLException | _: IOException) =>
                  logger.fatal(e1.getMessage)
              }
          }

          if(exists){
            logger.info(s"User ${id} has already been processed")
          }

          else {
            logger.info("Getting user profile...")
            HttpManager.setProperty("referrer", properties.getProperty("allrecipes.url.base"))
            val potUser = Extractor.extractUser(id, inCSVDelimiter)
            if (potUser.isEmpty) {
              logger.fatal("Stopping...")
              beforeExit
              System.exit(1)
            }
            val user = potUser.get
            if(!isValidUser(user, minrecipes, minreviews)){
              logger.warn(s"User has less than ${minrecipes} recipes. Discarded.")
            }
            else{

              Thread.sleep(500)

              logger.info("Getting user following...")
              val potFollowing = Extractor.extractFollowing(id, inCSVDelimiter, max_userfollowing)
              if (potFollowing.isEmpty) {
                logger.fatal("Stopping...")
                beforeExit
                System.exit(1)
              }
              val following = potFollowing.get
              logger.info(s"Got ${following.length} following")

              logger.info("Getting user followers...")
              val potFollowers = Extractor.extractFollowers(id, inCSVDelimiter, max_userfollower)
              if (potFollowers.isEmpty) {
                logger.fatal("Stopping...")
                beforeExit
                System.exit(1)
              }
              val followers = potFollowers.get
              logger.info(s"Got ${followers.length} followers")
              Thread.sleep(500)

              HttpManager.setProperty("referrer", user.profileUrl)

              logger.info("Getting user recipes...")
              val potUserRecipes = Extractor.extractUserRecipes(user, outCSVDelimiter, maxrecipes)
              if (potUserRecipes.isEmpty) {
                logger.fatal("Impossible to extract user recipes.\nClosing...")
                System.exit(1)
              }

              val user_recipes = potUserRecipes.get
              var new_recipes: Map[String, Seq[Recipe]] = user_recipes._1
              val rep_recipes: Map[String, Seq[Long]] = user_recipes._2

              var newIDs: Set[Long] = new_recipes.values.reduce((a, b) => a ++ b).flatMap(r => Seq(r.id)).toSet
              val new_recipes_number = newIDs.size
              var rep_recipes_number = 0
              rep_recipes.values.foreach(rep_recipes_number += _.length)
              logger.info(s"Got ${newIDs.size} new recipes and ${rep_recipes_number} repeated recipes")

              val len = new_recipes_number + rep_recipes_number

              if (len > 0) {
                logger.info("Getting user reviews...")
                val potReviews = Extractor.extractUserReviews(user.id, outCSVDelimiter, max_userreviews)
                if (potReviews.isEmpty) {
                  logger.fatal("Stopping...")
                  beforeExit
                  System.exit(1)
                }
                val reviews = potReviews.get
                logger.info(s"Got ${reviews.length} reviews")

                logger.info("Getting recipes from reviews...")
                Thread.sleep(500)
                val recipe_reviews = Extractor.extractRecipeFromReviews(user, reviews, newIDs, outCSVDelimiter)
                val new_recipe_reviews = recipe_reviews._1
                val repeated_recipe_reviews = recipe_reviews._2

                logger.info(s"Got ${new_recipe_reviews.length} new recipes and ${repeated_recipe_reviews.length} repeated recipes")

                newIDs ++= new_recipe_reviews.flatMap(r => Seq(r.id)).toSet

                new_recipes += "review" -> new_recipe_reviews

                val allrecipes: Map[String, Seq[Long]] = Map(
                  "recipes" -> (new_recipes("recipes").map(_.id) ++ rep_recipes("recipes")),
                  "fav" -> (new_recipes("fav").map(_.id) ++ rep_recipes("fav")),
                  "madeit" -> (new_recipes("madeit").map(_.id) ++ rep_recipes("madeit")),
                  "review" -> (new_recipe_reviews.map(_.id) ++ repeated_recipe_reviews)
                )

                val following = potFollowing.get.filter(_.id != user.id)
                val followers = potFollowers.get.filter(_.id != user.id)

                var authors: Set[Author] = new_recipes.values.reduce((a, b) => a ++ b).flatMap(r => Seq(r.author)).toSet

                printUser(user, csvUsers)
                printReviewList(potReviews.get, (csvReviews))
                printUserFollowing(following, user, (csvUsers, csvRelationshipUser, csvUsersUrls))
                printUserFollowers(followers, user, (csvUsers, csvRelationshipUser, csvUsersUrls))
                printAuthorList(authors, csvUsersUrls)
                printRecipeList(new_recipes, (csvRecipes, csvIngredients, csvSteps, csvNutrition, csvAuthorship, csvSimilarity))
                printUserRecipesList(user.id, allrecipes, csvRelationshipRecipe)

                nreviews += potReviews.get.length
                total_users += 1
                total_recipes += new_recipes_number + new_recipe_reviews.length

                logger.info(s"Extracted ${nreviews} reviews")
                logger.info(s"Extracted ${total_recipes} recipes in total")
                logger.info(s"Extracted ${total_users} users in total")

                //DB insertion
                try {
                  daoDB.insertUser(id.toString)
                } catch {
                  case sql : SQLException =>
                    logger.fatal(sql.getMessage)
                }

                newIDs.foreach(id =>
                  //DB insertion
                  try {
                    daoDB.insertRecipe(id.toString)
                  } catch {
                    case sql : SQLException =>
                      logger.fatal(sql.getMessage)
                  }
                )

              }
              else {
                logger.warn("Recipe list is empty")
              }
            }
          }
          lines += 1
          state.put("lastLine", lines)
          logger.info(s"Line $lines processed")
          logger.info(s"${state.toString}")
          if(lines % nlines == 0){
            logger.info(s"$nlines lines processed. Sleeping...")
            Thread.sleep(delay_nlines)
          }
          else{
            logger.info("Sleeping...")
            Thread.sleep(delay)
          }
          logger.info(s"Processed user ${id}")
        })
        CSVManager.closeCSVReader(csvReader)
      }
    })
  }

  def stage2(inputFileName: String, state: JSONObject, fromScratch: Boolean) = {
    connectionProperties = new Properties()
    connectionProperties.put("timeout", properties.getProperty("stage4.stage1.scraping.delay.timeout"))
    connectionProperties.put("max-body-size", properties.getProperty("stage4.stage1.scraping.maxBodySize"))
    connectionProperties.put("follow-redirects", properties.getProperty("stage4.stage1.scraping.followRedirects"))
    connectionProperties.put("api-host", properties.getProperty("allrecipes.api.host"))
    connectionProperties.put("base-host", properties.getProperty("allrecipes.url.base"))
    connectionProperties.put("host", properties.getProperty("allrecipes.host"))
    connectionProperties.put("attempts", properties.getProperty("stage4.stage1.scraping.attempts"))
    connectionProperties.put("delay-detection", properties.getProperty("stage4.stage1.scraping.delay.detection"))
    connectionProperties.put("delay-auth", properties.getProperty("stage4.stage1.scraping.delay.auth"))
    connectionProperties.put("referrer", properties.getProperty("general.scraping.referrer"))
    connectionProperties.put("max-pagesize", properties.getProperty("stage4.stage1.scraping.maxpagesize"))
    connectionProperties.put("delay-recipe", properties.getProperty("stage4.stage1.scraping.delay.recipe"))
    connectionProperties.put("delay-nrecipes", properties.getProperty("stage4.stage1.scraping.delay.nrecipes"))
    connectionProperties.put("max-times-301", properties.getProperty("general.extraction.http.max.times.301"))
    connectionProperties.put("default-category", properties.getProperty("stage4.stage1.scraping.recipe.category.default"))
    connectionProperties.put("use-cookies", properties.getString("stage4.stage2.scraping.use-cookies"))

    HttpManager.setProperties(properties)
    HttpManager.setConnectionProperties(connectionProperties)

    val inCSVDelimiter = properties.getString("stage4.stage1.input.csv.delimiter")
    val outCSVDelimiter = properties.getString("stage4.stage1.output.csv.delimiter")

    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    val outputDir = Utils.resolvePath(4, 2, hostname)

    val outputFileName: String = properties.getString("stage4.stage2.output.csv.recipes.filename")

    val csvWriter = CSVManager.openCSVWriter(outputDir, outputFileName, inCSVDelimiter.charAt(0), !fromScratch)

    if(fromScratch) {
      csvWriter.writeRow(Utils.headerToSeq(properties.getString("stage4.stage1.output.csv.recipes.header"), outCSVDelimiter.charAt(0)))
    }
    val csvReader: CSVReader = CSVManager.openCSVReaderLines(new File(inputFileName), inCSVDelimiter.charAt(0), 1)
    var lines = state.getInt("lastLine")
    CSVManager.skipLines(csvReader, lines + 1, (ret) => if(ret.isEmpty)false else true)

    def beforeExit = {
      CSVManager.closeCSVReader(csvReader)
      CSVManager.closeCSVWriter(csvWriter)
    }

    val delay = properties.getLong("stage4.stage2.scraping.delay.afterLine")
    val nlines = properties.getLong("stage4.stage2.scraping.nlines")
    val delay_nlines = properties.getLong("stage4.stage2.scraping.delay.nlines")
    val baseCategory = connectionProperties.getProperty("default-category").toLong

    csvReader.foreach(line => {
      val webURL = line(13)
      val id = line(1)
      logger.info(s"Processing recipe ${id}")
      try{
        logger.debug(s"Extracting recipe with webURL ${webURL}")
        val html_str = if(webURL.isEmpty) None else HttpManager.requestRecipeWebURL(webURL)
        val category =
        if(html_str.isEmpty){
          logger.error(s"Could not retrieve recipe with url ${webURL} from WEB")
          val cat = new RecipeCategory
          cat.id_=(baseCategory)
          cat
        }
        else{
          Scraper.scrapeCategory(html_str.get, baseCategory)
        }
        val newLine: mutable.Seq[String] = mutable.Seq.empty[String] ++ line
        newLine.update(2, category.id.toString)
        csvWriter.writeRow(newLine)

        lines += 1
        state.put("lastLine", lines)
        logger.info(s"Line $lines processed")
        logger.info(s"${state.toString}")
        if(lines % nlines == 0){
          logger.info(s"$nlines lines processed. Sleeping...")
          Thread.sleep(delay_nlines)
        }
        else{
          logger.info("Sleeping...")
          Thread.sleep(delay)
        }
        logger.info(s"Processed recipe ${id}")
      } catch {
        case sde : ScrapingDetectionException =>
          logger.error(sde)
          beforeExit
          System.exit(1)

        case aae : APIAuthorizationException =>
          logger.error(aae)
          beforeExit
          System.exit(1)
      }
    })
  }

  def printAuthorList(authors: Set[Author], writer: CSVWriter): Unit = {
    authors foreach { author =>
      writer.writeRow(author.toSeq())
    }
  }

  def printUser(user: User, writer: CSVWriter) : Unit = {
    writer.writeRow(user.toSeq)
  }

  def printUserFollowing(users: Seq[User], related_user: User, writers: (CSVWriter, CSVWriter, CSVWriter)) : Unit = {
    users foreach { user =>
      printUser(user, writers._1)
      writers._2.writeRow(Seq(related_user.id, user.id))
      writers._3.writeRow(Seq(user.profileUrl, user.id))
    }
  }

  def printUserRecipesList(id: Long, recipes: Map[String, Seq[Long]], writer : CSVWriter) : Unit = {
    recipes.foreach({case(list, recipeList) => {
      recipeList foreach {recipe =>
        writer.writeRow(Seq(list, recipe, id))
      }
    }})
  }

  def printUserFollowers(users: Seq[User], related_user: User, writers: (CSVWriter, CSVWriter, CSVWriter)) : Unit = {
    users foreach { user =>
      printUser(user, writers._1)
      writers._2.writeRow(Seq(user.id, related_user.id))
      writers._3.writeRow(Seq(user.profileUrl, user.id))
    }
  }

  def printReviewList(reviews: Seq[Review], writer: CSVWriter) : Unit = {
    reviews.foreach(review => {
      writer.writeRow(review.toSeq())
    })
  }

  def printRecipeList(recipes: Map[String, Seq[Recipe]], writers: (CSVWriter, CSVWriter, CSVWriter, CSVWriter, CSVWriter, CSVWriter)) : Unit = {

    recipes.foreach({case(list, recipeList) => {
      printRecipeList(recipeList, list, writers)
    }})
  }

  def printRecipeList(recipes: Seq[Recipe], list: String, writers: (CSVWriter, CSVWriter, CSVWriter, CSVWriter, CSVWriter, CSVWriter)) = {
    recipes foreach {recipe =>
      writers._1.writeRow(Utils.flatten(Seq(list, recipe.toSeq())))
      recipe.ingredients.foreach(ingredient => writers._2.writeRow(Utils.flatten(Seq(recipe.id, ingredient.toSeq()))))
      recipe.steps.foreach({ case (number, text) => writers._3.writeRow(Seq(recipe.id, number, text)) })
      if (recipe.nutritionInfo.isDefined)
        writers._4.writeRow(Utils.flatten(Seq(recipe.id, recipe.nutritionInfo.get.toSeq())))
      writers._5.writeRow(Seq(recipe.id, recipe.author.id))
      recipe.similarRecipes foreach {similar => {
        writers._6.writeRow(Seq(recipe.id, similar))
      }}
    }
  }

  def isValidUser(user: User, threshold_recipes: Int, threshold_reviews: Int) : Boolean = {
    (user.recipeCount + user.favCount + user.madeitCount) >= threshold_recipes && user.reviewCount >= threshold_reviews
  }
}
