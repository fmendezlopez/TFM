package es.uam.eps.tfm.fmendezlopez.extraction

import java.sql.SQLException
import java.util.Properties

import es.uam.eps.tfm.fmendezlopez.dao.DatabaseDAO
import es.uam.eps.tfm.fmendezlopez.dto.{Recipe, Review, User, UserRecipe}
import es.uam.eps.tfm.fmendezlopez.exception.{APIAuthorizationException, APILoginException, ScrapingDetectionException}
import es.uam.eps.tfm.fmendezlopez.scraping.Scraper
import es.uam.eps.tfm.fmendezlopez.utils.{HttpManager, Logging, Utils}
import org.apache.commons.configuration2.Configuration
import org.json.{JSONArray, JSONObject}

import scala.collection.mutable

/**
  * Created by franm on 20/06/2017.
  */
object Extractor extends Logging{

  /*
  def extractRecipe(id : String, properties : Configuration, connectionProperties : Properties, csvDelimiter : String, complete : Boolean = true): Option[Recipe] ={
    try{
      val url = Utils.compoundRecipeURL(id, properties)
      val html_str = HttpManager.requestRecipeWeb(url, connectionProperties)
      if(html_str.isEmpty){
        logger.error(s"Could not retrieve recipe with url ${url} from WEB")
        return None
      }
      val json_str = HttpManager.requestRecipeAPI(id.toLong, connectionProperties)
      if(json_str.isEmpty){
        logger.error(s"Could not retrieve recipe with url ${url} from API")
        return None
      }
      Some(Scraper.scrapeRecipe(json_str.get, html_str.get, csvDelimiter))
    } catch {
      case sde : ScrapingDetectionException =>
        logger.error(sde)
        return None
      case aae : APIAuthorizationException =>
        logger.fatal(aae)
        val newProps = HttpManager.resetToken(connectionProperties)
        if(newProps.isDefined) {
          val rt = extractRecipe(id, properties, newProps.get, csvDelimiter, complete)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }
  */

  def extractRecipe(author_id : Long, webURL : String, apiURL : String, connectionProperties : Properties, csvDelimiter : String): Option[Recipe] ={

    try{
      logger.debug(s"Extracting recipe with webURL ${webURL} and apiURL ${apiURL}")
      val html_str = if(webURL.isEmpty) Some("") else HttpManager.requestRecipeWebURL(webURL, connectionProperties)
      if(html_str.isEmpty){
        logger.error(s"Could not retrieve recipe with url ${webURL} from WEB")
        return None
      }
      val json_str = HttpManager.requestRecipeAPIURL(apiURL, connectionProperties)
      if(json_str.isEmpty){
        logger.error(s"Could not retrieve recipe with url ${apiURL} from API")
        return None
      }
      Some(Scraper.scrapeRecipe(author_id, json_str.get, html_str.get, csvDelimiter))
    } catch {
      case sde : ScrapingDetectionException =>
        logger.error(sde)
        return None

      case ale : APILoginException =>
        logger.error(ale)
        return None

      case aae : APIAuthorizationException =>
        logger.error(aae)
        val newProps = HttpManager.resetToken(connectionProperties)
        if(newProps.isDefined) {
          val rt = extractRecipe(author_id, webURL, apiURL, newProps.get, csvDelimiter)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def extractUser(id : Long, connectionProperties : Properties, csvDelimiter : String, complete : Boolean = true) : Option[User] = {
    try{
      var ret : Option[String] = None
      if(complete){
        HttpManager.requestUserWeb(id, connectionProperties)
      }
      ret = HttpManager.requestUserAPI(id, connectionProperties)
      if(ret.isEmpty){
        logger.error(s"Could not extract user with id ${id} from API")
        return None
      }
      Some(Scraper.scrapeUser(Left(ret.get), csvDelimiter))
    } catch {
      case sde : ScrapingDetectionException =>
        logger.fatal(sde)
        return None
      case aae : APIAuthorizationException =>
        logger.fatal(aae)
        val newProps = HttpManager.resetToken(connectionProperties)
        if(newProps.isDefined) {
          val rt = extractUser(id, newProps.get, csvDelimiter, complete)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def extractFollowing(id : Long, connectionProperties : Properties, csvDelimiter : String, nusers : Int) : Option[Seq[User]] = {
    val pagesize = connectionProperties.getProperty("max-pagesize").toInt
    val npages = nusers / pagesize
    var result : Seq[User] = Seq()
    try{
      (1 to npages).foreach(i => {
        val ret = HttpManager.requestFollowingAPI(id, connectionProperties, pagesize, i)
        if(ret.isEmpty){
          logger.error(s"Could not retrieve following for id ${id}")
          return None
        }
        val list : Seq[User] = Scraper.scrapeUserList(ret.get, csvDelimiter)
        result ++= list
      })
      Some(result)
    } catch {
      case sde : ScrapingDetectionException =>
        logger.fatal(sde)
        return None
      case aae : APIAuthorizationException =>
        logger.fatal(aae)
        val newProps = HttpManager.resetToken(connectionProperties)
        if(newProps.isDefined) {
          val rt = extractFollowing(id, newProps.get, csvDelimiter, nusers)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def extractFollowers(id : Long, connectionProperties : Properties, csvDelimiter : String, nusers : Int) : Option[Seq[User]] = {
    val pagesize = connectionProperties.getProperty("max-pagesize").toInt
    val npages = nusers / pagesize
    var result : Seq[User] = Seq()
    try{
      (1 to npages).foreach(i => {
        val ret = HttpManager.requestFollowersAPI(id, connectionProperties, pagesize, i)
        if(ret.isEmpty){
          logger.error(s"Could not retrieve following for id ${id}")
          return None
        }
        val list : Seq[User] = Scraper.scrapeUserList(ret.get, csvDelimiter)
        result ++= list
      })
      Some(result)
    } catch {
      case sde : ScrapingDetectionException =>
        logger.fatal(sde)
        return None
      case aae : APIAuthorizationException =>
        logger.fatal(aae)
        val newProps = HttpManager.resetToken(connectionProperties)
        if(newProps.isDefined) {
          val rt = extractFollowers(id, newProps.get, csvDelimiter, nusers)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def extractUserRecipes(author : User, connectionProperties : Properties, csvDelimiter : String, maxrecipes : Int) : Option[(Map[String, Seq[Recipe]], Map[String, Seq[Long]])] = {

    val daoDB = DatabaseDAO.getInstance()

    def extRecipes(json : String) : Seq[(Long, String, String)] = {
      val mainJSON = new JSONObject(json)
      var result : Seq[(Long, String, String)] = Seq()

      val arr = mainJSON.getJSONArray("recipes")
      var i = 0
      while(i < arr.length()){
        val json = arr.getJSONObject(i)
        val prior_id = json.getInt("recipeID")
        val links = json.getJSONObject("links")
        var weburl =
          if(links.has("recipeUrl") && !links.isNull("recipeUrl")) links.getJSONObject("recipeUrl").getString("href")
          else ""
        val apiurl =
          if(links.has("parent") && !links.isNull("parent")) links.getJSONObject("parent").getString("href")
          else ""
        val id = Utils.deduceRecipeID(prior_id, weburl, apiurl)
        result :+= (id, weburl, apiurl)
        i += 1
      }
      result
    }

    def scrapeFavList(json : String) : Seq[(Long, String, String)] = {
      val mainJSON = new JSONObject(json)
      var result : Seq[(Long, String, String)] = Seq()

      val arr = mainJSON.getJSONArray("items")
      var i = 0
      while(i < arr.length()){
        val json = arr.getJSONObject(i).getJSONObject("recipeSummary")
        val links = json.getJSONObject("links")
        val prior_id = json.getInt("recipeID")
        var weburl =
          if(links.has("recipeUrl") && !links.isNull("recipeUrl")) links.getJSONObject("recipeUrl").getString("href")
          else ""
        val apiurl =
          if(links.has("parent") && !links.isNull("parent")) links.getJSONObject("parent").getString("href")
          else ""
        val id = Utils.deduceRecipeID(prior_id, weburl, apiurl)
        result :+= (id, weburl, apiurl)
        i += 1
      }
      result
    }

    def getRecipes(author_id : Long, list : String, nrecipes : Int, scraped : Set[Long]) : (Seq[Recipe], Set[Long]) = {
      var new_recipes : Seq[Recipe] = Seq()
      var repeated_recipes : Set[Long] = Set()
      val delay = connectionProperties.getProperty("delay-recipe").toLong
      val nrecipes_delay = connectionProperties.getProperty("nrecipes").toLong
      val delay_nrecipes = connectionProperties.getProperty("delay-nrecipes").toLong
      val ret = HttpManager.requestUserRecipes(author_id, connectionProperties, nrecipes, 1, list)
      if(ret.isEmpty){
        logger.error(s"Could not retrieve user recipe list ${list} for id ${author_id}")
      }
      else{
        val lst = if(list == "fav") scrapeFavList(ret.get) else extRecipes(ret.get)
        var err = false
        var recipes = 0

        lst.foreach({case(id, web, api) => {
          logger.debug(s"Processing recipe with id ${id}")
          var exists = false
          //DB check
          try {
            exists = daoDB.existsRecipe(id.toString)
          } catch {
            case sql : SQLException =>
              logger.error(sql.getMessage)
          }

          if(exists || repeated_recipes.contains(id) || scraped.contains(id)){
            logger.debug(s"Recipe with id ${id} has already been scraped")
            if(!repeated_recipes.contains(id))
              repeated_recipes += id
          }
          else {
            val potRecipe = extractRecipe(author.id, web, api, connectionProperties, csvDelimiter)
            recipes += 1
            if (recipes % nrecipes_delay == 0) {
              logger.info(s"Extracted ${recipes} recipes. Sleeping...")
              Thread.sleep(delay_nrecipes)
            }
            else
              Thread.sleep(delay)
            if (potRecipe.isEmpty) {
              err = true
            }
            else {
              /*
              try {
                logger.debug(s"Inserting recipe ${id} into DB")
                daoDB.insertRecipe(potRecipe.get.id.toString)
              } catch {
                case sql : SQLException =>
                  logger.fatal(sql.getMessage)
              }
              */
              new_recipes :+= potRecipe.get
              repeated_recipes += id
            }
          }
        }})
        /*
        result = lst.flatMap({case(id, web, api) => {
          //errores sql el insertar en recetas y csv auxliar para usuario por receta

          var exists = false
          //DB check
          try {
            exists = daoDB.existsRecipe(id.toString)
          } catch {
            case sql : SQLException =>
              logger.fatal(sql.getMessage)
          }

          if(exists){
            logger.info(s"Recipe with id ${id} has already been processed")
            Seq()
          }
          else {
            val potRecipe = extractRecipe(author.id, web, api, connectionProperties, csvDelimiter)
            recipes += 1
            if (recipes % nrecipes_delay == 0) {
              logger.info(s"Extracted ${recipes} recipes. Sleeping...")
              Thread.sleep(delay_nrecipes)
            }
            else
              Thread.sleep(delay)
            if (potRecipe.isEmpty) {
              err = true
              Seq()
            }
            else {
              Seq(potRecipe.get)
            }
          }
        }})
        */
      }
      (new_recipes, repeated_recipes)
    }

    var map_new : Map[String, Seq[Recipe]] = Map()
    var map_repeated : Map[String, Seq[Long]] = Map()
    val map = getRecipesExtraction(author, maxrecipes)
    var left = maxrecipes
    val delay = connectionProperties.getProperty("delay-recipe-list").toLong

    var new_recipes_recipes : Set[Recipe] = Set()
    var repeated_recipes_recipes : Set[Long] = Set()

    var new_recipes_madeit : Set[Recipe] = Set()
    var repeated_recipes_madeit : Set[Long] = Set()

    var new_recipes_fav : Set[Recipe] = Set()
    var repeated_recipes_fav : Set[Long] = Set()

    if(map("recipes") > 0){
      val ret = getRecipes(author.id, "recipes", map("recipes").min(left), Set())
      new_recipes_recipes ++= ret._1
      repeated_recipes_recipes ++= ret._2
      //map_new += "recipes" -> ret._1
      //map_repeated += "recipes" -> ret._2
      left -= (new_recipes_recipes.size + repeated_recipes_recipes.size)
    }
    if(left > 0){
      logger.info("Extracted user recipes. Sleeping...")
      Thread.sleep(delay)

      if(map("madeit") > 0){
        val ret = getRecipes(author.id, "madeit", map("madeit").min(left), repeated_recipes_recipes)
        new_recipes_madeit ++= ret._1
        repeated_recipes_madeit ++= ret._2
        left -= (new_recipes_madeit.size + repeated_recipes_madeit.size)
      }
      if(left > 0){
        logger.info("Extracted user madeit. Sleeping...")
        Thread.sleep(delay)

        if(map("fav") > 0){
          val ret = getRecipes(author.id, "fav", map("fav").min(left), repeated_recipes_recipes ++ repeated_recipes_madeit)
          new_recipes_fav ++= ret._1
          repeated_recipes_fav ++= ret._2
          left -= (new_recipes_fav.size + repeated_recipes_fav.size)
        }
      }
    }
    map_new += "recipes" -> new_recipes_recipes.toSeq
    map_new += "madeit" -> new_recipes_madeit.toSeq
    map_new += "fav" -> new_recipes_fav.toSeq
    map_repeated += "recipes" -> repeated_recipes_recipes.toSeq
    map_repeated += "madeit" -> repeated_recipes_madeit.toSeq
    map_repeated += "fav" -> repeated_recipes_fav.toSeq
    Some((map_new, map_repeated))
  }

  def extractUserReviews(author_id : Long, connectionProperties : Properties, csvDelimiter : String, nreviews : Int) : Option[Seq[Review]] = {
    val pagesize = connectionProperties.getProperty("max-pagesize").toInt
    val npages = nreviews / pagesize
    var result : Seq[Review] = Seq()
    try{
      (1 to npages).foreach(i => {
        val ret = HttpManager.requestUserReviews(author_id, connectionProperties, pagesize, i)
        if(ret.isEmpty){
          logger.error(s"Could not retrieve user reviews for id ${author_id}")
          return None
        }
        val list : Seq[Review] = Scraper.scrapeReviewsList(author_id, ret.get, csvDelimiter)
        result ++= list
      })
      Some(result)
    } catch {
      case sde : ScrapingDetectionException =>
        logger.fatal(sde)
        return None
      case aae : APIAuthorizationException =>
        logger.fatal(aae)
        val newProps = HttpManager.resetToken(connectionProperties)
        if(newProps.isDefined) {
          val rt = extractUserReviews(author_id, newProps.get, csvDelimiter, nreviews)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def extractCategories(url : String, connectionProperties : Properties) : Option[JSONArray] = {
    try{
      val ret = HttpManager.requestCategory(url, connectionProperties)
      if(ret.isEmpty){
        logger.error(s"Could not retrieve categories for URL ${url}")
        return None
      }
      val json = Scraper.scrapeCategories(ret.get)
      if(json.isEmpty){
        logger.error(s"Could not scrape categories for URL ${url}")
        return None
      }
      else
        Some(json.get)
    } catch {
      case sde : ScrapingDetectionException =>
        logger.fatal(sde)
        None
      case aae : APIAuthorizationException =>
        logger.fatal(aae)
        val newProps = HttpManager.resetToken(connectionProperties)
        if(newProps.isDefined) {
          val rt = extractCategories(url, newProps.get)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def getRecipesExtraction(user : User, maxrecipes : Int) : mutable.Map[String, Int] = {

    def ceil10(n : Int) : Int = {
      (10 * scala.math.ceil(n.toDouble / 10.toDouble)).toInt
    }

    var result: mutable.Map[String, Int] = mutable.Map(
      "recipes" -> 0,
      "madeit" -> 0,
      "fav" -> 0
    )
    var curr = 0
    if (user.recipeCount >= maxrecipes) {
      result("recipes") = maxrecipes
    }
    else {
      curr = user.recipeCount
      result("recipes") = ceil10(curr)
      var left = maxrecipes - curr
      if (user.madeitCount >= left) {
        result("madeit") = ceil10(left)
      }
      else {
        curr += user.madeitCount
        result("madeit") = ceil10(curr)
        left = maxrecipes - curr
        result("fav") = ceil10(left)
      }
    }
    result
  }

}
