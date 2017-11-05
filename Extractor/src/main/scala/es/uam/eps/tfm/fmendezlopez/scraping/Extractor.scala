package es.uam.eps.tfm.fmendezlopez.scraping

import java.sql.SQLException

import es.uam.eps.tfm.fmendezlopez.dao.DatabaseDAO
import es.uam.eps.tfm.fmendezlopez.dto.{Recipe, Review, User}
import es.uam.eps.tfm.fmendezlopez.exception.{APIAuthorizationException, APILoginException, ScrapingDetectionException}
import es.uam.eps.tfm.fmendezlopez.utils.{HttpManager, Logging, Utils}
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

  def extractRecipe(author_id: Long, webURL: String, apiURL: String, csvDelimiter: String): Option[Recipe] ={

    try{
      logger.debug(s"Extracting recipe with webURL ${webURL} and apiURL ${apiURL}")
      val html_str = if(webURL.isEmpty) None else HttpManager.requestRecipeWebURL(webURL)
      if(html_str.isEmpty){
        logger.error(s"Could not retrieve recipe with url ${webURL} from WEB")
      }
      val json_str = HttpManager.requestRecipeAPIURL(apiURL)
      if(json_str.isEmpty){
        logger.error(s"Could not retrieve recipe with url ${apiURL} from API")
        return None
      }
      Some(Scraper.scrapeRecipe(json_str.get, html_str, csvDelimiter, HttpManager.getProperty("default-category").toLong, HttpManager.getProperty("similar-threshold").toInt))
    } catch {
      case sde : ScrapingDetectionException =>
        logger.error(sde)
        return None

      case ale : APILoginException =>
        logger.error(ale)
        return None

      case aae : APIAuthorizationException =>
        logger.error(aae)
        if(HttpManager.resetToken) {
          val rt = extractRecipe(author_id, webURL, apiURL, csvDelimiter)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def extractUser(id : Long, csvDelimiter : String, complete : Boolean = true) : Option[User] = {
    try{
      var ret : Option[String] = None
      if(complete){
        HttpManager.requestUserWeb(id)
      }
      ret = HttpManager.requestUserAPI(id)
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
        if(HttpManager.resetToken) {
          val rt = extractUser(id, csvDelimiter, complete)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def extractFollowing(id : Long, csvDelimiter : String, nusers : Int) : Option[Seq[User]] = {
    val pagesize = HttpManager.getProperty("max-pagesize").toInt
    val npages = nusers / pagesize
    var result : Seq[User] = Seq()
    try{
      (1 to npages).foreach(i => {
        val ret = HttpManager.requestFollowingAPI(id, pagesize = pagesize, pageNumber = i)
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
        if(HttpManager.resetToken) {
          val rt = extractFollowing(id, csvDelimiter, nusers)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }
  def extractFollowers(id : Long, csvDelimiter : String, nusers : Int) : Option[Seq[User]] = {
    val pageSize = HttpManager.getProperty("max-pagesize").toInt
    val npages = nusers / pageSize
    var result : Seq[User] = Seq()
    try{
      (1 to npages).foreach(i => {
        val ret = HttpManager.requestFollowersAPI(id, pagesize = pageSize, pageNumber = i)
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
        if(HttpManager.resetToken) {
          val rt = extractFollowers(id, csvDelimiter, nusers)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  def extractUserRecipes(author : User, csvDelimiter : String, maxrecipes : Int) : Option[(Map[String, Seq[Recipe]], Map[String, Seq[Long]])] = {

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
        if(weburl.isEmpty) weburl = Utils.compoundRecipeURL(id.toString, HttpManager.getProperty("base-recipe-web"))
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
        if(weburl.isEmpty) weburl = Utils.compoundRecipeURL(id.toString, HttpManager.getProperty("base-recipe-web"))
        result :+= (id, weburl, apiurl)
        i += 1
      }
      result
    }

    @throws(classOf[ScrapingDetectionException])
    def getRecipes(author_id : Long, lst : String, nrecipes : Int, scraped : Set[Long], maxrecipes : Int) : (Seq[Recipe], Set[Long]) = {
      logger.debug(s"Getting recipes for list ${lst}")
      def ceil10(n : Int) : Int = {
        (10 * scala.math.ceil(n.toDouble / 10.toDouble)).toInt
      }

      var new_recipes : Seq[Recipe] = Seq()
      var repeated_recipes : Set[Long] = Set()
      val delay = HttpManager.getProperty("delay-recipe").toLong
      val nrecipes_delay = HttpManager.getProperty("nrecipes").toLong
      val delay_nrecipes = HttpManager.getProperty("delay-nrecipes").toLong
      var retry = false
      var page = 1
      val pageSize = ceil10(nrecipes)
      val maxpages = math.ceil(maxrecipes.toDouble / pageSize).toInt
      var recipes = 0
      do{
        var ret: Option[String] = None
        try{
          ret = HttpManager.requestUserRecipes(author_id, pagesize = pageSize, pageNumber = page, list = lst)
        } catch {
          case ale : APILoginException =>
            logger.error(ale)
            retry = false

          case aae : APIAuthorizationException =>
            logger.error(aae)
            if(HttpManager.resetToken) {
              ret = HttpManager.requestUserRecipes(author_id, pagesize = pageSize, pageNumber = page, list = lst)
              if(ret.isEmpty)
                throw new ScrapingDetectionException(aae.getMessage)
            }
        }
        if(ret.isEmpty){
          logger.error(s"Could not retrieve user recipe list ${lst} for id ${author_id}")
          retry = false
        }
        else{
          val lstR = if(lst == "fav") scrapeFavList(ret.get) else extRecipes(ret.get)
          if(lstR.isEmpty){
            logger.warn(s"List ${lst} seems to be empty for user ${author_id}")
            retry = false
          }
          else{
            val it = lstR.iterator
            do{
              val (id, web, api) = it.next()
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
                recipes += 1
              }
              else {
                var potRecipe: Option[Recipe] = None
                try{
                  potRecipe = extractRecipe(author.id, web, api, csvDelimiter)
                } catch {
                  case ale : APILoginException =>
                    logger.error(ale)
                    retry = false

                  case aae : APIAuthorizationException =>
                    logger.error(aae)
                    val newProps = HttpManager.resetToken
                    if(HttpManager.resetToken) {
                      potRecipe = extractRecipe(author.id, web, api, csvDelimiter)
                      if(potRecipe.isEmpty)
                        throw new ScrapingDetectionException(aae.getMessage)
                    }
                    else
                      retry = false
                }
                if (recipes != 0 && recipes % nrecipes_delay == 0) {
                  logger.info(s"Extracted ${recipes} recipes. Sleeping...")
                  Thread.sleep(delay_nrecipes)
                }
                else
                  Thread.sleep(delay)
                if (potRecipe.isDefined) {
                  new_recipes :+= potRecipe.get
                  //repeated_recipes += id
                  recipes += 1
                }
              }
            }while(it.hasNext && recipes < nrecipes)
            retry = recipes < nrecipes && page < maxpages
          }
        }
        page += 1
      }while(retry)
      logger.debug(s"Got ${new_recipes.length} new recipes and ${repeated_recipes.size} repeated recipes")
      (new_recipes, repeated_recipes)
    }

    var map_new : Map[String, Seq[Recipe]] = Map()
    var map_repeated : Map[String, Seq[Long]] = Map()
    val map = getRecipesExtraction(author, maxrecipes)
    val delay = HttpManager.getProperty("delay-recipe-list").toLong

    var new_recipes_recipes : Set[Recipe] = Set()
    var repeated_recipes_recipes : Set[Long] = Set()

    var new_recipes_madeit : Set[Recipe] = Set()
    var repeated_recipes_madeit : Set[Long] = Set()

    var new_recipes_fav : Set[Recipe] = Set()
    var repeated_recipes_fav : Set[Long] = Set()

    var nextSet: Set[Long] = Set()

    if(map("recipes") > 0){
      val ret = getRecipes(author.id, "recipes", map("recipes"), Set(), author.recipeCount)
      new_recipes_recipes ++= ret._1
      repeated_recipes_recipes ++= ret._2
      val total = new_recipes_recipes.size + repeated_recipes_recipes.size
      if(total < map("recipes")){
        val diff = map("recipes") - total
        logger.debug(s"Extracted less recipes from user. Added ${diff} recipes to madeit.")
        map("madeit") += diff
      }
      if(new_recipes_recipes.size > 0) {
        logger.info("Extracted user recipes. Sleeping...")
        Thread.sleep(delay)
      }
      nextSet ++= new_recipes_recipes.flatMap(r=>Set(r.id))
    }
    if(map("madeit") > 0){
      val ret = getRecipes(author.id, "madeit", map("madeit"), nextSet, author.madeitCount)
      new_recipes_madeit ++= ret._1
      repeated_recipes_madeit ++= ret._2
      val total = new_recipes_madeit.size + repeated_recipes_madeit.size
      if(total < map("madeit")){
        val diff = map("madeit") - total
        logger.debug(s"Extracted less madeit from user. Added ${diff} recipes to fav.")
        map("fav") += diff
      }
      //map_new += "recipes" -> ret._1
      //map_repeated += "recipes" -> ret._2
      //left -= (new_recipes_recipes.size + repeated_recipes_recipes.size)
      if(new_recipes_madeit.size > 0) {
        logger.info("Extracted user madeit. Sleeping...")
        Thread.sleep(delay)
      }
      nextSet ++= new_recipes_madeit.flatMap(r=>Set(r.id))
    }
    if(map("fav") > 0){
      val ret = getRecipes(author.id, "fav", map("fav"), nextSet, author.favCount)
      new_recipes_fav ++= ret._1

      //map_new += "recipes" -> ret._1
      //map_repeated += "recipes" -> ret._2
      //left -= (new_recipes_recipes.size + repeated_recipes_recipes.size)
      if(new_recipes_fav.size > 0) {
        logger.info("Extracted user fav. Sleeping...")
        Thread.sleep(delay)
      }
      repeated_recipes_fav ++= ret._2
    }
    map_new += "recipes" -> new_recipes_recipes.toSeq
    map_new += "madeit" -> new_recipes_madeit.toSeq
    map_new += "fav" -> new_recipes_fav.toSeq
    map_repeated += "recipes" -> repeated_recipes_recipes.toSeq
    map_repeated += "madeit" -> repeated_recipes_madeit.toSeq
    map_repeated += "fav" -> repeated_recipes_fav.toSeq
    Some((map_new, map_repeated))
  }

  def extractUserReviews(author_id : Long, csvDelimiter : String, nReviews : Int) : Option[Seq[Review]] = {
    val pageSize = HttpManager.getProperty("max-pagesize").toInt
    val npages = nReviews / pageSize
    var result : Seq[Review] = Seq()
    try{
      (1 to npages).foreach(i => {
        val ret = HttpManager.requestUserReviews(author_id, pagesize = pageSize, pageNumber = i)
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
        val newProps = HttpManager.resetToken
        if(HttpManager.resetToken) {
          val rt = extractUserReviews(author_id,  csvDelimiter, nReviews)
          if(rt.isEmpty)
            None
          else
            rt
        }
        else
          None
    }
  }

  def extractRecipeFromReviews(author: User, reviews: Seq[Review], newIDs: Set[Long], csvDelimiter : String) : (Seq[Recipe], Seq[Long]) = {

    val daoDB = DatabaseDAO.getInstance()

    val delay = HttpManager.getProperty("delay-recipe").toLong
    val nrecipes_delay = HttpManager.getProperty("nrecipes").toLong
    val delay_nrecipes = HttpManager.getProperty("delay-nrecipes").toLong
    var nrecipes = 0
    var new_recipes: Seq[Recipe] = Seq()
    var repeated_recipes: Seq[Long] = Seq()
    reviews.foreach(review => {
      val id = review.recipe.id
      var exists = false
      //DB check
      try {
        exists = daoDB.existsRecipe(id.toString)
      } catch {
        case sql : SQLException =>
          logger.error(sql.getMessage)
      }
      if(!exists && !newIDs.contains(id)) {
        val potRecipe = extractRecipe(author.id, review.recipe.weburl, review.recipe.apiurl, csvDelimiter)
        if (potRecipe.isDefined) {
          nrecipes += 1
          if (nrecipes % nrecipes_delay == 0) {
            logger.info(s"Extracted ${nrecipes} recipes. Sleeping...")
            Thread.sleep(delay_nrecipes)
          }
          else
            Thread.sleep(delay)
          new_recipes :+= potRecipe.get
        }
      }
      else
        repeated_recipes :+= id
    })

    (new_recipes, repeated_recipes)
  }

  def extractCategories(url : String) : Option[JSONArray] = {
    try{
      val ret = HttpManager.requestCategory(url)
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
        if(HttpManager.resetToken) {
          val rt = extractCategories(url)
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

    val result: mutable.Map[String, Int] = mutable.Map(
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
      result("recipes") = curr
      var left = maxrecipes - curr
      if (user.madeitCount >= left) {
        result("madeit") = left
      }
      else {
        curr += user.madeitCount
        result("madeit") = user.madeitCount
        left = maxrecipes - curr
        result("fav") = left.min(user.favCount)
      }
    }
    result
  }
}
