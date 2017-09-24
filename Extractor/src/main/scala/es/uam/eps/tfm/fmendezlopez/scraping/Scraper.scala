package es.uam.eps.tfm.fmendezlopez.scraping

import java.io.File
import java.time.Duration
import java.util.{Properties, StringTokenizer}

import es.uam.eps.tfm.fmendezlopez.dto._
import es.uam.eps.tfm.fmendezlopez.utils.{Logging, Utils}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.{Element, TextNode}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors._
import org.apache.commons.configuration2.Configuration
import org.apache.commons.lang3.StringEscapeUtils
import org.apache.xml.serializer.utils.StringToIntTable
import org.json.{JSONArray, JSONObject}

import scala.collection.mutable

/**
  * Created by Francisco on 14/04/2017.
  */
object Scraper extends Logging{

  private var properties : Configuration = _
  private var baseURL : String = _
  private var nutritionKeys : Seq[String] = _
  private var nutritionMapping : Map[String, String] = _
  private var footnotesKeys : Map[String, String] = _

  private var recipe_id = -2

  def setProperties(properties : Configuration) : Unit = {
    this.properties = properties
    baseURL = properties.getString("allrecipes.url.base")

    val nutritionStr = properties.getString("general.scraping.recipe.nutrition.keys")
    nutritionKeys = Seq()
    nutritionKeys = nutritionStr.split('|')
    val nutritionMappingStr = properties.getString("stage2.scraping.nutrients.mapping")
    nutritionMapping = Map(nutritionKeys.zip(nutritionMappingStr.split(',')) :_*)
    nutritionMappingStr.split('|').foreach(str => {
      val arr = str.split(',')
      nutritionMapping += (arr(0) -> arr(1))
    })

    val footnotesStr = properties.getString("general.scraping.recipe.footnotes.keys")
    footnotesKeys = Map()
    footnotesStr.split('|').foreach(str => footnotesKeys += (str -> ""))
  }

  def scrapeRecipe(author_id : Long, json : String, html : String, csvSeparator : String): Recipe = {
    val mainJSON = new JSONObject(json)
    val result = new Recipe()
    var id : Long = -1
    val prior_id = mainJSON.getLong("recipeID")
    result.title_=(mainJSON.getString("title").replace(csvSeparator, " "))
    val links = mainJSON.getJSONObject("links")
    var weburl =
      if(links.has("recipeUrl") && !links.isNull("recipeUrl")) links.getJSONObject("recipeUrl").getString("href")
      else ""
    val apiurl =
      if(links.has("self") && !links.isNull("self")) links.getJSONObject("self").getString("href").replace(csvSeparator, " ")
      else ""
    result.url_=(weburl)
    id = Utils.deduceRecipeID(prior_id, weburl, apiurl)
    result.id_=(id)
    val json_author = mainJSON.getJSONObject("submitter")
    val author = new Author
    //author.id_=(json_author.getLong("userID"))
    author.id_=(author_id)
    author.url_=(baseURL + json_author.getString("profileUrl").replace(csvSeparator, " "))
    result.author_=(author)
    result.rating_=(mainJSON.getDouble("ratingAverage"))
    result.ratingCount_=(mainJSON.getInt("ratingCount"))
    result.reviewCount_=(mainJSON.getInt("reviewCount"))
    result.description_=(mainJSON.getString("description").replace(csvSeparator, " "))
    result.servings_=(mainJSON.getInt("servings"))

    if(mainJSON.isNull("nutrition"))
      result.nutritionInfo_=(None)
    else {
      val nutrition = mainJSON.getJSONObject("nutrition")
      result.nutritionInfo_=(Some(scrapeNutritionInfo(nutrition, csvSeparator)))
    }
    if(mainJSON.isNull("ingredients"))
      result.ingredients_=(Seq())
    else {
      val ingredients = mainJSON.getJSONArray("ingredients")
      result.ingredients_=(scrapeIngredients(ingredients, csvSeparator))
    }
    result.prepTime_=(mainJSON.optDouble("prepMinutes", 0).toInt)
    result.cookTime_=(mainJSON.optDouble("cookMinutes", 0).toInt)
    result.totalTime_=(mainJSON.optDouble("readyInMinutes", 0).toInt)
    val steps = mainJSON.getJSONArray("directions")
    result.steps_=(scrapeDirections(steps, csvSeparator))
    if(!mainJSON.isNull("footnotes")){
      val notes = mainJSON.getJSONArray("footnotes")
      result.notes_=(scrapeNotes(notes, csvSeparator))
    }
    else{
      result.notes_=(mutable.Map() ++= footnotesKeys)
    }
    if(!html.isEmpty) {
      result.madeit_=(scrapeMadeIt(html))
      val category = scrapeCategory(html)
      result.category_=(category)
    }
    else {
      result.madeit_=(0)
      val cat = new RecipeCategory
      cat.id_=(-1)
      cat.url_=("")
      result.category_=(cat)
    }
    result
  }

  def scrapeRecipeList(json : String, csvSeparator : String): Seq[Recipe] = {
    val mainJSON = new JSONObject(json)
    var result : Seq[Recipe] = Seq()

    val arr = mainJSON.getJSONArray("recipes")
    var i = 0
    while(i < arr.length()){
      val json = arr.getJSONObject(i)
      val recipe = new Recipe()
      recipe.id_=(json.getLong("recipeID"))
      recipe.title_=(json.getString("title").replace(csvSeparator, " "))
      recipe.url_=(json.getJSONObject("links").getJSONObject("recipeUrl").getString("href").replace(csvSeparator, " "))
      val json_author = json.getJSONObject("submitter")
      val author = new Author
      author.id_=(json_author.getLong("userID"))
      author.url_=(baseURL + json_author.getString("profileUrl").replace(csvSeparator, " "))
      recipe.author_=(author)
      recipe.rating_=(json.getDouble("ratingAverage"))
      recipe.ratingCount_=(json.getInt("ratingCount"))
      recipe.reviewCount_=(json.getInt("reviewCount"))
      recipe.description_=(json.getString("description").replace(csvSeparator, " "))
      recipe.servings_=(json.getInt("servings"))

      if(json.isNull("nutrition"))
        recipe.nutritionInfo_=(None)
      else {
        val nutrition = json.getJSONObject("nutrition")
        recipe.nutritionInfo_=(Some(scrapeNutritionInfo(nutrition, csvSeparator)))
      }
      val ingredients = json.getJSONArray("ingredients")
      recipe.ingredients_=(scrapeIngredients(ingredients, csvSeparator))
      recipe.prepTime_=(json.optDouble("prepMinutes", 0).toInt)
      recipe.cookTime_=(json.optDouble("cookMinutes", 0).toInt)
      recipe.totalTime_=(json.optDouble("readyInMinutes", 0).toInt)
      val steps = json.getJSONArray("directions")
      recipe.steps_=(scrapeDirections(steps, csvSeparator))
      val notes = json.getJSONArray("footnotes")
      recipe.notes_=(scrapeNotes(notes, csvSeparator))
      result :+= recipe
      i += 1
    }
    result
  }

  def scrapeFavList(json : String) : Seq[UserRecipe] = {
    val mainJSON = new JSONObject(json)
    var result : Seq[UserRecipe] = Seq()

    val arr = mainJSON.getJSONArray("items")
    var i = 0
    while(i < arr.length()){
      val json = arr.getJSONObject(i).getJSONObject("recipeSummary")
      val recipe = new UserRecipe()
      recipe.id_=(json.getLong("recipeID"))
      val links = json.getJSONObject("links")
      val web = links.getJSONObject("recipeUrl").getString("href")
      val api = links.getJSONObject("parent").getString("href")
      recipe.apiURL_=(api)
      recipe.webURL_=(web)
      result :+= recipe
      i += 1
    }
    result
  }

  def scrapeCategories(html : String) : Option[JSONArray] = {
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val scripts: List[Element] = doc >> elementList("script")
    val elements = scripts.filter(_.innerHtml.contains("hubCategories"))
    if(elements.length == 1){
      val script = elements.head.innerHtml
      val startIndex = script.indexOf("[")
      val mainJSONStr = StringEscapeUtils.unescapeJson(script.substring(startIndex, script.length - 3))
      val mainJSONObj = new JSONArray(mainJSONStr)
      Some(mainJSONObj)
    }
    else{
      None
    }
  }

  def scrapeNutritionInfo(json : JSONObject, csvSeparator : String) : NutritionInfo = {
    def getNutrient(json : JSONObject, key : String) : Nutrient = {
      val result = new Nutrient
      val name = key
      val amount = json.optDouble("amount", 0.0)
      val unit = json.optString("unit", "")
      val percent = json.optDouble("percentDailyValue", 0.0)
      result.name_=(name.replace(csvSeparator, " "))
      result.amount_=(resolveAmount(key, amount, nutritionMapping(key)))
      result.unit_=(unit.replace(csvSeparator, " "))
      result.percent_=(percent)
      result
    }
    def resolveAmount(nutrient : String, amount : Double, currentUnit : String) : Double = {
      val expectedUnit = nutritionMapping(nutrient)
      if(expectedUnit.equals(currentUnit))
        return amount
      var ret = amount
      currentUnit match {
        case "g" =>
          expectedUnit match {
            case "kg" => ret /= 1000
            case "mg" => ret /= 1000
            case "mcg" => ret /= 1000
            case _ => ret = ret
          }
        case "mg" =>
          expectedUnit match {
            case "kg" => ret /= 1000000
            case "g" => ret /= 1000
            case "mcg" => ret *= 1000
            case _ => ret = ret
          }
        case "kg" =>
          expectedUnit match {
            case "g" => ret *= 1000
            case "mg" => ret *= 1000000
            case "mcg" => ret *= 1000000000
            case _ => ret = ret
          }
        case "mcg" =>
          expectedUnit match {
            case "kg" => ret /= 1000000000
            case "g" => ret /= 1000000
            case "mg" => ret /= 1000
            case _ => ret = ret
          }
        case "kcal" =>
          expectedUnit match {
            case "cal" => ret *= 1000
            case _ => ret = ret
          }
        case "cal" =>
          expectedUnit match {
            case "kcal" => ret /= 1000
            case _ => ret = ret
          }
        case _ => ret = ret
      }
      return ret
    }
    val result : NutritionInfo = new NutritionInfo
    var map : Map[String, Nutrient] = Map()
    nutritionKeys.foreach(nutrient => {
      if(json.has(nutrient) && !json.isNull(nutrient)) map += (nutrient -> getNutrient(json.getJSONObject(nutrient), nutrient))
      else map += (nutrient -> Nutrient.defaultNutrient(nutrient))
    })
    result.nutrients_=(map)
    result
  }

  def scrapeIngredients(json : JSONArray, csvSeparator : String) : Seq[Ingredient] = {
    var result : Seq[Ingredient] = Seq()
    var i = 0
    while(i < json.length()){
      val ingredient = json.optJSONObject(i)
      if(ingredient != null) {
        val id = ingredient.getInt("ingredientID")
        val displayValue = ingredient.getString("displayValue").replace(csvSeparator, " ")
        val amount = ingredient.getDouble("grams")
        val ingr = new Ingredient
        ingr.id_=(id)
        ingr.displayValue_=(displayValue.replace(csvSeparator, " "))
        ingr.amount_=(amount)
        result :+= ingr
      }
      i += 1
    }
    result
  }

  def scrapeDirections(json : JSONArray, csvSeparator : String) : Seq[(Int, String)] = {
    var result : Seq[(Int, String)] = Seq()
    var i = 0
    while(i < json.length()){
      val step = json.getJSONObject(i)
      val number = step.getInt("ordinal")
      val text = step.getString("displayValue")
      result :+= (number, text.replace(csvSeparator, " "))
      i += 1
    }
    result
  }

  def scrapeNotes(json : JSONArray, csvSeparator : String) : mutable.Map[String, String] = {
    val result : mutable.Map[String, String] = mutable.Map() ++= footnotesKeys
    var i = 0
    while(i < json.length()){
      val name_obj = json.getJSONObject(i)
      val name = name_obj.getString("text")
      i += 1
      if(result.contains(name) && i < json.length()){
        val note_obj = json.getJSONObject(i)
        val note = note_obj.getString("text")
        result(name) = note.replace(csvSeparator, " ")
        i += 1
      }
    }
    result
  }

  def scrapeMadeIt(html : String) : Int = {
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val body = (doc >> elementList("body")).head
    val sections = body >> elementList("section")
    var sections_filtered = sections.filter(element => if(element.hasAttr("class")) element.attr("class").contains("recipe-summary") else false)
    if(sections_filtered.length != 1){
      logger.error("Could not scrape recipe summary (madeit scraping)")
      return 0
    }
    val rs_section = sections_filtered.head
    val rs_divs: List[Element] = rs_section >> elementList("div")
    var rs_divs_filtered = rs_divs.filter(element => if(element.hasAttr("class")) element.attr("class").contains("recipe-summary__stars") else false)
    if(rs_divs_filtered.length != 1){
      logger.error("Could not scrape recipe rating info (div) (madeit scraping)")
      return 0
    }
    rs_divs_filtered = rs_divs
      .filter(element => if(element.hasAttr("class") && element.hasAttr("data-ng-init")) element.attr("class").contains("total-made-it") else false)
    if(rs_divs_filtered.length != 1){
      logger.error("Could not scrape recipe #madeit (subdiv) (madeit scraping)")
      return 0
    }
    val tokenizer = new StringTokenizer(rs_divs_filtered.head.attr("data-ng-init"))
    tokenizer.nextToken("(")
    val recipe_madeit = sanitizeHTMLString(tokenizer.nextToken(",").substring(1))
    recipe_madeit.toInt
  }

  def scrapeCategory(html : String) : RecipeCategory = {
    val category = new RecipeCategory
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val body = (doc >> elementList("body")).head
    val sections = body >> elementList("section")
    val sections_filtered = sections.filter(element => if(element.hasAttr("class")) element.attr("class").contains("ar_recipe_index full-page") else false)
    if(sections_filtered.length != 1){
      logger.error("Could not scrape recipe summary (category scraping)")
      return category
    }

    val uls : List[Element] = (sections_filtered.head >> elementList("ul"))
      .filter(element => if(element.hasAttr("class")) element.attr("class").contains("breadcrumbs") else false)
    if(uls.length != 1){
      logger.error("Could not scrape ul breadcrumbs (category scraping)")
      return category
    }

    val items : List[Element] = (uls.head >> elementList("li"))
      .filter(element => element.hasAttr("itemscope"))
    if(items.length < 1){
      logger.error("Could not scrape ul items (category scraping)")
      return category
    }

    val links : List[Element] = (items.last >> elementList("a"))
      .filter(element => if(element.hasAttr("data-internal-referrer-link")) element.attr("data-internal-referrer-link").contains("breadcrumb") else false)
    if(uls.length != 1){
      logger.error("Could not scrape a link for ul item breadcrumb (category scraping)")
      return category
    }
    val link = links.head
    val href = link.attr("href")
    val url = s"${properties.getString("allrecipes.url.base")}${href}"
    val tokenizer = new StringTokenizer(href, "/")
    tokenizer.nextToken
    var id : Long = -1
    if(tokenizer.hasMoreTokens) {
      id = tokenizer.nextToken.toLong
    }
    category.id_=(id)
    category.url_=(url)
    category
  }

  def scrapeReviewsList(author_id: Long, json : String, csvSeparator : String) : Seq[Review] = {
    val mainJSON = new JSONObject(json)
    var result : Seq[Review] = Seq()
    val arr = mainJSON.getJSONArray("reviews")
    var i = 0
    while(i < arr.length()){
      val review = arr.getJSONObject(i)
      val id = review.getLong("reviewID")
      val rating = review.getInt("rating")
      val text = review.getString("text")
      val dateTokenizer = new StringTokenizer(review.getString("dateLastModified"))
      val date = dateTokenizer.nextToken("T")
      val helpfulCount = review.getInt("helpfulCount")
      val author = new Author
      val json_author = review.getJSONObject("submitter")
      //author.id_=(json_author.getLong("userID"))
      author.id_=(author_id)
      author.url_=(baseURL + json_author.getString("profileUrl").replace(csvSeparator, " "))

      val recipe = new Recipe
      val json_recipe = review.getJSONObject("recipe")
      recipe.id_=(json_recipe.getLong("recipeID"))
      var rec_url = ""
      val links = json_recipe.getJSONObject("links")
      if(!links.isNull("recipeUrl")){
        val url = links.getJSONObject("recipeUrl")
        if(!url.isNull("href")){
          rec_url = url.getString("href")
        }
      }

      recipe.url_=(rec_url)

      val rev = new Review
      rev.id_=(id)
      rev.rating_=(rating)
      rev.text_=(text.replace(csvSeparator, " "))
      rev.date_=(date)
      rev.helpfulCount_=(helpfulCount)
      rev.author_=(author)
      rev.recipe_=(recipe)
      result :+= rev
      i += 1
    }
    result
  }

  def scrapeUser(input : Either[String, JSONObject], csvDelimiter : String) : User = {
    val mainJSON : JSONObject = if(input.isLeft) new JSONObject(input.left.get) else input.right.get
    val user = new User
    user.id_=(mainJSON.getLong("userID"))
    user.name_=(mainJSON.getString("name").replace(csvDelimiter, " "))
    user.followerCount_=(mainJSON.getInt("followersCount"))
    user.followingCount_=(mainJSON.getInt("followingCount"))
    user.madeitCount=(mainJSON.getInt("madeRecipesCount"))
    user.favCount=(mainJSON.getInt("favoritesCount"))
    user.ratingCount_=(mainJSON.getInt("ratingsCount"))
    user.recipeCount_=(mainJSON.getInt("personalRecipeSharedCount"))
    user.reviewCount_=(mainJSON.getInt("reviewsCount"))
    user.city_=(mainJSON.optString("city", "").replace(csvDelimiter, " "))
    user.region_=(mainJSON.optString("region", "").replace(csvDelimiter, " "))
    user.country_=(mainJSON.optString("country", "").replace(csvDelimiter, " "))
    user.handle_=(mainJSON.getString("handle").replace(csvDelimiter, " "))
    user.profileUrl_=(s"${properties.getString("allrecipes.url.base")}${mainJSON.getString("profileUrl")}")
    user
  }

  def scrapeUserList(json : String, csvDelimiter : String) : Seq[User] = {
    val mainJSON = new JSONObject(json)
    var result : Seq[User] = Seq()
    val arr = mainJSON.getJSONArray("users")
    var i = 0
    while(i < arr.length()){
      val user = arr.getJSONObject(i)
      result :+= scrapeUser(Right(user), csvDelimiter)
      i += 1
    }
    result
  }
















  def scrapeRecipe(html : String): Option[Recipe] = {
    val result : Recipe = new Recipe
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)

    //head
    val head = (doc >> elementList("head")).head
    val urls = head >> elementList("link#canonicalUrl")
    if(urls.length != 1){
      logger.error("Could not scrape recipe canonical url")
      return None
    }
    val recipe_url = sanitizeHTMLString(urls.head.attr("href"))
    result.url_=(s"${baseURL}${recipe_url}")

    //Recipe summary
    val body = (doc >> elementList("body")).head
    val recipe_id = new JSONObject(body.attr("data-scoby-impression")).getString("id")
    result.id_=(recipe_id.toInt)

    val sections = body >> elementList("section")
    var sections_filtered = sections.filter(element => if(element.hasAttr("class")) element.attr("class").contains("recipe-summary") else false)
    if(sections_filtered.length != 1){
      logger.error("Could not scrape recipe summary")
      return None
    }
    val rs_section = sections_filtered.head
    val rs_h1s: List[Element] = rs_section >> elementList("h1")
    val rs_h1s_filtered = rs_h1s.filter(element => if(element.hasAttr("itemprop")) element.attr("itemprop").equals("name") else false)
    if(rs_h1s_filtered.length != 1){
      logger.error("Could not scrape recipe name")
      return None
    }
    val recipe_title = sanitizeHTMLString(rs_h1s_filtered.head.innerHtml)
    result.title_=(recipe_title)

    val rs_divs: List[Element] = rs_section >> elementList("div")
    var rs_divs_filtered = rs_divs.filter(element => if(element.hasAttr("class")) element.attr("class").contains("recipe-summary__stars") else false)
    if(rs_divs_filtered.length != 1){
      logger.error("Could not scrape recipe rating info (div)")
      return None
    }

    var rs_spans_filtered : List[Element] = (rs_divs_filtered.head >> elementList("span"))
      .filter(element => if(element.hasAttr("itemprop")) element.attr("itemprop").contains("aggregateRating") else false)
    if(rs_spans_filtered.length != 1){
      logger.error("Could not scrape recipe rating info (span)")
      return None
    }

    val rs_spans_meta_filtered : List[Element] = (rs_spans_filtered.head >> elementList("meta"))
      .filter(element => element.hasAttr("itemprop"))
    if(rs_spans_meta_filtered.length < 2){
      logger.error("Could not scrape recipe rating info (meta)")
      return None
    }
    var recipe_rating : String = ""
    var recipe_review_count : String = ""

    if(rs_spans_meta_filtered(0).attr("itemprop").equals("ratingValue")){
      recipe_rating = sanitizeHTMLString(rs_spans_meta_filtered(0).attr("content"))
       recipe_review_count = sanitizeHTMLString(rs_spans_meta_filtered(1).attr("content"))
    }
    else{
      recipe_rating = sanitizeHTMLString(rs_spans_meta_filtered(1).attr("content"))
      recipe_review_count = sanitizeHTMLString(rs_spans_meta_filtered(0).attr("content"))
    }
    result.rating_=(recipe_rating.toDouble)
    result.reviewCount_=(recipe_review_count.toInt)

    rs_divs_filtered = rs_divs
      .filter(element => if(element.hasAttr("class") && element.hasAttr("data-ng-init")) element.attr("class").contains("total-made-it") else false)
    if(rs_divs_filtered.length != 1){
      logger.error("Could not scrape recipe #madeit (subdiv)")
      return None
    }
    val tokenizer = new StringTokenizer(rs_divs_filtered.head.attr("data-ng-init"))
    tokenizer.nextToken("(")
    val recipe_madeit = sanitizeHTMLString(tokenizer.nextToken(",").substring(1))
    result.madeit_=(recipe_madeit.toInt)

    rs_divs_filtered = rs_divs
      .filter(element => if(element.hasAttr("class")) element.attr("class").contains("submitter__img") else false)
    if(rs_divs_filtered.length != 1){
      logger.error("Could not scrape recipe submitter (subdiv submitter)")
      return None
    }

    rs_spans_filtered = (rs_divs_filtered.head >> elementList("a"))
      .filter(element => element.hasAttr("href"))
    if(rs_spans_filtered.length != 1){
      logger.error("Could not scrape recipe submitter (subdiv submitter a)")
      return None
    }
    val author_url = sanitizeHTMLString(rs_spans_filtered.head.attr("href"))
    val subm_img = (rs_spans_filtered.head >> elementList("img.img-profile"))
    if(subm_img.length != 1){
      logger.error("Could not scrape recipe submitter (subdiv submitter img)")
      return None
    }
    val author_name = sanitizeHTMLString(subm_img.head.attr("title"))
    val author = new Author
    //author.name_=(author_name)
    author.url_=(author_url)
    result.author_=(author)

    rs_divs_filtered = rs_divs
      .filter(element => if(element.hasAttr("itemprop")) element.attr("itemprop").contains("description") else false)
    if(rs_divs_filtered.length != 1){
      logger.error("Could not scrape recipe submitter (subdiv desc)")
      return None
    }
    val description = sanitizeHTMLString(rs_divs_filtered.head.innerHtml)
    val recipe_description = description.substring(1, description.length - 1)
    result.description_=(recipe_description)

    //Ingredients
    sections_filtered = sections
      .filter(element => if(element.hasAttr("class")) element.attr("class").contains("recipe-ingredients") else false)
    if(sections_filtered.length != 1){
      logger.error("Could not scrape recipe ingredients (section)")
      return None
    }
    val ingredient_section = sections_filtered.head
    val ingredients_meta = (ingredient_section >> elementList("meta"))
      .filter(element => if(element.hasAttr("itemprop")) element.attr("itemprop").contains("recipeYield") else false)
    if(ingredients_meta.length != 1){
      logger.error("Could not scrape recipe ingredients (servings)")
      return None
    }
    val recipe_servings = sanitizeHTMLString(ingredients_meta.head.attr("content"))
    result.servings_=(recipe_servings.toInt)

    var nutrition_sections = (ingredient_section >> elementList("section.recipe-nutrition"))
    if(nutrition_sections.length != 1){
      logger.error("Could not scrape recipe ingredients (section)")
      return None
    }
    val nutrition_section = nutrition_sections.head
    val nutrients = (nutrition_section >> elementList("ul.nutrientLine"))
    if(nutrients.length == 0){
      logger.error("Could not scrape recipe ingredients (nutrition)")
      return None
    }
    var recipe_nutrition : Map[String, (String, String)] = Map()
    nutrients.foreach(nutrient => {
      val ret = scrapeNutrient(nutrient)
      if(ret.isEmpty){
        logger.error("Could not scrape recipe ingredients (nutrition info) properly")
      }
      else{
        recipe_nutrition += ret.get
      }
    })
    //result.nutritionInfo_=(recipe_nutrition)

    //Ingredients list
    var continue = true
    var i = 1
    var recipe_ingredients : Seq[String] = Seq()
    while(continue){
      val ingredient_list = (ingredient_section >> elementList(s"ul.list-ingredients-${i}"))
      continue = ingredient_list.length == 1
      if(!continue){
        logger.warn(s"Could not scrape recipe ingredients (list1 - page ${i}) (1)")
      }
      else{
        val res = scrapeIngredientList(ingredient_list.head)
        if(res.isDefined){
          recipe_ingredients ++= res.get
        }
        else{
          logger.error(s"Could not scrape recipe ingredients (list1 - page ${i}) (2)")
        }
      }
      i += 1
    }
    //result.ingredients_=(recipe_ingredients)

    //Directions
    sections_filtered = sections
      .filter(element => if(element.hasAttr("class")) element.attr("class").contains("recipe-directions") else false)
    if(sections_filtered.length != 1){
      logger.error("Could not scrape recipe directions (section)")
    }
    val direction_sections = sections_filtered.head >> elementList("div.directions--section__steps")
    if(direction_sections.length != 1){
      logger.error("Could not scrape recipe directions (div)")
    }
    val direction_section = direction_sections.head
    val prep_sections = direction_section >> elementList("ul.prepTime")
    if(direction_sections.length != 1){
      logger.error("Could not scrape recipe directions (ul prepTime)")
    }
    val time_list = (prep_sections.head >> elementList("li.prepTime__item"))
    if(time_list.length == 0){
      logger.error("Could not scrape recipe directions (ul prepTime item)")
    }
    var recipe_prepTime = -1
    var recipe_cookTime = -1
    var recipe_totalTime = -1
    time_list.foreach(element => {
      val timeItems = (element >> elementList("time"))
        .filter(element => element.hasAttr("itemprop"))
      if(timeItems.length == 1){
        val prepItem = timeItems.head
        val prepType = prepItem.attr("itemprop")
        if(prepType.contains("prepTime")){
          recipe_prepTime = parseTime(prepItem.attr("datetime"))
        }
        else if(prepType.contains("cookTime")){
          recipe_cookTime = parseTime(prepItem.attr("datetime"))
        }
        else if(prepType.contains("totalTime")){
          recipe_totalTime = parseTime(prepItem.attr("datetime"))
        }
      }
    })
    result.prepTime_=(recipe_prepTime)
    result.cookTime_=(recipe_cookTime)
    result.totalTime_=(recipe_totalTime)

    val step_sections = (direction_section >> elementList("ol"))
      .filter(element => if(element.hasAttr("itemprop")) element.attr("itemprop").contains("recipeInstructions") else false)
    if(step_sections.length != 1){
      logger.error("Could not scrape recipe directions (ol steps)")
    }
    var recipe_steps : Seq[(String, String)] = Seq()
    val step_list = (step_sections.head >> elementList("li.step"))
    if(step_list.length == 0){
      logger.error("Could not scrape recipe directions (li steps item)")
    }
    i = 0
    step_list.foreach(step => {
      val step_number : String =
        if(step.hasAttr("ng-click")) {
          val str = sanitizeHTMLString(step.attr("ng-click"))
          s"${str.charAt(str.length - 1)}"
        }
      else{
          s"$i"
        }
      val step_spans = step >> elementList("span.recipe-directions__list--item")
      if(step_spans.length == 1){
        val step_text = sanitizeHTMLString(step_spans.head.innerHtml)
        recipe_steps :+= (step_number, step_text)
      }
      else{
        logger.error("Could not scrape recipe step")
      }
      i += 1
    })
    //result.steps_=(recipe_steps)
    Some(result)
  }
  def scrapeNutrient(nutrient : Element): Option[(String, (String, String))] ={
    val elements = (nutrient >> elementList("li"))
      .filter(element => if(element.hasAttr("class")) element.attr("class").contains("nutrientLine__item") else false)
    if(elements.isEmpty) return None
    else{
      var name = ""
      var value = ""
      var percent = ""
      elements.foreach(element => {
        if(element.attr("class").equals("nutrientLine__item")){
          name = sanitizeHTMLString(element.innerHtml.trim.substring(0, element.innerHtml.length - 1))
        }
        else if(element.attr("class").contains("amount")){
          if(element.hasAttr("itemprop")) name = element.attr("itemprop")
          val lst = element >> elementList("span")
          if(lst.isEmpty){
            logger.error("Could not scrape recipe ingredients (nutrition info > amount) properly")
          }
          else{
            value = sanitizeHTMLString(lst.head.innerHtml)
          }
        }
        else if(element.attr("class").contains("percent")){
          percent = sanitizeHTMLString(element.innerHtml.trim.substring(0, element.innerHtml.length - 1))
        }
      })
      Some((name, (value, percent)))
    }
  }
  def scrapeIngredientList(ingredients : Element): Option[Seq[String]] = {
    val list = (ingredients >> elementList("li.checkList__line"))
    if(list.isEmpty) None
    var result : Seq[String] = Seq()
    list.foreach(ingredient =>
    {
      val spans = (ingredient >> elementList("span"))
        .filter(element => if(element.hasAttr("itemprop")) element.attr("itemprop").equals("ingredients") else false)
      if(spans.length != 1){
        logger.warn("Could not scrape recipe ingredients (ingredient info > text) properly")
      }
      else{
        result :+= sanitizeHTMLString(spans.head.innerHtml)
      }
    })
    Some(result)
  }
  def sanitizeHTMLString(string : String) : String = {
    StringEscapeUtils.unescapeHtml4(string).trim
  }
  def parseTime(time : String) : Int = {
    val duration : Duration = Duration.parse(time)
    (duration.getSeconds / 60).toInt
  }

  def scrapeRecipeKeyList(html : String, maxrecipes : Int) : Option[Seq[String]] = {
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val div = (doc >> elementList("div#recipe_output")).head

    var boxnumber1 = 4
    var boxnumber2 = 11
    var boxnumber = 0
    var continue = false
    val result : Seq[String] = (1 to maxrecipes).flatMap(a => {
      if(!continue){
        var boxes = div >> elementList(s"div#box${boxnumber1}")
        if(boxes.length == 0){
          boxes = div >> elementList(s"div#box${boxnumber2}")
          if(boxes.length == 0){
            Seq()
          }
          else{
            boxnumber = boxnumber2
          }
        }
        else {
          boxnumber = boxnumber1
        }
      }
      val boxes = div >> elementList(s"div#box${boxnumber}")
      if(boxes.isEmpty){
        Seq()
      }
      else{
        val box = boxes.head
        val div1 = (box >> elementList("div.box_content_browse")).head
        val div2 = if(!continue)(div1 >> elementList("div"))(2) else (div1 >> elementList("div")).head
        val table = (div2 >> elementList("table")).head
        val tbody = (table >> elementList("tbody")).head
        val tr = (tbody >> elementList("tr")).head
        val tds = (tr >> elementList("td"))
        val td = if(tds.length == 3) tds(1) else tds.head
        val a = (td >> elementList("a")).head
        val href = a.attr("href")
        boxnumber += 1
        continue = true
        //println(boxnumber)
        //println(href)
        Seq(href)
      }
    })
    Some(result)
  }

  def scrapeRecipeKeyRecipe(html : String) : Option[RecipeKeyRecipe] = {

    /*
    def scrapeSteps(steps_div : Element) : Seq[(Int, String)] = {
      var result : Seq[(Int, String)] = Seq()
      val str = steps_div.innerHtml
      var continue = true
      var prev = 0
      var curr = 0
      var sequence = 0
      while(continue){
        curr = str.indexOf("<br>", prev)
        curr match {
          case -1 => continue = false
          case _ =>
            val token = str.substring(prev, curr)
            var step = token
            if(token.contains("strong")) {
              var index = token.indexOf(">")
              index = token.indexOf(">", index + 1)
              step = token.substring(index + 1, token.length)
            }
            val str1 = step.replaceAll("\n", "").trim
            if(!str1.isEmpty){
              result :+= (sequence, str1)
            }
            prev = curr + 4
        }
        sequence += 1
      }
      result

    }
    */
    def scrapeSteps(steps_div : Element) : Seq[(Int, String)] = {
      var i = 0
      steps_div.childNodes.toSeq.flatMap(node => {
        if(node.isInstanceOf[TextNode]){
          val n = node.asInstanceOf[TextNode]
          i += 1
          Seq((i, n.content))
        }
        else{
          Seq()
        }
      })
    }
    def scrapeIngredients(ingredients_div : Element) : Seq[String] = {
      (ingredients_div >> elementList("div"))
        .filter(_.hasAttr("itemprop"))
        .flatMap(el => {
          val spans = (el >> elementList("span")).filter(_.hasAttr("itemprop"))
          var text = spans.filter(_.attr("itemprop") == "amount").head.text
          text += " " + spans.filter(_.attr("itemprop") == "name").head.text
          Seq(text.trim)
        })
    }

    val result : RecipeKeyRecipe = new RecipeKeyRecipe
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val h1 = (doc >> elementList("h1")).head
    val title = sanitizeHTMLString(h1.text)
    result.name_=(title)
    val ingredients_div = (doc >> elementList("div#ingredients")).head
    val ingredients = scrapeIngredients(ingredients_div)
    val ingredients_number = ingredients.length
    result.ingredients_=(ingredients)
    result.ingredientsnumber_=(ingredients_number)
    val div1 = (doc >> elementList("div#box_main_recipe")).head
    val div2 = (div1 >> elementList("div.box_content_browse")).head
    val div3 = (div2 >> elementList("div")).head
    val table = (div3 >> elementList("table")).head
    val tbody = (table >> elementList("tbody")).head
    val tr = (tbody >> elementList("tr")).head
    val td = (tr >> elementList("td")).head
    val divs = (td >> elementList("div"))
      .filter(element => if(element.hasAttr("style")) element.attr("style") == "padding:5px; font-size:12px; color:#666666; font-weight:bold; padding-left:7px;"
      else false)
    var recipeType = ""
    var prepTime = 0
    var cookTime = 0
    var totalTime = 0
    var method = ""
    var difficulty = ""
    var cuisine = ""
    divs.foreach(div => {
      val spans = (div >> elementList("span"))
      val times = (div >> elementList("time"))
      if(!spans.isEmpty){
        val span = spans.head
        if(span.hasAttr("itemprop") && span.attr("itemprop") == "recipeType"){
          if(!span.text.trim.isEmpty){
            val a = (span >> elementList("a")).head
            recipeType = a.text
          }
        }
      }
      else if(!times.isEmpty){
        val time = times.head
        if(time.hasAttr("itemprop")){
          if(time.attr("itemprop") == "prepTime")
            prepTime = parseTime(time.attr("datetime"))
          else if(time.attr("itemprop") == "cookTime")
            cookTime = parseTime(time.attr("datetime"))
          else
            totalTime = parseTime(time.attr("datetime"))
        }
      }
      else{
        val a = (div >> elementList("a")).head
        if(div.text.contains("Cook Method")){
          method = a.text
        }
        else if(div.text.contains("Difficulty")){
          difficulty = a.text
        }
        else if(div.text.contains("Cuisine")){
          cuisine = a.text
        }
      }
    })
    result.prepTime_=(prepTime)
    result.cookTime_=(cookTime)
    result.totalTime_=(totalTime)
    result.mealtype_=(recipeType)
    result.method_=(method)
    result.cuisine_=(cuisine)
    result.difficulty_=(difficulty)

    val steps_div = (doc >> elementList("div#directions")).head
    //val steps : List[Element] = (steps_div >> elementList("strong"))
    //val steps_number = steps.length


    val recipe_steps : Seq[(Int, String)] = scrapeSteps(steps_div)
    //val recipe_steps = scrapeSteps(steps_div)
    result.steps_=(recipe_steps)
    result.stepsnumber_=(recipe_steps.length)
    Some(result)
  }

  def scrapeCookipediaURLs(html : String, base_url : String, maxrecipes : Int) : Seq[String] = {
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val main_div = (doc >> element("div#mw-pages"))
    val div1 = main_div >> element("div.mw-category")
    var result : Seq[String] = Seq()
    val groups = div1 >> elementList("div.mw-category-group")
    val it1 = groups.iterator
    var continue = true
    var nrecipes = 0
    while(continue){
      if(it1.hasNext){
        val ul = it1.next() >> element("ul")
        val list = ul >> elementList("li")
        val it2 = list.iterator
        while(nrecipes < maxrecipes && it2.hasNext){
          result :+= s"${base_url}${(it2.next() >> element("a")).attr("href")}"
          continue = nrecipes < maxrecipes
          nrecipes += 1
        }
      }
      else continue = false
    }
    result
  }

  def scrapeCookipediaRecipe(html : String, delimiter : Char) : Option[CookipediaRecipe] = {
    def scrapeTime(text : String) : Option[Int] = {
      val tokenizer = new StringTokenizer(text, " ")
      var result = 0
      while(tokenizer.hasMoreElements){
        val token1 = tokenizer.nextToken()
        if(!token1.contains("None")) {
          val token = token1.trim.replaceAll("[^\\d]", "")
          if(!token.isEmpty && tokenizer.hasMoreTokens){
            var number = token.toInt
            var unit = tokenizer.nextToken().trim
            if (unit.contains("day")) {
              number *= 24
              unit = "hour"
            }
            if (unit.contains("hour")) {
              number *= 60
            }
            result += number
          }
          else{
            logger.debug("Founded time not 'None' and empty")
          }
        }
      }
      logger.debug(s"Time scraped: ${result}")
      Some(result)
    }

    def scrapeSteps(div : Element) : Seq[(Int, String)] = {
      var number = 0
      (div >> elementList("span.recipeInstructions")).flatMap(el => {
        number += 1
        val span = el >> element("span")
        Seq((number, el.text))
      })
    }

    def scrapeIngredients(div : Element) : Seq[String] = {
      (div >> elementList("span.recipeIngredient")).flatMap(el => {
        val span = el >> element("span")
        Seq(el.text)
      })
    }

    val result : CookipediaRecipe = new CookipediaRecipe()
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val name = (doc >> element("h1")).innerHtml.replace(delimiter, ' ')
    val div = doc >> element("div#mw-content-text")
    val ingredients = scrapeIngredients(div)
    val ingredients_number = ingredients.length
    val steps = scrapeSteps(div)
    val steps_number = steps.length
    val table = doc >> element("table.wikitable")
    val body = table >> element("tbody")
    val trs = (body >> elementList("tr"))
    var prep_time = -1
    var cook_time = -1
    var total_time = -1
    trs.foreach(tr => {
      val tds = tr >> elementList("td")
      var found = false
      var timeType = ""
      tds.foreach(td => {
        if(found){
          val value = scrapeTime(td.innerHtml)
          if(value.isEmpty){
            logger.error("Could not scrap recipe")
            return None
          }
          else {
            timeType match {
              case "preparation" => prep_time = value.get
              case "cooking" => cook_time = value.get
              case "total" => total_time = value.get
            }
          }
          found = false
        }
        else{
          val as = td >> elementList("a")
          as.foreach(a => {
            if (a.hasAttr("title") && a.attr("title").contains("time")) {
              val title = a.attr("title")
              if (title.contains("preparation")) {
                timeType = "preparation"
                found = true
              }
              else if (title.contains("cooking")) {
                timeType = "cooking"
                found = true
              }
              else if (title.contains("total")) {
                timeType = "total"
                found = true
              }
            }
          })
        }
      })
    })
    result.name_=(name)
    result.ingredients_=(ingredients)
    result.steps_=(steps)
    result.prepTime_=(prep_time)
    result.cookTime_=(cook_time)
    result.totalTime_=(total_time)
    result.ingredientsnumber_=(ingredients_number)
    result.stepsnumber_=(steps_number)
    Some(result)
  }

  def scrapeSaveurURLs(html : File, base_url : String, maxrecipes : Int) : Seq[String] = {
    var result : Seq[String] = Seq()
    val browser = JsoupBrowser()
    val doc = browser.parseFile(html)
    val div = doc >> element("div.results_items_wrap")
    val elements = div >> elementList("div.result_item")
    val it = elements.iterator
    var continue = true
    var nrecipes = 0
    while(continue){
      if(it.hasNext){
        val el = it.next()
        val img = el >> element("div.result_image")
        val a = img >> element("a")
        result :+= a.attr("href")
        nrecipes += 1
        continue = nrecipes < maxrecipes
      }
      else continue = false
    }
    result
  }

  def scrapeSaveurRecipe(html : String, delimiter : Char) : Option[SaveurRecipe] = {
    val result : SaveurRecipe = new SaveurRecipe()
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val name = (doc >> element("h1")).innerHtml.replace(delimiter, ' ')
    val divs_time = doc >> elementList("div.cook-time")
    var time = 0
    if(!divs_time.isEmpty) {
      val div_time = divs_time.head
      time = parseTime((div_time >> element("meta")).attr("content"))
    }
    val ingredients : Seq[String] = (doc >> elementList("div.ingredient")).map(_.text.trim.replace(delimiter, ' '))
    var i = 0
    val steps : Seq[(Int, String)] = (doc >> elementList("div.instruction")).map({el =>
      i += 1
      (i, el.text.trim.replace(delimiter, ' '))
    })
    result.name_=(name)
    result.prepTime_=(time)
    result.cookTime_=(time)
    result.totalTime_=(time)
    result.ingredients_=(ingredients)
    result.ingredientsnumber_=(ingredients.length)
    result.steps_=(steps)
    result.stepsnumber_=(steps.length)
    Some(result)
  }

  def scrapeChowhoundURLs(html : File, maxrecipes : Int) : Seq[String] = {
    var result : Seq[String] = Seq()
    val browser = JsoupBrowser()
    val doc = browser.parseFile(html)
    val main_div = doc >> element("div.fr_res")
    val elements = main_div >> elementList("div.freyja_box.freyja_box7.fr_box_rechub")
    val it = elements.iterator
    var continue = true
    var nrecipes = 0
    while(continue){
      if(it.hasNext){
        val el = it.next()
        val ls = el >> elementList("a")
        if(!ls.isEmpty){
          val a = ls.head
          result :+= a.attr("href")
          nrecipes += 1
        }
        continue = nrecipes < maxrecipes
      }
      else continue = false
    }
    result
  }

  def scrapeChowhoundRecipe(html : String, delimiter : Char) : Option[ChowhoundRecipe] = {
    def scrapeTime(str : String) : Int = {
      def isAllDigits(x: String) = x forall Character.isDigit
      val tokenizer = new StringTokenizer(str, " ")
      var continue = true
      var time = 0
      do{
        val token = tokenizer.nextToken()
        if(isAllDigits(token)){
          if(tokenizer.hasMoreTokens){
            val next = tokenizer.nextToken().toLowerCase
            if(next.contains("hr")){
              time += token.toInt * 60
            }
            else if(next.contains("min")){
              time += token.toInt
            }
          }
          else{
            time += token.toInt
          }
        }
        continue = tokenizer.hasMoreTokens
      }while(continue)
      time
    }
    val result : ChowhoundRecipe = new ChowhoundRecipe()
    val browser = JsoupBrowser()
    val doc = browser.parseString(html)
    val name = (doc >> element("h1")).innerHtml.replace(delimiter, ' ')
    val spans_time = doc >> elementList("span.frr_totaltime")
    var total_time = 0
    var cook_time = 0
    if(!spans_time.isEmpty){
      parseTime((spans_time.head >> element("time")).attr("content"))
      if(spans_time.length > 1){
        val active_time = spans_time.tail.head >> element("time")
        cook_time = scrapeTime(active_time.text)
      }
      else{
        cook_time = total_time
      }
    }
    val prep_time = cook_time.min(total_time)
    val ingredients : Seq[String] = (doc >> elementList("li[itemprop=ingredients]")).map(_.text.trim.replace(delimiter, ' '))
    var i = 0
    val steps_div = doc >> element("div[itemprop=recipeInstructions]")
    val steps : Seq[(Int, String)] = (steps_div >> elementList("li")).map({el =>
      i += 1
      (i, el.text.trim.replace(delimiter, ' '))
    })
    result.name_=(name)
    result.prepTime_=(prep_time)
    result.cookTime_=(cook_time)
    result.totalTime_=(total_time)
    result.ingredients_=(ingredients)
    result.ingredientsnumber_=(ingredients.length)
    result.steps_=(steps)
    result.stepsnumber_=(steps.length)
    Some(result)
  }

  def scrapeChowhoundRecipe(html : File, delimiter : Char) : Option[ChowhoundRecipe] = {
    def scrapeTime(str : String) : Int = {
      def isAllDigits(x: String) = x forall Character.isDigit
      val tokenizer = new StringTokenizer(str, " ")
      var continue = true
      var time = 0
      do{
        val token = tokenizer.nextToken()
        if(isAllDigits(token)){
          val next = tokenizer.nextToken().toLowerCase
          if(next.contains("hr")){
            time += token.toInt * 60
          }
          else if(next.contains("min")){
            time += token.toInt
          }
        }
        continue = tokenizer.hasMoreTokens
      }while(continue)
      time
    }
    val result : ChowhoundRecipe = new ChowhoundRecipe()
    val browser = JsoupBrowser()
    val doc = browser.parseFile(html)
    val name = (doc >> element("h1")).innerHtml.replace(delimiter, ' ')
    val spans_time = doc >> elementList("span.frr_totaltime")
    val total_time = parseTime((spans_time.head >> element("time")).attr("content"))
    var cook_time = 0
    if(spans_time.length > 1){
      val active_time = spans_time.tail.head >> element("time")
      cook_time = scrapeTime(active_time.text)
    }
    else{
      cook_time = total_time
    }
    val prep_time = cook_time.min(total_time)
    val ingredients : Seq[String] = (doc >> elementList("li[itemprop=ingredients]")).map(_.text.trim.replace(delimiter, ' '))
    var i = 0
    val steps_div = doc >> element("div[itemprop=recipeInstructions]")
    val steps : Seq[(Int, String)] = (steps_div >> elementList("li")).map({el =>
      i += 1
      (i, el.text.trim.replace(delimiter, ' '))
    })
    result.name_=(name)
    result.prepTime_=(prep_time)
    result.cookTime_=(cook_time)
    result.totalTime_=(total_time)
    result.ingredients_=(ingredients)
    result.ingredientsnumber_=(ingredients.length)
    result.steps_=(steps)
    result.stepsnumber_=(steps.length)
    Some(result)
  }
}
