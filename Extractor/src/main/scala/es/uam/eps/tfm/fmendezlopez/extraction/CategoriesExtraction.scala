package es.uam.eps.tfm.fmendezlopez.extraction

/**
  * Created by Francisco on 02/04/2017.
  */
import java.io._
import java.net.SocketTimeoutException
import java.util.Properties

import com.github.tototoshi.csv.{CSVWriter, DefaultCSVFormat}
import es.uam.eps.tfm.fmendezlopez.utils._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import org.apache.commons.configuration2.builder.FileBasedConfigurationBuilder
import org.apache.commons.configuration2.builder.fluent.Parameters
import org.apache.commons.configuration2.{Configuration, FileBasedConfiguration, PropertiesConfiguration}
import org.apache.http.HttpResponse
import org.apache.http.client.ClientProtocolException
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.HttpGet
import org.apache.http.conn.ConnectTimeoutException
import org.apache.http.impl.client.{CloseableHttpClient, DefaultHttpClient, HttpClientBuilder}
import org.apache.http.util.EntityUtils
import org.json.{JSONArray, JSONObject}
import org.json4s._
import org.json4s.native.JsonMethods._

import scala.collection.mutable

object CategoriesExtraction extends Logging{

  private var properties : Configuration = _
  private var baseURL : String = _
  private var connectionProperties : Properties = _
  private var categoriesVisited : mutable.Set[Int] = _

  private var csvCategories : CSVWriter = _
  private var csvCategoriesRel : CSVWriter = _

  private var ncategories : Int = 0

  def main(args: Array[String]): Unit = {
    if(args.length != 1) printHelp
    else{
      properties = PropertiesManager.loadProperties(args(0), PropertiesManager.EXTRACTION_PROPERTIES_FILE)
      baseURL = properties.getString("allrecipes.url.base")
      categoriesVisited = mutable.Set()

      val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
      val outputDir = Utils.resolvePath(0, hostname)

      val allCategoriesURL = properties.getString("allrecipes.url.categories")
      val csvCategoriesName = s"${properties.getProperty("stage0.output.csv.categories.filename")}"
      val csvCategoriesRelName = s"${properties.getProperty("stage0.output.csv.categories.relationship.filename")}"
      val delimiter:Char = properties.getProperty("stage0.output.csv.delimiter").toString.charAt(0)

      connectionProperties = new Properties()
      connectionProperties.put("timeout", properties.getProperty("stage0.scraping.timeout"))
      connectionProperties.put("delay-auth", properties.getProperty("stage0.scraping.delay.auth"))
      connectionProperties.put("max-attempts", properties.getProperty("stage0.scraping.max-attempts"))
      connectionProperties.put("delay-detection", properties.getProperty("stage0.scraping.delay.detection"))
      connectionProperties.put("cookie-token", properties.getProperty("general.scraping.cookiename.artoken"))
      connectionProperties.put("referrer", allCategoriesURL)
      connectionProperties.put("max-body-size", properties.getProperty("stage0.scraping.maxBodySize"))
      connectionProperties.put("host", properties.getProperty("allrecipes.host"))
      connectionProperties.put("delay-category", properties.getProperty("stage0.scraping.delay.category"))
      connectionProperties.put("delay-categories", properties.getProperty("stage0.scraping.delay.categories"))
      connectionProperties.put("delay-n", properties.getProperty("stage0.scraping.delay.n"))

      HttpManager.setProperties(properties)
      HttpManager.setConnectionProperties(connectionProperties)
      val res = HttpManager.requestAuthToken()
      if(res.isEmpty){
        logger.fatal("Cannot retrieve auth token\nFinishing...")
        System.exit(1)
      }
      connectionProperties.put("auth-token", res.get)
      HttpManager.setConnectionProperties(connectionProperties)

      csvCategories = CSVManager.openCSVWriter(outputDir, csvCategoriesName, delimiter)
      csvCategoriesRel = CSVManager.openCSVWriter(outputDir, csvCategoriesRelName, delimiter)
      csvCategories.writeRow(Seq("ID", "NAME", "URL", "COUNT"))
      csvCategoriesRel.writeRow(Seq("ID_PARENT", "ID_CHILD"))

      val start = System.currentTimeMillis()
      val mainCategories = Extractor.extractCategories(allCategoriesURL, connectionProperties).getOrElse(new JSONArray)
      val mainJSON = processGroups(mainCategories, -1, "", 0, 0, allCategoriesURL)
      val end = System.currentTimeMillis()
      logger.info(s"Elapsed time: ${end - start}ms")
      println(mainJSON.toString())
      CSVManager.closeCSVWriter(csvCategories)
      CSVManager.closeCSVWriter(csvCategoriesRel)
    }
  }

  def printHelp : Unit = {
    println("Wrong parameters: try CategoriesExtractor <configuration_path>")
  }

  def processGroups(groups:JSONArray, id:Int, hierarchy:String, currDepth:Int, idParent:Int, cat_url : String) : Unit = {
    if(groups.length() == 0){
      return
    }
    else{
      var i = 0
      for(i <- 0 until groups.length()){
        val group = groups.getJSONObject(i)
        val id = group.getInt("Id")
        val title = group.getString("Title")
        val name = s"${id}_${title}"

        val categories = group.getJSONArray("ChildHubs")
        if(categoriesVisited.contains(id)){
          logger.warn(s"Related categories already processed for ${id}")
        }
        else{
          val newHierarchy = s"${hierarchy}${name}>"
          logger.info(s"Hierarchy: ${newHierarchy}")
          logger.info(s"Depth: ${currDepth}")
          logger.info(s"Processing group: ${name}")
          processCategories(categories, newHierarchy, currDepth, idParent, cat_url)
          logger.info(s"Processed group: ${name}")
        }
      }
    }
  }
  def processCategories(categories:JSONArray, hierarchy:String, currDepth:Int, idParent:Int, cat_url : String) : Unit = {
    if(categories.length() == 0){
      return
    }
    else{
      for(i <- 0 until categories.length()){
        val category = categories.getJSONObject(i)
        val id = category.getInt("Id")
        val title = category.getString("Title")
        val name = s"${id}_${title}"
        val url = s"${baseURL}${category.getString("Path")}"
        val count = category.getInt("TotalCount")
        val path = category.getString("Path")
        val newHierarchy = s"${hierarchy}${name}>"
        if(categoriesVisited.contains(id)){
          logger.warn(s"Related categories already processed for ${id}")
        }
        else{
          csvCategories.writeRow(Seq(id, title, url, count))
          csvCategoriesRel.writeRow(Seq(idParent, id))
          logger.info(s"Processing category: ${name}")
          val curl = s"${baseURL}${path}"
          connectionProperties.replace("referrer", cat_url)
          val subCategories = Extractor.extractCategories(curl, connectionProperties).getOrElse(new JSONArray)
          ncategories += 1
          if(ncategories % connectionProperties.getProperty("delay-n").toInt == 0){
            logger.info(s"Requested ${ncategories} categories. Sleeping...")
            Thread.sleep(connectionProperties.getProperty("delay-categories").toLong)
          }
          else{
            Thread.sleep(connectionProperties.getProperty("delay-category").toLong)
          }
          categoriesVisited += id
          processGroups(subCategories, id, newHierarchy, currDepth + 1, id, curl)
          logger.info(s"Processed category: ${id}_${title}")
        }
        /*
        category.put("ChildHubs", subArray)
        category.put("Processed", false)
        category.put("Current page", -1)
        category.put("Last URL", JSONObject.NULL)
        category.put("Page completed", false)
        category.put("Hierarchy", newHierarchy)
        */
      }
    }
  }

  def beforeExit = {
    CSVManager.closeCSVWriter(csvCategories)
    CSVManager.closeCSVWriter(csvCategoriesRel)
  }

  def prueba1 : Unit = {
    val get = new HttpGet("http://allrecipes.com/recipes/?grouping=all");
    val client = HttpClientBuilder.create().build();
    try {
      val response = client.execute(get);
      val json = EntityUtils.toString(response.getEntity(), "UTF-8");
      val browser = JsoupBrowser()
      val str1 = "[{\"Title\":\"Most Popular\",\"Id\":0,\"BreadcrumbName\":\"Most Popular\",\"Description\":\"\",\"ChildHubs\":[{\"Id\":1642,\"BreadCrumbName\":\"Everyday Cooking\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":36833,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Everyday Cooking\",\"Description\":\"Speedy weeknight dinners, 5-ingredient dishes, quick and easy meals, plus kid-pleasing snacks and desserts\",\"Path\":\"/recipes/1642/everyday-cooking/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/58655.png\",\"UrlSafeTitle\":\"everyday-cooking\",\"SingularizedHubTitle\":\"Everyday Cooking Recipes\",\"ImageMap\":null},{\"Id\":1116,\"BreadCrumbName\":\"Fruits and Vegetables\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":30391,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Fruits, Vegetables and Other Produce\",\"Description\":\"Find recipes for all kinds of fruits and vegetables, including broccoli, apples, strawberries, zucchini... we have it all! Mushrooms, beans and peas, too.\",\"Path\":\"/recipes/1116/fruits-and-vegetables/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56586.png\",\"UrlSafeTitle\":\"fruits-and-vegetables\",\"SingularizedHubTitle\":\"Fruits, Vegetables and Other Produce Recipes\",\"ImageMap\":null},{\"Id\":80,\"BreadCrumbName\":\"Main Dish\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":15695,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Main Dishes\",\"Description\":\"Hundreds of main dish recipes. Choose from top-rated comfort food, healthy, and vegetarian options. Find your dinner star now!\",\"Path\":\"/recipes/80/main-dish/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56564.png\",\"UrlSafeTitle\":\"main-dish\",\"SingularizedHubTitle\":\"Main Dish Recipes\",\"ImageMap\":null},{\"Id\":92,\"BreadCrumbName\":\"Meat and Poultry\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":15644,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Meat and Poultry\",\"Description\":\"Top recipes for beef, chicken, pork, and more. See classic recipes or find something new.\",\"Path\":\"/recipes/92/meat-and-poultry/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56568.png\",\"UrlSafeTitle\":\"meat-and-poultry\",\"SingularizedHubTitle\":\"Meat and Poultry Recipes\",\"ImageMap\":null},{\"Id\":79,\"BreadCrumbName\":\"Desserts\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":13295,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Desserts\",\"Description\":\"Whether you crave sweet, savory, decadent or healthy, we have hundreds of top-rated dessert recipes to satisfy your taste buds.\",\"Path\":\"/recipes/79/desserts/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56562.png\",\"UrlSafeTitle\":\"desserts\",\"SingularizedHubTitle\":\"Dessert Recipes\",\"ImageMap\":null},{\"Id\":85,\"BreadCrumbName\":\"Holidays and Events\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":12844,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Holidays and Events\",\"Description\":\"Celebrate with top-rated holiday cookies, appetizers, and desserts. Find the right recipe for any holiday.\",\"Path\":\"/recipes/85/holidays-and-events/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/58447.png\",\"UrlSafeTitle\":\"holidays-and-events\",\"SingularizedHubTitle\":\"Holidays and Events Recipes\",\"ImageMap\":null}]},{\"Title\":\"Courses\",\"Id\":1,\"BreadcrumbName\":\"\",\"Description\":\"\",\"ChildHubs\":[{\"Id\":76,\"BreadCrumbName\":\"Appetizers and Snacks\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":5000,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Appetizers and Snacks\",\"Description\":\"Perfect party appetizers the easy way. See hundreds of tasty appetizers with photos and tips on how to make them.\",\"Path\":\"/recipes/76/appetizers-and-snacks/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56559.png\",\"UrlSafeTitle\":\"appetizers-and-snacks\",\"SingularizedHubTitle\":\"Appetizers and Snack Recipes\",\"ImageMap\":null},{\"Id\":156,\"BreadCrumbName\":\"Bread\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":3475,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Bread Recipes\",\"Description\":\"See how to bake bread at home. Recipes for white, wheat, and more with photos, video, and tips to help you make them. Bread machine versions, too!\",\"Path\":\"/recipes/156/bread/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56560.png\",\"UrlSafeTitle\":\"bread\",\"SingularizedHubTitle\":\"Bread Recipes\",\"ImageMap\":null},{\"Id\":79,\"BreadCrumbName\":\"Desserts\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":13295,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Desserts\",\"Description\":\"Whether you crave sweet, savory, decadent or healthy, we have hundreds of top-rated dessert recipes to satisfy your taste buds.\",\"Path\":\"/recipes/79/desserts/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56562.png\",\"UrlSafeTitle\":\"desserts\",\"SingularizedHubTitle\":\"Dessert Recipes\",\"ImageMap\":null},{\"Id\":77,\"BreadCrumbName\":\"Drinks\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":3073,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Drinks\",\"Description\":\"From cocktails to punch for kids, find the perfect party drink. Plus videos, photos, and reviews to help you mix drinks right.\",\"Path\":\"/recipes/77/drinks/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56563.png\",\"UrlSafeTitle\":\"drinks\",\"SingularizedHubTitle\":\"Drinks Recipes\",\"ImageMap\":null},{\"Id\":80,\"BreadCrumbName\":\"Main Dish\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":15695,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Main Dishes\",\"Description\":\"Hundreds of main dish recipes. Choose from top-rated comfort food, healthy, and vegetarian options. Find your dinner star now!\",\"Path\":\"/recipes/80/main-dish/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56564.png\",\"UrlSafeTitle\":\"main-dish\",\"SingularizedHubTitle\":\"Main Dish Recipes\",\"ImageMap\":null},{\"Id\":96,\"BreadCrumbName\":\"Salad\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":3429,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Salad Recipes\",\"Description\":\"Find the best green salad recipes, plus trusted recipes for more than 3,420 other dinner and picnic salads.\",\"Path\":\"/recipes/96/salad/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56565.png\",\"UrlSafeTitle\":\"salad\",\"SingularizedHubTitle\":\"Salad Recipes\",\"ImageMap\":null},{\"Id\":81,\"BreadCrumbName\":\"Side Dish\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":4610,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Side Dishes\",\"Description\":\"The best dinner side dishes. Find vegetable sides, BBQ sides, and the perfect sides for chicken dinner. Hundreds of side dish recipes with photos and reviews.\",\"Path\":\"/recipes/81/side-dish/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56566.png\",\"UrlSafeTitle\":\"side-dish\",\"SingularizedHubTitle\":\"Side Dish Recipes\",\"ImageMap\":null},{\"Id\":94,\"BreadCrumbName\":\"Soups, Stews and Chili\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":4172,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Soups, Stews and Chili\",\"Description\":\"Find recipes for hearty favorites like chicken tortilla soup, beef stew, white chicken chili, and more.\",\"Path\":\"/recipes/94/soups-stews-and-chili/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56567.png\",\"UrlSafeTitle\":\"soups-stews-and-chili\",\"SingularizedHubTitle\":\"Soups, Stews and Chili Recipes\",\"ImageMap\":null}]},{\"Title\":\"Meals\",\"Id\":2,\"BreadcrumbName\":\"\",\"Description\":\"\",\"ChildHubs\":[{\"Id\":78,\"BreadCrumbName\":\"Breakfast and Brunch\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":2767,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Breakfast and Brunch\",\"Description\":\"Eggs, omelets, pancakes, breakfast casseroles, and more! See hundreds of trusted breakfast and brunch recipes with reviews and tips from home cooks.\",\"Path\":\"/recipes/78/breakfast-and-brunch/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56561.png\",\"UrlSafeTitle\":\"breakfast-and-brunch\",\"SingularizedHubTitle\":\"Breakfast and Brunch Recipes\",\"ImageMap\":null},{\"Id\":17561,\"BreadCrumbName\":\"Lunch\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":796,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Lunch Recipes\",\"Description\":\"Looking for lunch recipes? Allrecipes has more than 790 trusted lunch ideas whether you\\u0027re planning ahead or looking for something last minute.\",\"Path\":\"/recipes/17561/lunch/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/58652.png\",\"UrlSafeTitle\":\"lunch\",\"SingularizedHubTitle\":\"Lunch Recipes\",\"ImageMap\":null},{\"Id\":17562,\"BreadCrumbName\":\"Dinner\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":1127,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Dinner Recipes\",\"Description\":\"Allrecipes has hundreds of dinner ideas to help you get an easy and healthy dinner on the table fast.\",\"Path\":\"/recipes/17562/dinner/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/58654.png\",\"UrlSafeTitle\":\"dinner\",\"SingularizedHubTitle\":\"Dinner Recipes\",\"ImageMap\":null}]},{\"Title\":\"Main Ingredients\",\"Id\":3,\"BreadcrumbName\":\"\",\"Description\":\"\",\"ChildHubs\":[{\"Id\":1116,\"BreadCrumbName\":\"Fruits and Vegetables\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":30391,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Fruits, Vegetables and Other Produce\",\"Description\":\"Find recipes for all kinds of fruits and vegetables, including broccoli, apples, strawberries, zucchini... we have it all! Mushrooms, beans and peas, too.\",\"Path\":\"/recipes/1116/fruits-and-vegetables/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56586.png\",\"UrlSafeTitle\":\"fruits-and-vegetables\",\"SingularizedHubTitle\":\"Fruits, Vegetables and Other Produce Recipes\",\"ImageMap\":null},{\"Id\":92,\"BreadCrumbName\":\"Meat and Poultry\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":15644,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Meat and Poultry\",\"Description\":\"Top recipes for beef, chicken, pork, and more. See classic recipes or find something new.\",\"Path\":\"/recipes/92/meat-and-poultry/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56568.png\",\"UrlSafeTitle\":\"meat-and-poultry\",\"SingularizedHubTitle\":\"Meat and Poultry Recipes\",\"ImageMap\":null},{\"Id\":95,\"BreadCrumbName\":\"Pasta and Noodles\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":3042,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Pasta and Noodles\",\"Description\":\"Find recipes for all your favorite pasta dishes including lasagna, baked ziti, pasta salad, macaroni and cheese, and pesto.\",\"Path\":\"/recipes/95/pasta-and-noodles/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56589.png\",\"UrlSafeTitle\":\"pasta-and-noodles\",\"SingularizedHubTitle\":\"Pasta and Noodle Recipes\",\"ImageMap\":null},{\"Id\":93,\"BreadCrumbName\":\"Seafood\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":3694,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Seafood Recipes\",\"Description\":\"Top recipes for fish, shellfish, and hearty chowder. See easy ways to make seafood part of your low-cal diet.\",\"Path\":\"/recipes/93/seafood/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56591.png\",\"UrlSafeTitle\":\"seafood\",\"SingularizedHubTitle\":\"Seafood Recipes\",\"ImageMap\":null}]},{\"Title\":\"Occasions and Cooking Styles\",\"Id\":4,\"BreadcrumbName\":\"\",\"Description\":\"\",\"ChildHubs\":[{\"Id\":88,\"BreadCrumbName\":\"BBQ \\u0026 Grilling\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":1963,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"BBQ \\u0026 Grilling\",\"Description\":\"The best BBQ chicken, pork and BBQ sauces. Hundreds of barbecue and grilling recipes, with tips and tricks from home grillers.\",\"Path\":\"/recipes/88/bbq-grilling/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56594.png\",\"UrlSafeTitle\":\"bbq-grilling\",\"SingularizedHubTitle\":\"BBQ \\u0026 Grilling Recipes\",\"ImageMap\":null},{\"Id\":1642,\"BreadCrumbName\":\"Everyday Cooking\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":36833,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Everyday Cooking\",\"Description\":\"Speedy weeknight dinners, 5-ingredient dishes, quick and easy meals, plus kid-pleasing snacks and desserts\",\"Path\":\"/recipes/1642/everyday-cooking/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/58655.png\",\"UrlSafeTitle\":\"everyday-cooking\",\"SingularizedHubTitle\":\"Everyday Cooking Recipes\",\"ImageMap\":null},{\"Id\":84,\"BreadCrumbName\":\"Healthy Recipes\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":4305,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Healthy Recipes\",\"Description\":\"Find trusted recipes for eating healthy: start the day with a wholesome breakfast, cut the carbs or calories, find the perfect main dish for your special diet.\",\"Path\":\"/recipes/84/healthy-recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56597.png\",\"UrlSafeTitle\":\"healthy-recipes\",\"SingularizedHubTitle\":\"Healthy Recipes\",\"ImageMap\":null},{\"Id\":85,\"BreadCrumbName\":\"Holidays and Events\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":12844,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Holidays and Events\",\"Description\":\"Celebrate with top-rated holiday cookies, appetizers, and desserts. Find the right recipe for any holiday.\",\"Path\":\"/recipes/85/holidays-and-events/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/58447.png\",\"UrlSafeTitle\":\"holidays-and-events\",\"SingularizedHubTitle\":\"Holidays and Events Recipes\",\"ImageMap\":null},{\"Id\":17567,\"BreadCrumbName\":\"Ingredients\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":2238,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Ingredients\",\"Description\":\"Recipes organized by primary and important ingredients, like oils, flours and herbs.\",\"Path\":\"/recipes/17567/ingredients/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56569.png\",\"UrlSafeTitle\":\"ingredients\",\"SingularizedHubTitle\":\"Ingredient Recipes\",\"ImageMap\":null},{\"Id\":236,\"BreadCrumbName\":\"U.S. Recipes\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":3775,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"U.S. Recipes\",\"Description\":\"Find recipes from across the United States! Allrecipes has recipes for every region and state, including mouthwatering Southern favorites, classic fare from New England, and spicy Southwest dishes.\",\"Path\":\"/recipes/236/us-recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/58656.png\",\"UrlSafeTitle\":\"us-recipes\",\"SingularizedHubTitle\":\"U.S. Recipes\",\"ImageMap\":null},{\"Id\":86,\"BreadCrumbName\":\"World Cuisine\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":8557,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"World Cuisine\",\"Description\":\"Boldly go where your taste buds haven\\u0027t gone before with recipes from countries far and near. Your kitchen is the flight deck.\",\"Path\":\"/recipes/86/world-cuisine/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56610.png\",\"UrlSafeTitle\":\"world-cuisine\",\"SingularizedHubTitle\":\"World Cuisine Recipes\",\"ImageMap\":null},{\"Id\":82,\"BreadCrumbName\":\"Trusted Brands: Recipes and Tips\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null}],\"TotalCount\":2862,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Trusted Brands\",\"Description\":\"Browse more than 2,860 recipes provided by Allrecipes.com\\u0026#8217;s trusted brand name partners.\",\"Path\":\"/recipes/82/trusted-brands-recipes-and-tips/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/images/56617.png\",\"UrlSafeTitle\":\"trusted-brands-recipes-and-tips\",\"SingularizedHubTitle\":\"Trusted Brand Recipes\",\"ImageMap\":null}]}]"
      val str2 = "[{\"Title\":\"Subcategories\",\"Id\":0,\"BreadcrumbName\":\"\",\"Description\":\"\",\"ChildHubs\":[{\"Id\":16930,\"BreadCrumbName\":\"Beans and Peas\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null},{\"Id\":1116,\"BreadCrumbName\":\"Fruits and Vegetables\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Fruits, Vegetables and Other Produce\",\"Description\":\"\",\"Path\":\"/recipes/1116/fruits-and-vegetables/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"fruits-and-vegetables\",\"SingularizedHubTitle\":\"Fruits, Vegetables and Other Produce Recipes\",\"ImageMap\":null}],\"TotalCount\":2131,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Bean Recipes\",\"Description\":\"Add more protein to your diet with these bean recipes. Browse top-rated recipes with beans including chili, side dishes and more.\",\"Path\":\"/recipes/16930/fruits-and-vegetables/beans-and-peas/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/userphotos/140x140/00/78/68/786800.jpg\",\"UrlSafeTitle\":\"fruits-and-vegetables/beans-and-peas\",\"SingularizedHubTitle\":\"Bean Recipes\",\"ImageMap\":null},{\"Id\":1058,\"BreadCrumbName\":\"Fruits\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null},{\"Id\":1116,\"BreadCrumbName\":\"Fruits and Vegetables\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Fruits, Vegetables and Other Produce\",\"Description\":\"\",\"Path\":\"/recipes/1116/fruits-and-vegetables/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"fruits-and-vegetables\",\"SingularizedHubTitle\":\"Fruits, Vegetables and Other Produce Recipes\",\"ImageMap\":null}],\"TotalCount\":12517,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Fruit Recipes\",\"Description\":\"Looking for fruit recipes? Allrecipes has more than 12,510 trusted fruit recipes complete with ratings, reviews and serving tips.\",\"Path\":\"/recipes/1058/fruits-and-vegetables/fruits/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/userphotos/140x140/01/11/91/1119153.jpg\",\"UrlSafeTitle\":\"fruits-and-vegetables/fruits\",\"SingularizedHubTitle\":\"Fruit Recipes\",\"ImageMap\":null},{\"Id\":15172,\"BreadCrumbName\":\"Mushrooms\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null},{\"Id\":1116,\"BreadCrumbName\":\"Fruits and Vegetables\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Fruits, Vegetables and Other Produce\",\"Description\":\"\",\"Path\":\"/recipes/1116/fruits-and-vegetables/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"fruits-and-vegetables\",\"SingularizedHubTitle\":\"Fruits, Vegetables and Other Produce Recipes\",\"ImageMap\":null}],\"TotalCount\":1654,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Mushroom Recipes\",\"Description\":\"Stuffed mushrooms. Chicken and mushrooms. Mushroom soup. Mushroom risotto. Mushroom gravy. Pick your favorite mushroom recipes and make some magic.\",\"Path\":\"/recipes/15172/fruits-and-vegetables/mushrooms/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/userphotos/140x140/00/97/15/971587.jpg\",\"UrlSafeTitle\":\"fruits-and-vegetables/mushrooms\",\"SingularizedHubTitle\":\"Mushroom Recipes\",\"ImageMap\":null},{\"Id\":1059,\"BreadCrumbName\":\"Vegetables\",\"Ancestors\":[{\"Id\":1,\"BreadCrumbName\":\"Recipes\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Recipes\",\"Description\":\"\",\"Path\":\"/recipes/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"recipes\",\"SingularizedHubTitle\":\"Recipes\",\"ImageMap\":null},{\"Id\":1116,\"BreadCrumbName\":\"Fruits and Vegetables\",\"Ancestors\":[],\"TotalCount\":0,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Fruits, Vegetables and Other Produce\",\"Description\":\"\",\"Path\":\"/recipes/1116/fruits-and-vegetables/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png\",\"UrlSafeTitle\":\"fruits-and-vegetables\",\"SingularizedHubTitle\":\"Fruits, Vegetables and Other Produce Recipes\",\"ImageMap\":null}],\"TotalCount\":17934,\"VideoCount\":0,\"IsSlideshowHub\":false,\"Macros\":[],\"IsMerchControlledHub\":false,\"Title\":\"Vegetable Recipes\",\"Description\":\"Looking for vegetable recipes? Allrecipes has more than 17,930 trusted vegetable recipes complete with how-to videos, ratings, reviews, and cooking tips.\",\"Path\":\"/recipes/1059/fruits-and-vegetables/vegetables/\",\"Slug\":null,\"ImageUrl\":\"http://images.media-allrecipes.com/userphotos/140x140/00/01/41/14106.jpg\",\"UrlSafeTitle\":\"fruits-and-vegetables/vegetables\",\"SingularizedHubTitle\":\"Vegetable Recipes\",\"ImageMap\":null}]}]"
      //val doc = browser.parseString(json)
      val doc = browser.parseString(str1)
      val els: List[Element] = doc >> elementList("script")
      val exist = els.foldLeft(false)((acc, element) => acc || element.innerHtml.contains("hubCategories"))
      if(true){
        val el = els(els.length - 7).innerHtml
        println(el)
        val startIndex = el.indexOf("[")
        val mainJSONStr = el.substring(startIndex, el.length - 3)
        val mainJSONObj = new JSONArray(mainJSONStr)

        val basePath = "http://allrecipes.com"
        var i = 0
        for(i <- 0 until mainJSONObj.length()){
          val category0:JSONObject = mainJSONObj.getJSONObject(i)
          val children = category0.getJSONArray("ChildHubs")
          var j = 0
          for(j <- 0 until children.length()){
            val category1:JSONObject = children.getJSONObject(j)
            println(category1.get("id"))
          }
        }

      }
    } catch {
      case ioe: ClientProtocolException => ioe.printStackTrace() // more specific cases first !
      case e: IOException => e.printStackTrace()
    }
  }

  def prueba2 : Unit = {
    val JSONString1 = """
      {
         "name":"luca",
         "id": "1q2w3e4r5t",
         "age": 26,
         "url":"http://www.nosqlnocry.wordpress.com",
         "url":"https://nosqlnocry.wordpress.com",
         "loginTimeStamps": [1434904257,1400689856,1396629056],
         "messages": [
          {"id":1,"content":"Please like this post!"},
          {"id":2,"content":"Forza Roma!"}
         ],
         "example": [
         ]
         "profile": { "id":"my-nickname", "score":123, "avatar":"path.jpg" }
      }
      """
    var json = parse(JSONString1)

    val JSONString2 =
      """
        {
          "name":"luca",
          "id": "1q2w3e4r5t",
          "age": 26,
          "url":"http://www.nosqlnocry.wordpress.com"
        }
      """
    var arr = json\\"messages"
    var arr2 = arr.asInstanceOf[JArray]
    for (message <- arr2.children){
      //println(message\"content")
    }
    //println(s"arr2:\n${arr2}")
    val JSON2 = parse(JSONString2)
    arr = JSON2
    //println(arr)
    json = json transformField {
      case JField("example", _) => ("example", arr)
    }
    val a = JObject()
    json = json merge json
    //println(pretty(render(json)))
    //json = json merge render("height",175)
    //println(pretty(json)) // use 'pretty' or 'compact'
  }

  def prueba3 : Unit = {
    val str =
      """
        [
          {
            "Path": "A",
            "ChildHubs":
            [
              {
                "Path":"AA"
              },
              {
                "Path":"AB"
              }
            ]
          },
          {
            "Path": "B",
            "ChildHubs":
            [
              {
                "Path":"BA"
              },
              {
                "Path":"BB"
              }
            ]
          }
        ]
      """
    val mainJSONObj = new JSONArray(str)
    var i = 0
    //val fin = processCategories(mainJSONObj)
    //println(fin.toString)
  }

  def getCategories1(url:String) : JSONArray = {
    val strAA =
      """
        [
          {
            "Path": "",
            "ChildHubs":
            [
              {
                "Path":"AAA"
              },
              {
                "Path":"AAB"
              }
            ]
          }
        ]
      """
    val strAB =
      """
        [
          {
            "Path": "",
            "ChildHubs":
            [
              {
                "Path":"ABA"
              },
              {
                "Path":"ABB"
              }
            ]
          }
        ]
      """
    val strBA =
      """
        [
          {
            "Path": "",
            "ChildHubs":
            [
              {
                "Path":"BAA"
              },
              {
                "Path":"BAB"
              }
            ]
          }
        ]
      """
    val strBB =
      """
        [
          {
            "Path": "",
            "ChildHubs":
            [
              {
                "Path":"BBA"
              },
              {
                "Path":"BBB"
              }
            ]
          }
        ]
      """
    val strEmpty =
      """
        [
        ]
      """
    val str = ""
    url match
    {
      case "AA" => new JSONArray(strAA)
      case "AB" => new JSONArray(strAB)
      case "BA" => new JSONArray(strBA)
      case "BB" => new JSONArray(strBB)
      case _ => new JSONArray(strEmpty)
    }

  }
  def prueba4() : Unit = {
    val str =
      """
        [{"Title":"Related Categories","Id":0,"BreadcrumbName":"","Description":"","ChildHubs":[{"Id":16824,"BreadCrumbName":"Pear Crisps and Crumbles","Ancestors":[{"Id":1,"BreadCrumbName":"Recipes","Ancestors":[],"TotalCount":0,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":false,"Title":"Recipes","Description":"","Path":"/recipes/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png","UrlSafeTitle":"recipes","SingularizedHubTitle":"Recipes","ImageMap":null},{"Id":79,"BreadCrumbName":"Desserts","Ancestors":[],"TotalCount":0,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":false,"Title":"Desserts","Description":"","Path":"/recipes/79/desserts/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png","UrlSafeTitle":"desserts","SingularizedHubTitle":"Dessert Recipes","ImageMap":null},{"Id":15840,"BreadCrumbName":"Crisps and Crumbles","Ancestors":[],"TotalCount":0,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":false,"Title":"Crisps and Crumbles","Description":"","Path":"/recipes/15840/crisps-and-crumbles/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png","UrlSafeTitle":"crisps-and-crumbles","SingularizedHubTitle":"Crisps and Crumbles Recipes","ImageMap":null}],"TotalCount":13,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":true,"Title":"Pear Crisps and Crumbles","Description":"Looking for pear crisps, crumbles \u0026 buckle recipes? Allrecipes has more than 10 trusted pear crisps, crumbles \u0026 buckle recipes complete with ratings, reviews and baking tips.","Path":"/recipes/16824/desserts/crisps-and-crumbles/pear-crisps-and-crumbles/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/userphotos/140x140/01/11/76/1117671.jpg","UrlSafeTitle":"desserts/crisps-and-crumbles/pear-crisps-and-crumbles","SingularizedHubTitle":"Pear Crisps and Crumbles Recipes","ImageMap":null},{"Id":12951,"BreadCrumbName":"Pear Pie","Ancestors":[{"Id":1,"BreadCrumbName":"Recipes","Ancestors":[],"TotalCount":0,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":false,"Title":"Recipes","Description":"","Path":"/recipes/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png","UrlSafeTitle":"recipes","SingularizedHubTitle":"Recipes","ImageMap":null},{"Id":79,"BreadCrumbName":"Desserts","Ancestors":[],"TotalCount":0,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":false,"Title":"Desserts","Description":"","Path":"/recipes/79/desserts/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png","UrlSafeTitle":"desserts","SingularizedHubTitle":"Dessert Recipes","ImageMap":null},{"Id":367,"BreadCrumbName":"Pies","Ancestors":[],"TotalCount":0,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":false,"Title":"Pie Recipes","Description":"","Path":"/recipes/367/pies/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png","UrlSafeTitle":"pies","SingularizedHubTitle":"Pie Recipes","ImageMap":null},{"Id":808,"BreadCrumbName":"Fruit Pies","Ancestors":[],"TotalCount":0,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":false,"Title":"Fruit Pies","Description":"","Path":"/recipes/808/fruit-pies/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/global/recipes/nophoto/nopicture-250x250.png","UrlSafeTitle":"fruit-pies","SingularizedHubTitle":"Fruit Pie Recipes","ImageMap":null}],"TotalCount":27,"VideoCount":0,"IsSlideshowHub":false,"Macros":[],"IsMerchControlledHub":true,"Title":"Pear Pie","Description":"Looking for pear pie recipes? Allrecipes has more than 20 trusted pear pie recipes complete with ratings, reviews and baking tips.","Path":"/recipes/12951/desserts/pies/fruit-pies/pear-pie/","Slug":null,"ImageUrl":"http://images.media-allrecipes.com/userphotos/140x140/01/69/30/1693096.jpg","UrlSafeTitle":"desserts/pies/fruit-pies/pear-pie","SingularizedHubTitle":"Pear Pie Recipes","ImageMap":null}]}]
      """.stripMargin
    val json = new JSONArray(str)
    println(json.toString(5))
  }
}
