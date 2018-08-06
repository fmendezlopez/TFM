package es.uam.eps.tfm.fmendezlopez.extraction

/**
  * Created by Francisco on 02/04/2017.
  */
import java.io._
import java.net.SocketTimeoutException
import java.util.Properties

import com.github.tototoshi.csv.{CSVWriter, DefaultCSVFormat}
import es.uam.eps.tfm.fmendezlopez.scraping.Extractor
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

object CategoriesExtractor extends Logging{

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
      val outputDir = Utils.resolvePath("CategoriesExtractor", hostname)

      val allCategoriesURL = properties.getString("allrecipes.url.categories")
      val csvCategoriesName = s"${properties.getProperty("stage4.stage1.dataset.categories.filename")}"
      val csvCategoriesRelName = s"${properties.getProperty("stage4.stage1.dataset.category_hierarchy.filename")}"
      val delimiter:Char = properties.getProperty("stage4.stage1.output.csv.delimiter").toString.charAt(0)

      connectionProperties = new Properties()
      connectionProperties.put("timeout", properties.getProperty("stage0.scraping.timeout"))
      connectionProperties.put("delay-auth", properties.getProperty("stage0.scraping.delay.auth"))
      connectionProperties.put("max-attempts", properties.getProperty("stage0.scraping.max-attempts"))
      connectionProperties.put("delay-detection", properties.getProperty("stage0.scraping.delay.detection"))
      connectionProperties.put("referrer", allCategoriesURL)
      connectionProperties.put("max-body-size", properties.getProperty("stage0.scraping.maxBodySize"))
      connectionProperties.put("host", properties.getProperty("allrecipes.host"))
      connectionProperties.put("delay-category", properties.getProperty("stage0.scraping.delay.category"))
      connectionProperties.put("delay-categories", properties.getProperty("stage0.scraping.delay.categories"))
      connectionProperties.put("delay-n", properties.getProperty("stage0.scraping.delay.n"))
      connectionProperties.put("use-cookies", properties.getString("stage0.scraping.use-cookies"))
      connectionProperties.put("follow-redirects", properties.getProperty("stage0.scraping.followRedirects"))

      HttpManager.setProperties(properties)
      HttpManager.setConnectionProperties(connectionProperties)
      if(!HttpManager.resetToken){
        logger.fatal("Cannot retrieve auth token\nFinishing...")
        System.exit(1)
      }

      csvCategories = CSVManager.openCSVWriter(outputDir, csvCategoriesName, delimiter)
      csvCategoriesRel = CSVManager.openCSVWriter(outputDir, csvCategoriesRelName, delimiter)
      csvCategories.writeRow(properties.getString("stage4.stage1.dataset.categories.header"))
      csvCategoriesRel.writeRow(properties.getString("stage4.stage1.dataset.category_hierarchy.header"))

      val start = System.currentTimeMillis()
      //todo capturar excepciones de extractCategories
      val mainCategories = Extractor.extractCategories(allCategoriesURL).getOrElse(new JSONArray)
      val mainJSON = processGroups(mainCategories, -1, "", 0, 0, allCategoriesURL)
      val end = System.currentTimeMillis()
      logger.info(s"Elapsed time: ${end - start}ms")
      println(mainJSON.toString())
      CSVManager.closeCSVWriter(csvCategories)
      CSVManager.closeCSVWriter(csvCategoriesRel)
    }
  }

  def printHelp = {
    println("Usage:")
    println("\targ1: configuration path")
  }

  def processGroups(groups:JSONArray, id:Int, hierarchy:String, currDepth:Int, idParent:Int, cat_url : String) : Unit = {
    if(groups.length() == 0){
      return
    }
    else{
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
          logger.debug(s"Hierarchy: ${newHierarchy}")
          logger.debug(s"Depth: ${currDepth}")
          logger.debug(s"Processing group: ${name}")
          processCategories(categories, newHierarchy, currDepth, idParent, cat_url)
          logger.debug(s"Processed group: ${name}")
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
          logger.debug(s"Processing category: ${name}")
          val curl = s"${baseURL}${path}"
          connectionProperties.replace("referrer", cat_url)
          val subCategories = Extractor.extractCategories(curl).getOrElse(new JSONArray)
          ncategories += 1
          if(ncategories % connectionProperties.getProperty("delay-n").toInt == 0){
            logger.info(s"Got ${ncategories} categories. Sleeping...")
            Thread.sleep(connectionProperties.getProperty("delay-categories").toLong)
          }
          else{
            Thread.sleep(connectionProperties.getProperty("delay-category").toLong)
          }
          categoriesVisited += id
          processGroups(subCategories, id, newHierarchy, currDepth + 1, id, curl)
          logger.debug(s"Processed category: ${id}_${title}")
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
}
