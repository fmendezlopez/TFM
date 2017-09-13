package es.uam.eps.tfm.fmendezlopez.extraction

import java.io._
import java.net.{InetAddress, MalformedURLException, SocketTimeoutException, UnknownHostException}
import java.util
import java.util.{Properties, StringTokenizer}

import com.github.tototoshi.csv.CSVWriter
import es.uam.eps.tfm.fmendezlopez.exception.ScrapingDetectionException
import es.uam.eps.tfm.fmendezlopez.utils._
import es.uam.eps.tfm.fmendezlopez.exception.ScrapingDetectionException
import es.uam.eps.tfm.fmendezlopez.utils._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.elementList
import org.apache.commons.configuration2.Configuration
import org.apache.http.HttpResponse
import org.apache.http.client.ClientProtocolException
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.HttpGet
import org.apache.http.conn.ConnectTimeoutException
import org.apache.http.impl.client.{CloseableHttpClient, HttpClientBuilder}
import org.apache.http.util.EntityUtils
import org.json.{JSONArray, JSONObject, JSONTokener}
import org.jsoup.{Connection, Jsoup, UnsupportedMimeTypeException}

import scala.collection.mutable
import scala.util.Try

/**
  * Created by Francisco on 09/04/2017.
  */
object RecipeURLExtractor extends Logging{

  private var properties : Configuration = _
  private var errorTimes : Int = _
  private var connectionProperties : Properties = _
  private var baseURL : String = _

  private var csvURLs : CSVWriter = _
  private var csvCategories : CSVWriter = _
  private var csvCategoriesRel : CSVWriter = _

  private var processedCategories : Set[Int] = _

  def main(args: Array[String]): Unit = {
    if(args.length < 3){
      printHelp
      System.exit(1)
    }
    properties = PropertiesManager.loadProperties(args(0), PropertiesManager.EXTRACTION_PROPERTIES_FILE)
    val hostname = getHostName
    val rootID : Int = args(1).toInt
    val fromScratch : Boolean = args(2).toBoolean
    errorTimes = 0
    baseURL = properties.getString("allrecipes.url.base")
    val outputDir = Utils.resolvePath(1, hostname)

    val csvURLsName : String = s"${properties.getProperty("stage1.output.csv.url.filename")}"
    val csvCategoriesName : String = s"${properties.getProperty("stage1.output.csv.categories.filename")}"
    val csvCategoriesRelName : String = s"${properties.getProperty("stage1.output.csv.categories.relationship.filename")}"
    val delimiter:Char = properties.getProperty("stage1.output.csv.delimiter").toString.charAt(0)

    csvURLs = CSVManager.openCSVWriter(outputDir, csvURLsName, delimiter, !fromScratch)
    csvCategories = CSVManager.openCSVWriter(outputDir, csvCategoriesName, delimiter, !fromScratch)
    csvCategoriesRel = CSVManager.openCSVWriter(outputDir, csvCategoriesRelName, delimiter, !fromScratch)

    connectionProperties = new Properties()
    connectionProperties.put("user-agent", properties.getProperty("general.scraping.userAgent"))
    connectionProperties.put("timeout", properties.getProperty("stage1.scraping.delay.timeout"))
    connectionProperties.put("follow-redirects", properties.getProperty("stage1.scraping.followRedirects"))
    connectionProperties.put("delay-detection", properties.getProperty("stage1.scraping.time.detection"))
    connectionProperties.put("referrer", properties.getProperty("general.scraping.referrer"))
    connectionProperties.put("max-body-size", properties.getProperty("stage1.scraping.maxBodySize"))
    connectionProperties.put("delay", properties.getProperty("stage1.scraping.time.delay"))
    connectionProperties.put("attempts", properties.getProperty("stage1.scraping.maxAttempts"))
    connectionProperties.put("delay-category", properties.getProperty("stage1.scraping.delay.category"))
    connectionProperties.put("delay-npages", properties.getProperty("stage1.scraping.delay.npages"))
    connectionProperties.put("delay-pages", properties.getProperty("stage1.scraping.delay.pages"))
    connectionProperties.put("host", properties.getProperty("allrecipes.host"))
    HttpManager.setProperties(properties)
    HttpManager.setConnectionProperties(connectionProperties)

    processedCategories = Set[Int]()

    if(fromScratch){
      logger.info("Starting from scratch")
      logger.info(s"Root category: ${rootID}")

      csvURLs.writeRow(Seq("ID", "URL"))
      csvCategories.writeRow(Seq("ID", "NAME", "URL", "COUNT"))
      csvCategoriesRel.writeRow(Seq("ID_PARENT", "ID_CHILD"))

      val allCategoriesURL = properties.getString("allrecipes.url.categories")
      val groups = getCategoryChildren(s"${allCategoriesURL}").getOrElse(new JSONArray)
      logger.info("Searching category...")
      val (parentID, hierarchy, jsonRoot) = searchCategory(rootID, groups, true, "")
      logger.info(s"Category found with hierarchy ${hierarchy}")
      if(!jsonRoot.isDefined){
        logger.fatal(s"RootID ${rootID} not found")
        System.exit(1)
      }
      val status = new JSONObject()
      status.put("page_processed", false)
      status.put("parentID", parentID)
      status.put("rootID", rootID)
      status.put("last_page", 0)
      getURLsfromScratch(parentID, jsonRoot.get, status, hierarchy)
    }
    else{
      val statusFile : String = args(3)
      logger.info("Starting from status")
      logger.info(s"Root category: ${rootID}")
      val status = JSONManager.jsonFromFile(statusFile)
      val hierarchySeq = hierarchyToSeq(status.getString("hierarchy"))
      getURLsfromStatus(status.getInt("parentID"), status, status.getString("hierarchy"), hierarchySeq)

    }
    CSVManager.closeCSVWriter(csvURLs)
    CSVManager.closeCSVWriter(csvCategories)
    CSVManager.closeCSVWriter(csvCategoriesRel)
  }
  def getHostName(): String = {
    try
    {
      val addr : InetAddress = InetAddress.getLocalHost()
      addr.getHostName()
    }
    catch{
      case uhe : UnknownHostException =>
        s"${properties.getProperty("stage1.default.hostname")}"
    }
  }
  def printHelp : Unit = {
    System.err.println("Wrong parameters: try RecipeURLExtractor <config_path> <rootID> <from_scractch> [<status_file>]")
  }

  def hierarchyToSeq(hierarchy : String): mutable.MutableList[String] ={
    val tokenizer = new StringTokenizer(hierarchy, ">")
    var result = mutable.MutableList[String]()
    while(tokenizer.hasMoreTokens){
      result += tokenizer.nextToken()
    }
    result
  }

  def seqToHierarchy(hierarchy : Seq[String]): String ={
    hierarchy.foldLeft("")((acc, str) => s"${acc}>${str}")
  }

  def readStatus(statusFile : String): JSONObject = {
    val file = new File(statusFile)
    if(!file.exists() || !file.canRead)
      throw new FileNotFoundException()
    val fis = new FileInputStream(file)
    val isr = new InputStreamReader(fis)
    val br = new BufferedReader(isr)
    new JSONObject(new JSONTokener(br))
  }

  @throws(classOf[ScrapingDetectionException])
  def getCategoryChildren(url : String) : Option[JSONArray] = {

    try {
      val response = HttpManager.requestURL(url).orNull
      if(response == null){
        return None
      }
      else{
        val html = response
        val browser = JsoupBrowser()
        val doc = browser.parseString(html)
        val scripts: List[Element] = doc >> elementList("script")
        val elements = scripts.filter(element => element.innerHtml.contains("hubCategories"))
        if(elements.length == 1){
          val script = elements.head.innerHtml
          val startIndex = script.indexOf("[")
          val mainJSONStr = script.substring(startIndex, script.length - 3)
          val mainJSONObj = new JSONArray(mainJSONStr)
          return Some(mainJSONObj)
        }
        else{
          return None
        }
      }
    } catch {
      case sde: ScrapingDetectionException =>
        sde.printStackTrace()
        throw new ScrapingDetectionException(cause = sde)
      case ioe: ClientProtocolException =>
        ioe.printStackTrace() // more specific cases first !
        return None
      case e: IOException =>
        e.printStackTrace()
        return None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  def getCategoryRecipes(url : String) : Option[JSONObject] = {

    try {
      val response = HttpManager.requestURL(url).orNull
      if(response == null){
        return None
      }
      else{
        val html = response
        val browser = JsoupBrowser()
        val doc = browser.parseString(html)
        val scripts: List[Element] = doc >> elementList("script")
        val elements = scripts.filter(element => element.outerHtml.contains("application/ld+json"))
        if(elements.length == 1){
          val mainJSONStr = elements.head.innerHtml
          val mainJSONObj = new JSONObject(mainJSONStr)
          Some(mainJSONObj)
        }
        else{
          return None
        }
      }
    } catch {
      case sde: ScrapingDetectionException =>
        sde.printStackTrace()
        throw new ScrapingDetectionException(cause = sde)
      case ioe: ClientProtocolException =>
        ioe.printStackTrace() // more specific cases first !
        return None
      case e: IOException =>
        e.printStackTrace()
        return None
    }
  }

  def searchCategory(categoryID : Int, array : JSONArray, isGroup : Boolean, hierarchy : String): (Int, String, Option[JSONObject]) ={
    def search(parentID : Int, categoryID : Int, array : JSONArray, isGroup : Boolean, hierarchy : String): (Int, String, Option[JSONObject]) = {
      if(!isGroup){
        for(i <- 0 until array.length()){
          val category = array.getJSONObject(i)
          val id = category.getInt("Id")
          val title = category.getString("Title")
          val name = s"C_${id}_${title}"
          if(id == categoryID){
            val newHierarchy = s"${hierarchy}${name}>"
            return (parentID, newHierarchy, Some(category))
          }
        }
        for(i <- 0 until array.length()){
          val category = array.getJSONObject(i)
          val id = category.getInt("Id")
          val title = category.getString("Title")
          val name = s"C_${id}_${title}"
          val path = category.getString("Path")
          val newHierarchy = s"${hierarchy}${name}>"
          try{
            val groups = getCategoryChildren(s"${baseURL}${path}").getOrElse(new JSONArray)
            val ret = search(id, categoryID, groups, true, newHierarchy)
            if(ret._3.isDefined) return ret
          } catch {
            case sde: ScrapingDetectionException =>
            System.exit(1)
          }
        }
        return (parentID, hierarchy, None)
      }
      else{
        for(i <- 0 until array.length()){
          val group = array.getJSONObject(i)
          val id = group.getInt("Id")
          val title = group.getString("Title")
          val name = s"G_${id}_${title}"
          val newHierarchy = s"${hierarchy}${name}>"
          val categories = group.getJSONArray("ChildHubs")
          val ret = search(parentID, categoryID, categories, false, newHierarchy)
          if(ret._3.isDefined) return ret
        }
        return (parentID, hierarchy, None)
      }
    }
    search(0, categoryID, array, isGroup, hierarchy)
  }

  def getURLsfromScratch(idParent : Int, category : JSONObject, status : JSONObject, hierarchy : String) : Unit = {
    val path = category.getString("Path")
    val id = category.getInt("Id")
    val name = category.getString("Title")
    val url = s"${baseURL}${path}"
    val count = category.getInt("TotalCount")
    try{
      val groups = getCategoryChildren(url)
      if(groups.isEmpty) return
      val groups2 = groups.get
      if(groups2.length() == 0){
        status.put("hierarchy", hierarchy)
        logger.info(s"Processing pages for category ${id}...")
        status.put("last_page", 0)
        processCategory(id, url, status)
        logger.info(s"Processed pages for category ${id}...")
        Thread.sleep(connectionProperties.getProperty("delay-category").toInt)
        csvCategories.writeRow(Seq(id, name, url, count))
        csvCategoriesRel.writeRow(Seq(idParent, id))
      }
      else{
        for(i <- 0 until groups2.length()){
          val group = groups2.getJSONObject(i)
          val groupId = group.getInt("Id")
          val groupTitle = group.getString("Title")
          val hierarchyName = s"G_${groupId}_${groupTitle}"
          var newHierarchy : String = s"${hierarchy}${hierarchyName}>"
          val categories = group.getJSONArray("ChildHubs")
          for(i <- 0 until categories.length()){
            val cat = categories.getJSONObject(i)
            val catId = cat.getInt("Id")
            if(!processedCategories.contains(catId)) {
              processedCategories += catId
              val catName = cat.getString("Title")
              val hierarchyName = s"C_${catId}_${catName}"
              newHierarchy = s"${newHierarchy}${hierarchyName}>"
              getURLsfromScratch(id, cat, status, newHierarchy)
            }
          }
        }
        status.put("hierarchy", hierarchy)
        status.put("last_page", 0)
        logger.info(s"Processing pages for parent category ${id}...")
        processCategory(id, url, status)
        logger.info(s"Processed pages for parent category ${id}...")
        Thread.sleep(connectionProperties.getProperty("delay-category").toInt)
        csvCategories.writeRow(Seq(id, name, url, count))
        csvCategoriesRel.writeRow(Seq(idParent, id))
        if(id == status.getInt("rootID")){
          logger.info("URL extraction finished")
          return
        }
      }
    } catch {
      case sde: ScrapingDetectionException =>
        System.exit(1)
    }
  }
  def getURLsfromStatus(idParent : Int, status : JSONObject, hierarchy : String, hierarchySeq : mutable.MutableList[String]) : Unit = {
    val allCategoriesURL = properties.getString("allrecipes.url.categories")
    val groups = getCategoryChildren(allCategoriesURL).getOrElse(new JSONArray)
    if(groups.length() == 0){
      logger.fatal(s"Main JSON not found!")
      System.exit(1)
    }
    get(idParent, Right(groups), status, hierarchySeq, hierarchySeq, false, true, 0)
    def get(idParent : Int, json : Either[JSONObject, JSONArray], status : JSONObject, hierarchy : mutable.MutableList[String], hierarchySeq : mutable.MutableList[String], found : Boolean, isGroup : Boolean, seqIndex : Int) : Boolean = {
      if(json.isLeft){
        val obj = json.left.get
        val id = obj.getInt("Id")
        val path = obj.getString("Path")
        val url = s"${baseURL}${path}"
        val name = obj.getString("Title")
        val count = obj.getInt("TotalCount")
        status.put("hierarchy", seqToHierarchy(hierarchy))
        logger.info(s"Processing pages for category ${id}...")
        status.put("last_page", 0)
        processCategory(id, url, status)
        logger.info(s"Processed pages for category ${id}...")
        Thread.sleep(connectionProperties.getProperty("delay-category").toInt)
        csvCategories.writeRow(Seq(id, name, url, count))
        csvCategoriesRel.writeRow(Seq(idParent, id))

        found
      }
      else{
        val array = json.right.get
        var index = -1
        var nextFound = found
        if(found){
          index = 0
        }
        else{
          val tokenizer = new StringTokenizer(hierarchySeq.head, "_")
          val currObjType = tokenizer.nextToken()
          val currID = tokenizer.nextToken().toInt

          var continue = true
          var i = 0
          while(continue){
            val item = array.getJSONObject(i)
            val itemID = item.getInt("Id")
            continue = i < array.length()
            if(itemID == currID){
              index = i
              continue = false
            }
            i += 1
          }
          nextFound = hierarchySeq.length == 1
          if(nextFound){
            get(idParent, Left(array.getJSONObject(index)), status, hierarchy, hierarchySeq.tail, nextFound, false, 0)
            index += 1
          }
        }
        /*
        for(i <- 0 until array.length()){
          item = array.getJSONObject(i)
          val itemID = item.getInt("Id")
          if(itemID == currID) index = i
        }
        */
        var unique = false
        var categoryProcessed = false
        val newSeq = if(hierarchySeq.isEmpty) mutable.MutableList[String]() else hierarchySeq.tail
        for(i <- index until array.length()){
          var newParentID = idParent
          val elem = array.getJSONObject(i)
          val id = elem.getInt("Id")
          val name = elem.getString("Title")
          var newArray : JSONArray = null
          var currObjType : String = ""
          currObjType = if(!isGroup) "C" else "G"
          val str = s"${currObjType}_${id}_${name}"
          if(hierarchy.length < seqIndex + 1) hierarchy += str
          else hierarchy(seqIndex) = str
          val newGroup = !isGroup
          if(!isGroup){
            if(processedCategories.contains(id)){
              categoryProcessed = true
            }
            else{
              categoryProcessed = false
              processedCategories += id
              val path = elem.getString("Path")
              val url = s"${baseURL}${path}"
              logger.info(s"Requesting groups for category ${id}...")
              val groups = getCategoryChildren(url)
              if(groups.isEmpty){
                logger.info(s"Groups are None")
                return found
              }
              else{
                val getArr = groups.get
                if(getArr.length() == 0){
                  logger.info(s"Groups are Empty")
                  nextFound = get(newParentID, Left(elem), status, hierarchy, newSeq, nextFound, newGroup, seqIndex + 1)
                  unique = true
                }
                else{
                  newParentID = id
                  newArray = groups.get
                  unique = false
                }
              }
            }

          }
          else{
            newArray = elem.getJSONArray("ChildHubs")
          }
          if(!unique && !categoryProcessed){

            nextFound = get(newParentID, Right(newArray), status, hierarchy, newSeq, nextFound, newGroup, seqIndex + 1)

            if(currObjType.equals("C")){
              val id = elem.getInt("Id")
              val path = elem.getString("Path")
              val url = s"${baseURL}${path}"
              val name = elem.getString("Title")
              val count = elem.getInt("TotalCount")
              logger.info(s"Processing pages for category ${id}...")
              status.put("last_page", 0)
              status.put("hierarchy", seqToHierarchy(hierarchy))
              processCategory(id, url, status)
              logger.info(s"Processed pages for category ${id}...")
              Thread.sleep(connectionProperties.getProperty("delay-category").toInt)
              csvCategories.writeRow(Seq(id, name, url, count))
              csvCategoriesRel.writeRow(Seq(idParent, id))
              if(id == status.getInt("rootID")){
                logger.info("URL extraction finished")
                nextFound
              }
            }
          }
        }
        nextFound
        /*
        pendientes: pisar seq y conjunto de categorias ya exploradas
        if(hierarchySeq.length == 1){
          get(newParentID, Left(array.getJSONObject(index)), status, hierarchy, hierarchySeq.tail)
          index += 1
        }
        var newArray : JSONArray = null
        if(currObjType.equals("C")){
          newParentID = currID
          val path = item.getString("Path")
          val url = s"${baseURL}${path}"
          val groups = getCategoryChildren(url)
          if(groups.isEmpty) return else newArray = groups.get
        }
        else{
          newArray = item.getJSONArray("ChildHubs")
        }
        for(i <- index until array.length()){
          val elem = newArray.get(i)
          get(newParentID, Right(newArray), status, hierarchy, hierarchySeq.tail)
        }
        if(currObjType.equals("C")){
          val id = item.getInt("Id")
          val path = item.getString("Path")
          val url = s"${baseURL}${path}"
          val name = item.getString("Title")
          val count = item.getInt("TotalCount")
          logger.info(s"Processing pages for category ${id}...")
          processCategory(id, url, status)
          logger.info(s"Processed pages for category ${id}...")
          Thread.sleep(connectionProperties.getProperty("delay-category").toInt)
          csvCategories.writeRow(Seq(id, name, url, count))
          csvCategoriesRel.writeRow(Seq(idParent, id))
        }
        */
      }
    }
  }
  def processCategory(idCategory : Int, url : String, status : JSONObject): Unit ={
    val pageStr = "?page="
    var continue = true
    var i = status.getInt("last_page") + 1
    do{
      status.put("page_processed", false)
      status.put("last_page", i)
      //status.write(statusJSON)
      logger.info(status.toString)
      val pagePath = s"${url}${pageStr}${i}"
      continue = processCategoryPage(idCategory, pagePath)
      status.put("page_processed", true)
      //status.write(statusJSON)
      logger.info(status.toString)
      i += 1
      if(i % connectionProperties.getProperty("delay-npages").toInt == 0){
        Thread.sleep(connectionProperties.getProperty("delay-pages").toInt)
      }
    }while(continue)
  }
  def processCategoryPage(idCategory : Int, url : String): Boolean ={
    try{
      val json = getCategoryRecipes(url)
      if(json.isEmpty) return false
      else{
        val arr = json.get.getJSONArray("itemListElement")
        for(i <- 0 until arr.length()){
          val recipe = arr.getJSONObject(i)
          val url = recipe.getString("url")
          csvURLs.writeRow(Seq(idCategory, url))
        }
        return true
      }
    } catch {
      case sde: ScrapingDetectionException =>
        System.exit(1)
        return false
    }
  }
}
