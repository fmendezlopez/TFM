package es.uam.eps.tfm.fmendezlopez.utils

import java.io.IOException
import java.net.{MalformedURLException, SocketTimeoutException}
import java.util.Properties

import es.uam.eps.tfm.fmendezlopez.exception.{APIAuthorizationException, APILoginException, ScrapingDetectionException}
import org.apache.commons.configuration2.Configuration
import org.json.JSONObject
import org.jsoup.{Connection, Jsoup, UnsupportedMimeTypeException}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import scala.collection.parallel.mutable.ParArray

/**
  * Created by Francisco on 14/04/2017.
  */
object HttpManager extends Logging{

  private var properties : Configuration = _
  private var connectionProperties : Properties = _

  private var webURLs : Map[String, String] = _
  private var apiURLs : Map[String, String] = _

  private val userAgents = Seq(
    "Opera/9.80 (Windows NT 6.0) Presto/2.12.388 Version/12.14",
    "Mozilla/5.0 (Windows NT 6.0; rv:2.0) Gecko/20100101 Firefox/4.0 Opera 12.14",
    "Opera/12.80 (Windows NT 5.1; U; en) Presto/2.10.289 Version/12.02",
    "Opera/9.80 (Windows NT 5.1; U; zh-sg) Presto/2.9.181 Version/12.00",
    "Opera/9.80 (Macintosh; Intel Mac OS X 10.6.8; U; de) Presto/2.9.168 Version/11.52",
    "Opera/9.80 (X11; Linux x86_64; U; Ubuntu/10.10 (maverick); pl) Presto/2.7.62 Version/11.01",
    "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1664.3 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1623.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.17 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1464.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1468.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:25.0) Gecko/20100101 Firefox/25.0",
    "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:24.0) Gecko/20100101 Firefox/24.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:24.0) Gecko/20100101 Firefox/24.0",
    "Mozilla/5.0 (Windows NT 6.2; rv:22.0) Gecko/20130405 Firefox/23.0",
    "Mozilla/5.0 (Windows NT 6.1; rv:6.0) Gecko/20100101 Firefox/19.0",
    "Mozilla/5.0 (Windows NT 6.1; rv:14.0) Gecko/20100101 Firefox/18.0.1",
    "Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20121202 Firefox/17.0 Iceweasel/17.0.1",
    "Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0",
    "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)",
    "Mozilla/5.0 (Windows; U; MSIE 9.0; WIndows NT 9.0; en-US))",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; Zune 4.0; InfoPath.3; MS-RTC LM 8; .NET4.0C; .NET4.0E)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; chromeframe/12.0.742.112)",
    "Mozilla/5.0 (iPad; CPU OS 6_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/6.0 Mobile/10A5355d Safari/8536.25",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2"
  )

  def setProperties(properties : Configuration): Unit ={
    this.properties = properties

    webURLs = Map(
      "base" -> properties.getString("allrecipes.url.base"),
      "categories" -> properties.getString("allrecipes.url.base"),
      "recipe" -> properties.getString("allrecipes.url.recipe"),
      "user" -> properties.getString("allrecipes.url.user")
    )

    apiURLs = Map(
      "base" -> properties.getString("allrecipes.api.url.base"),
      "recipe" -> properties.getString("allrecipes.api.url.recipe"),
      "recipe_reviews" -> properties.getString("allrecipes.api.url.recipe.review"),
      "user" -> properties.getString("allrecipes.api.url.user"),
      "user_followers" -> properties.getString("allrecipes.api.url.user.followers"),
      "user_following" -> properties.getString("allrecipes.api.url.user.following"),
      "user_recipes" -> properties.getString("allrecipes.api.url.user.recipes"),
      "user_fav" -> properties.getString("allrecipes.api.url.user.fav"),
      "user_madeit" -> properties.getString("allrecipes.api.url.user.madeit"),
      "user_reviews" -> properties.getString("allrecipes.api.url.user.reviews")
    )

  }

  def setConnectionProperties(connectionProperties : Properties): Unit ={
    this.connectionProperties = connectionProperties
  }

  def requestAuthToken(): Option[Map[String, String]] ={
    val url = webURLs("base")
    makeAuthRequest(url)
  }

  def selectUserAgent : String = {
    userAgents(scala.util.Random.nextInt(userAgents.length))
  }

  def makeAuthRequest(url : String, connectionProperties : Properties = connectionProperties): Option[Map[String, String]] ={
    val connection = Jsoup.connect(url)

    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.followRedirects(false)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    connection.ignoreHttpErrors(true)
    connection.referrer(connectionProperties.getProperty("referrer"))
    connection.method(Connection.Method.GET)

    connection.header("Host", connectionProperties.getProperty("host"))
    connection.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    connection.header("Accept-Language", "keep-alive")
    connection.header("Accept-Encoding", "gzip, deflate")
    connection.header("Connection", "keep-alive")
    connection.header("Upgrade-Insecure-Requests", "1")

    logger.debug(s"GET ${url}")
    val response = connection.execute()
    val errorCode = response.statusCode()
    logger.debug(s"Got HTTP Status ${errorCode}")
    var ret : Option[Map[String, String]] = None
    errorCode match {
      case 200 =>
        val prefix = properties.getProperty("general.scraping.auth.token.prefix")
        val arsiteuser = response.cookie(properties.getProperty("general.scraping.cookiename.arsiteuser").toString)
        val arsession = response.cookie(properties.getProperty("general.scraping.cookiename.arsession").toString)
        val artoken = response.cookie(properties.getProperty("general.scraping.cookiename.artoken").toString)
        ret = Some(Map(
          "ar_token" -> artoken,
          "ar_user" -> arsiteuser,
          "ar_session" -> arsession
        ))
      case 403 =>
        logger.error(s"Scraper detected!\n")
        ret = None
      case _ =>
        ret = None
    }
    ret
  }

  @throws(classOf[ScrapingDetectionException])
  def requestCategory(url : String, connectionProperties : Properties = connectionProperties) : Option[String] = {

    val connection = Jsoup.connect(url)

    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    connection.followRedirects(false)
    connection.referrer(connectionProperties.getProperty("referrer"))
    connection.ignoreHttpErrors(true)

    connection.method(Connection.Method.GET)
    connection.header("Host", connectionProperties.getProperty("host"))
    connection.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    connection.header("Connection", "keep-alive")
    connection.header("Accept-Encoding", "gzip, deflate")
    connection.header("Connection", "keep-alive")
    connection.header("Upgrade-Insecure-Requests", "1")
    connection.cookie(connectionProperties.getProperty("cookie-token"), connectionProperties.getProperty("auth-token"))

    var times = 0
    val maxTimes = connectionProperties.getProperty("max-attempts").toInt
    var delayDetection = connectionProperties.getProperty("delay-detection").toInt
    val delayAuth = connectionProperties.getProperty("delay-auth").toInt

    try {
      var continue = true
      var ret: Option[String] = null
      do {
        logger.debug(s"GET ${url}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 401 =>
            logger.warn("Token expired. Sleeping...")
            Thread.sleep(delayAuth)
            logger.warn(s"Requesting another token...")
            val res = requestAuthToken()
            if(res.isEmpty){
              throw new ScrapingDetectionException(s"Scraped detected when trying to get auth token ${url}")
            }
            connectionProperties.put("auth-token", res.get)
            connection.header("Authorization", connectionProperties.getProperty("auth-token"))
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if (times == maxTimes) {
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
            delayDetection *= 2
          case _ =>
            continue = false
            ret = None
        }
      } while (continue)
      return ret
    } catch {
      case mue: MalformedURLException =>
        mue.printStackTrace()
        return None
      case ume: UnsupportedMimeTypeException =>
        ume.printStackTrace()
        return None
      case ste: SocketTimeoutException =>
        ste.printStackTrace()
        return None
      case ioe: IOException =>
        ioe.printStackTrace()
        return None
    }
  }

  def getWebConnection(url : String, connectionProperties: Properties) : Connection = {
    val connection = Jsoup.connect(url)

    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    connection.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    connection.referrer(connectionProperties.getProperty("referrer"))
    connection.ignoreHttpErrors(true)

    connection.method(Connection.Method.GET)
    connection.header("Host", connectionProperties.getProperty("host"))
    connection.header("Accept", "text/html,application/xhtml+x…lication/xml;q=0.9,*/*;q=0.8")
    connection.header("Accept-Encoding", "gzip, deflate, br")
    connection.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    connection.header("Connection", "keep-alive")
    connection.header("Upgrade-Insecure-Requests", "1")

    connection.cookie(s"${properties.getString("general.scraping.cookiename.arsiteuser")}", connectionProperties.getProperty("ar_user"))
    connection.cookie(s"${properties.getString("general.scraping.cookiename.arsession")}", connectionProperties.getProperty("ar_session"))
    connection.cookie(s"${properties.getString("general.scraping.cookiename.artoken")}", connectionProperties.getProperty("ar_token"))
    connection
  }

  def resetToken(connectionProperties : Properties) : Option[Properties] = {
    val res = requestAuthToken()
    if(res.isEmpty)
      None
    val map = res.get
    val newProps = connectionProperties
    newProps.replace("ar_token", map("ar_token"))
    newProps.replace("ar_user", map("ar_user"))
    newProps.replace("ar_session", map("ar_session"))
    newProps.replace("auth-token", s"${properties.getString("general.scraping.auth.token.prefix")} ${connectionProperties.getProperty("ar_token")}")
    logger.debug(s"NEW-TOKEN: ${newProps.getProperty("auth-token")}")
    Some(newProps)
  }
  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestRecipeWeb(id : String, connectionProperties  : Properties = connectionProperties): Option[String] = {
    val url = String.format(webURLs("recipe"), id)
    requestRecipeWebURL(url, connectionProperties)
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestRecipeWebURL(url : String, connectionProperties  : Properties = connectionProperties): Option[String] = {
    var connection = getWebConnection(url, connectionProperties)

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt
    val delayAuth = connectionProperties.getProperty("delay-auth").toInt
    val max_times_notfound = connectionProperties.getProperty("max-times-301").toInt
    var notFound = 0

    try {
      var continue = true
      var ret: Option[String] = None
      do {
        logger.debug(s"GET ${connection.request().url.toString()}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 301 =>
            if(notFound == max_times_notfound){
              logger.warn(s"Ignored HTTP request. Not found ${max_times_notfound} times...")
              continue = false
            }
            else{
              var newURL = response.header("Location")
              if(!newURL.startsWith("http")) newURL = s"${connectionProperties.getProperty("base-host")}${newURL}"
              connection = getWebConnection(newURL, connectionProperties)
              notFound += 1
            }
          case 401 =>
            logger.warn("Token expired. Sleeping...")
            Thread.sleep(delayAuth)
            throw new APIAuthorizationException("API authorization token expired.")
            /*
            logger.warn(s"Requesting another token...")
            val res = resetToken(connectionProperties)
            if(res.isEmpty){
              throw new ScrapingDetectionException(s"Scraped detected when trying to get auth token ${url}")
            }
            connection.header("Authorization", connectionProperties.getProperty("auth-token"))
            */
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if (times == maxTimes) {
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = None
        }
      } while (continue)
      ret
    } catch {
      case mue: MalformedURLException =>
        mue.printStackTrace()
        None
      case ume: UnsupportedMimeTypeException =>
        ume.printStackTrace()
        None
      case ste: SocketTimeoutException =>
        ste.printStackTrace()
        None
      case ioe: IOException =>
        ioe.printStackTrace()
        None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestUserWeb(id : Long, connectionProperties : Properties = connectionProperties): Option[String] = {

    val url = String.format(webURLs("user"), id.toString)
    var connection = getWebConnection(url, connectionProperties)

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt
    val delayAuth = connectionProperties.getProperty("delay-auth").toInt
    val max_times_notfound = connectionProperties.getProperty("max-times-301").toInt
    var notFound = 0

    try {
      var continue = true
      var ret: Option[String] = None
      do {
        logger.debug(s"GET ${connection.request().url.toString()}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 301 =>
            if(notFound == max_times_notfound){
              logger.warn(s"Ignored HTTP request. Not found ${max_times_notfound} times...")
              continue = false
            }
            else{
              var newURL = response.header("Location")
              if(!newURL.startsWith("http")) newURL = s"${newURL}${connectionProperties.getProperty("base-host")}"
              connection = getWebConnection(newURL, connectionProperties)
              notFound += 1
            }
          case 401 =>
            logger.warn("Token expired. Sleeping...")
            Thread.sleep(delayAuth)
            throw new APIAuthorizationException("API authorization token expired.")
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if (times == maxTimes) {
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = None
        }
      } while (continue)
      ret
    } catch {
      case mue: MalformedURLException =>
        mue.printStackTrace()
        None
      case ume: UnsupportedMimeTypeException =>
        ume.printStackTrace()
        None
      case ste: SocketTimeoutException =>
        ste.printStackTrace()
        None
      case ioe: IOException =>
        ioe.printStackTrace()
        None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestRecipeAPI(id : Long, connectionProperties : Properties = connectionProperties): Option[String] = {
    val url = String.format(apiURLs("recipe"), id.toString)
    val connection = Jsoup.connect(url)

    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    connection.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    connection.referrer(connectionProperties.getProperty("referrer"))
    connection.ignoreHttpErrors(true)
    connection.ignoreContentType(true)

    connection.method(Connection.Method.GET)
    connection.header("Host", connectionProperties.getProperty("api-host"))
    connection.header("Accept", "*/*")
    connection.header("Origin", connectionProperties.getProperty("base-host"))
    connection.header("Accept-Encoding", "gzip, deflate, br")
    connection.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    connection.header("Connection", "keep-alive")
    connection.header("Authorization", connectionProperties.getProperty("auth-token"))
    connection.header("X-Requested-With", "XMLHttpRequest")
    connection.header("Referer", connectionProperties.getProperty("referrer"))

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt
    val delayAuth = connectionProperties.getProperty("delay-auth").toInt

    try {
      var continue = true
      var ret: Option[String] = None
      do {
        logger.debug(s"GET ${url}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 401 =>
            logger.warn("Token expired. Sleeping...")
            Thread.sleep(delayAuth)
            throw new APIAuthorizationException("API authorization token expired.")
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if (times == maxTimes) {
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = None
        }
      } while (continue)
      return ret
    } catch {
      case mue: MalformedURLException =>
        mue.printStackTrace()
        None
      case ume: UnsupportedMimeTypeException =>
        ume.printStackTrace()
        None
      case ste: SocketTimeoutException =>
        ste.printStackTrace()
        None
      case ioe: IOException =>
        ioe.printStackTrace()
        None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestRecipeAPIURL(url : String, connectionProperties : Properties = connectionProperties): Option[String] = {
    val connection = Jsoup.connect(url)

    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    connection.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    connection.referrer(connectionProperties.getProperty("referrer"))
    connection.ignoreHttpErrors(true)
    connection.ignoreContentType(true)

    connection.method(Connection.Method.GET)
    connection.header("Host", connectionProperties.getProperty("api-host"))
    connection.header("Accept", "*/*")
    connection.header("Origin", connectionProperties.getProperty("base-host"))
    connection.header("Accept-Encoding", "gzip, deflate, br")
    connection.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    connection.header("Connection", "keep-alive")
    connection.header("Authorization", connectionProperties.getProperty("auth-token"))
    connection.header("X-Requested-With", "XMLHttpRequest")
    connection.header("Referer", connectionProperties.getProperty("referrer"))

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt
    val delayAuth = connectionProperties.getProperty("delay-auth").toInt

    try {
      var continue = true
      var ret: Option[String] = None
      do {
        logger.debug(s"GET ${url}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 401 =>
            val json = new JSONObject(response.body())
            val msg = json.getString("message")
            if(msg == properties.getString("general.api.error.token")){
              logger.warn("Token expired. Sleeping...")
              Thread.sleep(delayAuth)
              throw new APIAuthorizationException("API authorization token expired.")
            }
            else if(msg == properties.getString("general.api.error.login")){
              logger.warn("Login must be performed. Ignoring...")
              throw new APILoginException("API login must be performed.")
            }
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if (times == maxTimes) {
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = None
        }
      } while (continue)
      return ret
    } catch {
      case mue: MalformedURLException =>
        mue.printStackTrace()
        None
      case ume: UnsupportedMimeTypeException =>
        ume.printStackTrace()
        None
      case ste: SocketTimeoutException =>
        ste.printStackTrace()
        None
      case ioe: IOException =>
        ioe.printStackTrace()
        None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestRecipeReviewsAPI(id : Long, connectionProperties : Properties = connectionProperties, pagesize : Int, pageNumber : Int): Option[String] = {
    val url = String.format(apiURLs("recipe_reviews"), id.toString, pagesize.toString, pageNumber.toString)
    val connection = Jsoup.connect(url)

    connection.userAgent(connectionProperties.getProperty("user-agent"))
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    connection.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    connection.referrer(connectionProperties.getProperty("referrer"))
    connection.ignoreHttpErrors(true)
    connection.ignoreContentType(true)

    connection.method(Connection.Method.GET)
    connection.header("Host", connectionProperties.getProperty("host"))
    connection.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    connection.header("Connection", "keep-alive")
    connection.header("Authorization", connectionProperties.getProperty("auth-token"))

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt
    val delayAuth = connectionProperties.getProperty("delay-auth").toInt

    try {
      var continue = true
      var ret: Option[String] = None
      do {
        logger.debug(s"GET ${url}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 401 =>
            logger.warn("Token expired. Sleeping...")
            Thread.sleep(delayAuth)
            throw new APIAuthorizationException("API authorization token expired.")
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if (times == maxTimes) {
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = None
        }
      } while (continue)
      return ret
    } catch {
      case mue: MalformedURLException =>
        mue.printStackTrace()
        return None
      case ume: UnsupportedMimeTypeException =>
        ume.printStackTrace()
        return None
      case ste: SocketTimeoutException =>
        ste.printStackTrace()
        return None
      case ioe: IOException =>
        ioe.printStackTrace()
        return None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestUserAPI(id : Long, connectionProperties : Properties = connectionProperties): Option[String] = {
    val url = String.format(apiURLs("user"), id.toString)
    val connection = Jsoup.connect(url)

    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    connection.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    connection.referrer(connectionProperties.getProperty("referrer"))
    connection.ignoreHttpErrors(true)
    connection.ignoreContentType(true)

    connection.method(Connection.Method.GET)
    connection.header("Host", connectionProperties.getProperty("api-host"))
    connection.header("Accept", "*/*")
    connection.header("Origin", connectionProperties.getProperty("base-host"))
    connection.header("Accept-Encoding", "gzip, deflate, br")
    connection.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    connection.header("Connection", "keep-alive")
    connection.header("Authorization", connectionProperties.getProperty("auth-token"))
    connection.header("X-Requested-With", "XMLHttpRequest")

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt
    val delayAuth = connectionProperties.getProperty("delay-auth").toInt

    try {
      var continue = true
      var ret: Option[String] = None
      do {
        logger.debug(s"GET ${url}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 401 =>
            logger.warn("Token expired. Sleeping...")
            Thread.sleep(delayAuth)
            throw new APIAuthorizationException("API authorization token expired.")
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if (times == maxTimes) {
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = None
        }
      } while (continue)
      ret
    } catch {
      case mue: MalformedURLException =>
        mue.printStackTrace()
        None
      case ume: UnsupportedMimeTypeException =>
        ume.printStackTrace()
        None
      case ste: SocketTimeoutException =>
        ste.printStackTrace()
        None
      case ioe: IOException =>
        ioe.printStackTrace()
        None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestFollowingAPI(id : Long, connectionProperties  : Properties = connectionProperties, pagesize : Int, pageNumber : Int) : Option[String] = {
    val url = String.format(apiURLs("user_following"), id.toString, pagesize.toString, pageNumber.toString)
    requestUserParameterToAPI(url)
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestFollowersAPI(id : Long, connectionProperties  : Properties = connectionProperties, pagesize : Int, pageNumber : Int) : Option[String] = {
    val url = String.format(apiURLs("user_followers"), id.toString, pagesize.toString, pageNumber.toString)
    requestUserParameterToAPI(url)
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestUserRecipes(id : Long, connectionProperties  : Properties = connectionProperties, pagesize : Int, pageNumber : Int, list : String): Option[String] ={

    val url = list match {
      case "recipes" => String.format(apiURLs("user_recipes"), id.toString, pagesize.toString, pageNumber.toString)
      case "fav" => String.format(apiURLs("user_fav"), id.toString, pagesize.toString, pageNumber.toString)
      case "madeit" => String.format(apiURLs("user_madeit"), id.toString, pagesize.toString, pageNumber.toString)
    }
    requestUserParameterToAPI(url)
  }

  def requestUserReviews(id : Long, connectionProperties  : Properties = connectionProperties, pagesize : Int, pageNumber : Int) : Option[String] = {
    val url = String.format(apiURLs("user_reviews"), id.toString, pagesize.toString, pageNumber.toString)
    requestUserParameterToAPI(url)
  }

  @throws(classOf[ScrapingDetectionException])
  def requestURL(url : String, connectionProperties  : Properties = connectionProperties):Option[String] ={
    val connection = Jsoup.connect(url)
    //connection.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    connection.followRedirects(true)
    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    val ref = connectionProperties.getProperty("referrer")
    if(!ref.isEmpty)connection.referrer(ref)

    connection.header("Host", connectionProperties.getProperty("host"))
    connection.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    connection.header("Connection", "keep-alive")
    connection.header("Accept-Encoding", "gzip, deflate, br")
    connection.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    connection.header("Upgrade-Insecure-Requests", "1")
    connection.ignoreHttpErrors(true)
    connection.ignoreContentType(true)
    connection.method(Connection.Method.GET)

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt

    try{
      var continue = true
      var ret : Option[String] = null
      do{
        logger.debug(s"GET ${url}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if(times == maxTimes){
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = Some(response.body())
        }
      }while(continue)
      return ret
    } catch {
      case mue : MalformedURLException =>
        mue.printStackTrace()
        return None
      case ume : UnsupportedMimeTypeException =>
        ume.printStackTrace()
        return None
      case ste : SocketTimeoutException =>
        ste.printStackTrace()
        return None
      case ioe : IOException =>
        ioe.printStackTrace()
        return None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  @throws(classOf[APIAuthorizationException])
  def requestUserParameterToAPI(url : String) : Option[String] = {
    val get = Jsoup.connect(url)
    val useragent = selectUserAgent
    get.userAgent(useragent)
    get.timeout(connectionProperties.getProperty("timeout").toInt)
    get.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    get.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    get.referrer(connectionProperties.getProperty("referrer"))
    get.ignoreHttpErrors(true)
    get.ignoreContentType(true)

    get.method(Connection.Method.GET)
    get.header("Host", connectionProperties.getProperty("api-host"))
    get.header("Accept", "*/*")
    get.header("Origin", connectionProperties.getProperty("base-host"))
    get.header("Accept-Encoding", "gzip, deflate, br")
    get.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    get.header("Connection", "keep-alive")
    get.header("Authorization", connectionProperties.getProperty("auth-token"))
    get.header("X-Requested-With", "XMLHttpRequest")

    val options = Jsoup.connect(url)
    options.userAgent(useragent)
    options.timeout(connectionProperties.getProperty("timeout").toInt)
    options.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    options.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    options.referrer(connectionProperties.getProperty("referrer"))
    options.ignoreHttpErrors(true)

    options.method(Connection.Method.OPTIONS)
    options.header("Host", connectionProperties.getProperty("api-host"))
    options.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    options.header("Origin", connectionProperties.getProperty("base-host"))
    options.header("Accept-Encoding", "gzip, deflate, br")
    options.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    options.header("Access-Control-Request-Method", "GET")
    options.header("Access-Control-Request-Headers", "authorization, x-requested-with")
    options.header("Connection", "keep-alive")
    options.header("Authorization", connectionProperties.getProperty("auth-token"))
    options.header("X-Requested-With", "XMLHttpRequest")

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt
    val delayAuth = connectionProperties.getProperty("delay-auth").toInt

    try {
      var continue = true
      var ret: Option[String] = None
      do {
        logger.debug(s"OPTIONS ${url}")
        options.execute()
        logger.debug(s"GET ${url}")
        val response = get.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 401 =>
            logger.warn("Token expired. Sleeping...")
            Thread.sleep(delayAuth)
            throw new APIAuthorizationException("API authorization token expired.")
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if (times == maxTimes) {
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = None
        }
      } while (continue)
      return ret
    } catch {
      case mue: MalformedURLException =>
        mue.printStackTrace()
        return None
      case ume: UnsupportedMimeTypeException =>
        ume.printStackTrace()
        return None
      case ste: SocketTimeoutException =>
        ste.printStackTrace()
        return None
      case ioe: IOException =>
        ioe.printStackTrace()
        return None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  def requestRecipeKeyCookie(url : String, connectionProperties  : Properties = connectionProperties):Option[String] ={
    val connection = Jsoup.connect(url)
    connection.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    val ref = connectionProperties.getProperty("referrer")
    if(!ref.isEmpty)connection.referrer(ref)

    connection.header("Host", connectionProperties.getProperty("host"))
    connection.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    connection.header("Connection", "keep-alive")
    connection.header("Accept-Encoding", "gzip, deflate, br")
    connection.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    connection.header("Upgrade-Insecure-Requests", "1")
    connection.ignoreHttpErrors(true)
    connection.ignoreContentType(true)
    connection.method(Connection.Method.GET)

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt

    try{
      var continue = true
      var ret : Option[String] = null
      do{
        logger.debug(s"GET ${url}")
        val response = connection.execute()

        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if(times == maxTimes){
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = Some(response.cookie(properties.getString("recipekey.cookie.name")))
        }
      }while(continue)
      return ret
    } catch {
      case mue : MalformedURLException =>
        mue.printStackTrace()
        return None
      case ume : UnsupportedMimeTypeException =>
        ume.printStackTrace()
        return None
      case ste : SocketTimeoutException =>
        ste.printStackTrace()
        return None
      case ioe : IOException =>
        ioe.printStackTrace()
        return None
    }
  }

  @throws(classOf[ScrapingDetectionException])
  def requestRecipeKeyURL(url : String, connectionProperties : Properties = connectionProperties):Option[String] ={
    val connection = Jsoup.connect(url)
    connection.followRedirects(connectionProperties.getProperty("follow-redirects").toBoolean)
    connection.userAgent(selectUserAgent)
    connection.timeout(connectionProperties.getProperty("timeout").toInt)
    connection.maxBodySize(connectionProperties.getProperty("max-body-size").toInt)
    val ref = connectionProperties.getProperty("referrer")
    if(!ref.isEmpty)connection.referrer(ref)

    connection.header("Host", connectionProperties.getProperty("host"))
    connection.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    connection.header("Connection", "keep-alive")
    connection.header("Accept-Encoding", "gzip, deflate, br")
    connection.header("Accept-Language", "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3")
    connection.header("Upgrade-Insecure-Requests", "1")
    connection.ignoreHttpErrors(true)
    connection.ignoreContentType(true)
    connection.method(Connection.Method.GET)

    connection.cookie(properties.getString("recipekey.cookie.name"), connectionProperties.getProperty("cookie"))

    var times = 0
    val maxTimes = connectionProperties.getProperty("attempts").toInt
    val delayDetection = connectionProperties.getProperty("delay-detection").toInt

    try{
      var continue = true
      var ret : Option[String] = null
      do{
        logger.debug(s"GET ${url}")
        val response = connection.execute()
        val errorCode = response.statusCode()
        logger.debug(s"Got HTTP Status ${errorCode}")
        errorCode match {
          case 200 =>
            continue = false
            ret = Some(response.body())
          case 301 =>
            var newURL = response.header("Location")
            if(!newURL.startsWith("http")) newURL = s"${newURL}${connectionProperties.getProperty("base-host")}"
            return requestRecipeKeyURL(newURL, connectionProperties)
          case 403 =>
            logger.error(s"Scraper detected!\nSleeping ${delayDetection}...")
            if(times == maxTimes){
              logger.error("Scraper detected again. Aborting program...")
              throw new ScrapingDetectionException(s"Scraped detected when trying to GET ${url}")
            }
            times += 1
            Thread.sleep(delayDetection)
          case _ =>
            continue = false
            ret = None
        }
      }while(continue)
      return ret
    } catch {
      case mue : MalformedURLException =>
        mue.printStackTrace()
        return None
      case ume : UnsupportedMimeTypeException =>
        ume.printStackTrace()
        return None
      case ste : SocketTimeoutException =>
        ste.printStackTrace()
        return None
      case ioe : IOException =>
        ioe.printStackTrace()
        return None
    }
  }

}
