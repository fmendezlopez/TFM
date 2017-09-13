package es.uam.eps.tfm.fmendezlopez.utils

import java.io.File
import java.net.{InetAddress, UnknownHostException}
import java.util.StringTokenizer

import es.uam.eps.tfm.fmendezlopez.dto.Recipe
import org.apache.commons.configuration2.Configuration

/**
  * Created by Francisco on 14/04/2017.
  */
object Utils extends Logging{
  def getHostName(default : String): String = {
    try
    {
      val addr : InetAddress = InetAddress.getLocalHost()
      addr.getHostName()
    }
    catch{
      case uhe : UnknownHostException =>
        default
    }
  }
  def resolvePath(stageNumber : Int, hostname : String): String = {
    val prefix = s"${System.getProperty("user.dir")}${File.separator}Extractor${File.separator}output${File.separator}"
    val suffix = s"${File.separator}${hostname}"
    val infix = s"stage${stageNumber}"
    s"${prefix}${infix}${File.separator}${suffix}"
  }
  def resolvePath(stageNumber : Int, substageNumber : Int, hostname : String): String = {
    val prefix = s"${System.getProperty("user.dir")}${File.separator}Extractor${File.separator}output${File.separator}"
    val suffix = s"${File.separator}${hostname}"
    val infix = s"stage${stageNumber}"
    s"${prefix}${infix}${File.separator}${substageNumber}${suffix}"
  }

  def deduceRecipeID(prior_id : Long, weburl : String, apiurl : String) : Long = {
    var id : Long = -1
    if(Recipe.isValidRecipeID(prior_id)){
      id = prior_id
    }
    else{
      try{
        logger.warn(s"Found recipe with invalid id ${prior_id}")
        val try1 = Utils.extractRecipeIDFromWebURL(weburl)
        logger.debug(s"id extracted from web URL ${try1}")
        id = try1
      }
      catch {
        case nfe : NumberFormatException =>
          logger.error(nfe)
          logger.info("Trying to get recipe id from api URL...")
          try{
            val try2 = Utils.extractRecipeIDFromAPIURL(apiurl)
            logger.debug(s"id extracted from api URL ${try2}")
            try2
          }
          catch {
            case nfe: NumberFormatException =>
              logger.error(nfe)
              logger.info("Trying to deduce id from web URL...")
              val try3 = Utils.detectRecipeID(weburl)
              if (try3.isEmpty) {
                logger.info("Trying to deduce id from api URL...")
                val try4 = Utils.detectRecipeID(apiurl)
                if (try4.isEmpty) {
                  logger.warn("recipe id is assigned from timestamp")
                  id = System.currentTimeMillis()
                }
                else{
                  id = try4.get
                }
              }
              else{
                id = try3.get
              }
          }
      }
    }
    id
  }

  @throws(classOf[NumberFormatException])
  def extractRecipeIDFromWebURL(url : String) : Long = {
    if(url.isEmpty)
      throw new NumberFormatException("String is empty")
    val tokenizer = new StringTokenizer(url)
    tokenizer.nextToken(".")
    tokenizer.nextToken("/")
    tokenizer.nextToken("/")
    tokenizer.nextToken("/").toLong
  }

  @throws(classOf[NumberFormatException])
  def extractRecipeIDFromAPIURL(url : String) : Long = {
    if(url.isEmpty)
      throw new NumberFormatException("String is empty")
    val tokenizer = new StringTokenizer(url)
    tokenizer.nextToken(".")
    tokenizer.nextToken("/")
    tokenizer.nextToken("/")
    tokenizer.nextToken("/")
    tokenizer.nextToken("/").toLong
  }

  def detectRecipeID(url : String) : Option[Long] = {
    if(url.isEmpty)
      return None
    val tokenizer = new StringTokenizer(url)
    tokenizer.nextToken(".")
    var continue = true
    var number : Option[Long] = None
    do {
      val token = tokenizer.nextToken("/")

      try{
        number = Some(token.toLong)
      } catch{
        case nfe : NumberFormatException =>
          logger.error(nfe)
          number = None
      }
      continue = number.isEmpty && tokenizer.hasMoreTokens
    } while(continue)
    number
  }

  def flatten(ls: Seq[Any]): Seq[Any] = ls flatMap {
    case ms: Seq[_] => flatten(ms)
    case e => Seq(e)
  }

  def compoundRecipeURL(id : String, properties : Configuration) : String = {
    s"${properties.getString("allrecipes.url.base")}/${id}"
  }

  def getInputFiles(path : String) : Seq[File] = {
    val dir = new File(path)
    dir.listFiles().toSeq
  }

  def headerToSeq(header : String, delimiter : Char) : Seq[String] = {
    header.split(delimiter).toSeq
  }
}
