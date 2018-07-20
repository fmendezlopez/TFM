package es.uam.eps.tfm.fmendezlopez.extraction

import java.io.File
import java.util.Properties

import es.uam.eps.tfm.fmendezlopez.dto.SeedDTO
import es.uam.eps.tfm.fmendezlopez.exception.ScrapingDetectionException
import es.uam.eps.tfm.fmendezlopez.scraping.Scraper
import es.uam.eps.tfm.fmendezlopez.utils.{CSVManager, HttpManager, Logging}
import org.apache.commons.configuration2.Configuration
import org.json.JSONObject

import scala.collection.immutable.{Queue, Stack}
import scala.collection.mutable
import scala.util.Try

/**
  * Created by franm on 24/06/2018.
  */
class SeedProvider(state: JSONObject, configurationPath: String, properties: Configuration) extends Logging{

  private val seedsFile = new File(state.getString("seedsFile"))
  private val seedsBuffer: mutable.Queue[SeedDTO] = mutable.Queue()
  private val csvDelimiter = properties.getString("stage4.stage1.output.csv.delimiter")
  private var lastLine: Int = state.getInt("lastLine")

  if(!seedsFile.exists() || !seedsFile.canRead) {
    logger.info(s"File ${seedsFile.getName} does not exist")
    requestSeeds()
  }

  @throws[ScrapingDetectionException]
  def nextSeed(): SeedDTO = {
    Try(seedsBuffer.dequeue)
      .getOrElse({
        Try({
          val csvReader = CSVManager.openCSVReader(seedsFile, csvDelimiter.charAt(0))
          CSVManager.skipLines(csvReader, lastLine + 1, (ret) => ret.nonEmpty)
          val lineOpt = csvReader.readNext()
          CSVManager.closeCSVReader(csvReader)
          lastLine += 1
          logger.info(s"Line $lastLine read")
          val seed = SeedDTO(lineOpt.get(0))
          seedsBuffer.enqueue(seed)
          seed
        })
          .getOrElse({
            requestSeeds()
            seedsBuffer.dequeue()
          })
        })
  }

  def hasMoreElements: Boolean = Try({nextSeed();true}).getOrElse(false)

  @throws[ScrapingDetectionException]
  private def requestSeeds(): Seq[SeedDTO] = {
    val htmlOpt = HttpManager.requestURL(properties.getString("allrecipes.url.base"))
    val seeds = Scraper.scrapeAuthorList(htmlOpt.getOrElse(""))
    seeds.map(author => {val seed = SeedDTO(author.id.toString);seedsBuffer.enqueue(seed);seed})
  }

  def getLastLine: Int = lastLine
}
