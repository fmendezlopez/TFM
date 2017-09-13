package es.uam.eps.tfm.fmendezlopez.utils

import java.io.File

import org.apache.commons.configuration2.{Configuration, FileBasedConfiguration, PropertiesConfiguration}
import org.apache.commons.configuration2.builder.FileBasedConfigurationBuilder
import org.apache.commons.configuration2.builder.fluent.Parameters

/**
  * Created by Francisco on 09/04/2017.
  */
object PropertiesManager {

  val EXTRACTION_PROPERTIES_FILE = "extraction.properties"

  def loadProperties(configPath : String, fileName : String) : Configuration = {
    var completePath = configPath
    val conf = new File(configPath)
    if(!conf.isAbsolute()){
      completePath = conf.getAbsolutePath();
    }
    val separator = if(completePath.charAt(completePath.length() - 1) == File.separatorChar) "" else File.separator
    val propertiesPath = completePath + separator + fileName
    val params : Parameters = new Parameters()
    val builder : FileBasedConfigurationBuilder[FileBasedConfiguration] =
      new FileBasedConfigurationBuilder[FileBasedConfiguration](classOf[PropertiesConfiguration])
        .configure(params.properties()
          .setFileName(propertiesPath))
    builder.getConfiguration()
  }
}
