package es.uam.eps.tfm.fmendezlopez.utils

import java.io._

import org.json.{JSONObject, JSONTokener}

/**
  * Created by Francisco on 12/04/2017.
  */
object JSONManager {

  def writeJSON(json: JSONObject, path: String): Unit = {
    val pathFile = new File(path)
    if(!pathFile.exists()){
      pathFile.createNewFile()
    }
    val fos = new FileOutputStream(path)
    val osw = new OutputStreamWriter(fos)
    val bw = new BufferedWriter(osw)
    bw.write(json.toString)
    bw.close()
  }
  def openJSONWriter(path:String, name:String) : BufferedWriter = {
    val pathFile = new File(path)
    if(!pathFile.exists()){
      pathFile.mkdirs()
    }
    val file = s"${pathFile}${File.separator}${name}"
    val fos = new FileOutputStream(file)
    val osw = new OutputStreamWriter(fos)
    new BufferedWriter(osw)
  }
  def closeJSONWriter(bufferedWriter: BufferedWriter): Unit ={
    bufferedWriter.close()
  }
  def jsonFromFile(name : String): JSONObject ={
    val pathFile = new File(name)
    val fis = new FileInputStream(pathFile)
    val isr = new InputStreamReader(fis)
    val br = new BufferedReader(isr)
    new JSONObject(new JSONTokener(br))
  }
}
