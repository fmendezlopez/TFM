package es.uam.eps.tfm.fmendezlopez.dao

import java.io.IOException
import java.sql.{Connection, DriverManager, SQLException}

import es.uam.eps.tfm.fmendezlopez.dao.DatabaseDAO.conn
import org.apache.commons.configuration2.Configuration
import org.apache.commons.text.StringEscapeUtils

/**
  * Created by franm on 01/08/2018.
  */
object DatasetSQLDAO {

  private var properties: Configuration = _
  private var conn: Connection = _

  //todo organizar properties versionandolo antes
  //todo acabar esta utilidad
  def initialize(properties: Configuration) = this.properties = properties

  @throws[InstantiationException]
  @throws[IllegalAccessException]
  @throws[ClassNotFoundException]
  @throws[SQLException]
  def connect(): Unit = {
    Class.forName(properties.getString("stage4.stage1.database.driver")).newInstance
    conn = DriverManager.getConnection(
      properties.getString("stage4.stage1.database.url"),
      properties.getString("stage4.stage1.database.username"),
      properties.getString("stage4.stage1.database.password")
    )
  }

  @throws[SQLException]
  @throws[IOException]
  def disconnect(): Unit = conn.close()

  @throws[SQLException]
  def insertData(tableName: String, data: Seq[String], string_fields: Seq[Int] = Seq()): Unit = {

      val query =
        s"""
          |INSERT INTO "$tableName"
          |VALUES (${
          data
          .zipWithIndex
            .map({case(column, index) =>
              if(string_fields.contains(index))
                s"""'${column.replace("'", "")}'"""
              else
                column})
            .mkString(",")})
        """.stripMargin
      val stmt = conn.createStatement
      try{
        stmt.execute(query)
        stmt.close()
      } catch {
        case e: SQLException =>
          println(e.getMessage)
      }

  }
}
