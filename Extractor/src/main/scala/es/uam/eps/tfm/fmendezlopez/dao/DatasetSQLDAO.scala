package es.uam.eps.tfm.fmendezlopez.dao

import java.io.IOException
import java.sql.{Connection, DriverManager, SQLException, SQLType}

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

  /*
  @throws[SQLException]
  def insertData(tableName: String, data: Seq[String], string_fields: Seq[Int] = Seq()): Unit = {
    var a = data
      .zipWithIndex
      .map({case(column, index) =>
        if(string_fields.contains(index)) {
          s"""'${column.replace("'", "")}'"""
        }
        else column})
      .mkString(",")
    val query =
      s"""
        |INSERT INTO "$tableName"
        |VALUES (${a})
      """.stripMargin
    val stmt = conn.createStatement
    try{
      stmt.execute(query)
      stmt.close()
    } catch {
      case e: SQLException =>
        println(s"a: $a")
        println(query)
        println(e.getMessage)
        println(e)
        println(e.getErrorCode)
        println(e.getSQLState)
        System.exit(1)
    }
  }
  */

  @throws[SQLException]
  def insertData(tableName: String, data: Seq[Seq[String]], string_fields: Seq[Int] = Seq()): Unit = {
    val b = data.head.length
    val a = Seq.fill(b)("?") mkString(",")
    val query =
      s"""
         |INSERT INTO "$tableName"
         |VALUES (${a})
      """.stripMargin
    val stmt = conn.prepareStatement(query)
    data
      .foreach(seq => {
        val value =
          seq
          .zipWithIndex
          .map({case(column, index) =>
              val col =
                if(string_fields.contains(index)) {
                  s"""'${column.replace("'", "")}'"""
                }
                else column
            (index, col)
          })
          //.mkString(",")
        try{
          value.foreach({case(index, col) => stmt.setObject(index + 1, col)})
          stmt.executeUpdate()
        } catch {
          case e: SQLException =>
            println(s"value: $value")
            println(query)
            println(e.getMessage)
            println(e)
            println(e.getErrorCode)
            println(e.getSQLState)
            stmt.close()
            conn.close()
            System.exit(1)
        }
      })
    stmt.close()
  }

  @throws[SQLException]
  def beginTransaction(): Unit = conn.setAutoCommit(false)

  @throws[SQLException]
  def commit(): Unit = conn.commit()

  @throws[SQLException]
  def rollback(): Unit = conn.rollback()

  @throws[SQLException]
  def endTransaction(): Unit = conn.setAutoCommit(true)

  def getConnection : Connection = conn
}
