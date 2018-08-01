package es.uam.eps.tfm.fmendezlopez.dao

import java.io.IOException
import java.sql.{Connection, DriverManager, SQLException}

import es.uam.eps.tfm.fmendezlopez.dao.DatabaseDAO.conn
import org.apache.commons.configuration2.Configuration

/**
  * Created by franm on 01/08/2018.
  */
object DatasetSQLDAO {

  private var properties: Configuration = _
  private var conn: Connection = _

  //todo organizar properties versionandolo antes
  //todo acabar esta utilidad
  //todo quitar la extraccion de recetas de cada review y añadir la extraccion de reviews para cada receta
  def initialize(properties: Configuration) = this.properties = properties

  @throws[InstantiationException]
  @throws[IllegalAccessException]
  @throws[ClassNotFoundException]
  @throws[SQLException]
  def connect(): Unit = {
    Class.forName(properties.getString("")).newInstance
    conn = DriverManager.getConnection(properties.getString(""))
  }

  @throws[SQLException]
  @throws[IOException]
  def disconnect(): Unit = conn.close()
}
