package es.uam.eps.tfm.fmendezlopez.dao

import es.uam.eps.tfm.fmendezlopez.dto.UserRecipe
import org.apache.commons.io.FileUtils
import java.io.File
import java.io.IOException
import java.sql._

import scala.util.{Failure, Success, Try}

object DatabaseDAO{
  private val DBNAME = "EXTRACTION_DB"
  private val USERS_TABLENAME = "visited_users"
  private val RECIPES_TABLENAME = "visited_recipes"
  private val REVIEWS_TABLENAME = "visited_reviews"
  private val FRONTIER_TABLENAME = "frontier"
  private val CLASSNAME = "org.apache.derby.jdbc.EmbeddedDriver"
  private val DBURL_CREATE = "jdbc:derby:" + DBNAME + ";create=true"
  private val DBURL = "jdbc:derby:" + DBNAME + ""

  private var conn: Connection = null

  def existDB: Boolean = {
    Try{
      connect()
    } match{
      case Failure(_) => false
      case Success(_) => true
    }
  }

  @throws[InstantiationException]
  @throws[IllegalAccessException]
  @throws[ClassNotFoundException]
  @throws[SQLException]
  def connectAndCreate(): Unit = {
    Class.forName(DatabaseDAO.CLASSNAME).newInstance
    conn = DriverManager.getConnection(DatabaseDAO.DBURL_CREATE)
  }

  @throws[InstantiationException]
  @throws[IllegalAccessException]
  @throws[ClassNotFoundException]
  @throws[SQLException]
  def connect(): Unit = {
    Class.forName(DatabaseDAO.CLASSNAME).newInstance
    conn = DriverManager.getConnection(DatabaseDAO.DBURL)
  }

  @throws[SQLException]
  @throws[IOException]
  def disconnect(): Unit = conn.close()

  @throws[SQLException]
  @throws[IOException]
  def disconnectAndDrop(): Unit = {
    dropDatabase()
    conn.close()
  }

  @throws[IOException]
  private def dropDatabase() = {
    val path = System.getProperty("user.dir") + File.separator + DatabaseDAO.DBNAME
    val db = new File(path)
    FileUtils.deleteDirectory(db)
  }

  @throws[SQLException]
  private def createUsersTable() = {
    val query =
      s"""
         |CREATE TABLE ${DatabaseDAO.USERS_TABLENAME}
         |(id BIGINT PRIMARY KEY,
         |queue SMALLINT,
         |priority INT)
      """.stripMargin
    val stmt = conn.createStatement
    stmt.execute(query)
    stmt.close()
  }

  @throws[SQLException]
  private def createRecipesTable() = {
    val query =
      s"""
         |CREATE TABLE ${DatabaseDAO.RECIPES_TABLENAME}
         |(id BIGINT PRIMARY KEY)
      """.stripMargin
    val stmt = conn.createStatement
    stmt.execute(query)
    stmt.close()
  }

  @throws[SQLException]
  private def createReviewsTable() = {
    val query =
      s"""
         |CREATE TABLE ${DatabaseDAO.REVIEWS_TABLENAME}
         |(id BIGINT PRIMARY KEY)
      """.stripMargin
    val stmt = conn.createStatement
    stmt.execute(query)
    stmt.close()
  }

  @throws[SQLException]
  def configure(): Unit = {
    createUsersTable()
    createRecipesTable()
    createReviewsTable()
  }

  @throws[SQLException]
  def insertUser(id: Long, queue: Boolean, priority: Int = -1): Unit = {
    val query =
      s"""
         |INSERT INTO ${DatabaseDAO.USERS_TABLENAME}
         |VALUES ($id, ${if(queue) 1 else 0}, $priority)
      """.stripMargin
    val stmt = conn.createStatement
    stmt.execute(query)
    stmt.close()
  }

  @throws[SQLException]
  def getUsers(priority: Int): Seq[Long]= {
    val select_query =
      s"""
         |SELECT id FROM ${DatabaseDAO.USERS_TABLENAME}
         |WHERE priority = $priority
         |AND queue = 1
      """.stripMargin

    val stmt = conn.createStatement
    stmt.executeQuery(select_query)
    val users = stmt.getResultSet
    var ret: Seq[Long] = Seq()
    while(users.next()) ret :+= users.getInt(1).toLong
    stmt.close()
    ret
  }

  @throws[SQLException]
  def getHeadPriority: Int = {
    val query1 =
      s"""
        |SELECT COUNT(*) as count
        |FROM $USERS_TABLENAME
        |WHERE queue = 1
      """.stripMargin

    val stmt = conn.createStatement
    val result = stmt.executeQuery(query1)
    result.next()
    val count = result.getInt(1)
    result.close()
    stmt.close()
    if(count > 0){
      val query2 =
        s"""
           |SELECT MIN(priority) as p
           |FROM $USERS_TABLENAME
           |WHERE queue = 1
      """.stripMargin

      val stmt = conn.createStatement
      val result = stmt.executeQuery(query2)
      result.next()
      val number = result.getInt(1)
      result.close()
      stmt.close()
      number
    }
    else 0

  }

  @throws[SQLException]
  def printUsers: Unit= {
    val select_query =
      s"""
         |SELECT id FROM ${DatabaseDAO.USERS_TABLENAME}
      """.stripMargin

    val stmt = conn.createStatement
    stmt.executeQuery(select_query)
    val users = stmt.getResultSet
    var ret: Seq[Long] = Seq()
    while(users.next()) ret :+= users.getInt(1).toLong
    ret.foreach(println)
    stmt.close()
    ret
  }

  @throws[SQLException]
  def emptyQueue: Boolean = {
    val select_query =
      s"""
         |SELECT COUNT(*) AS number
         |FROM ${DatabaseDAO.USERS_TABLENAME}
         |WHERE queue = 1
      """.stripMargin

    val stmt = conn.createStatement
    val result = stmt.executeQuery(select_query)
    result.next()
    val number = result.getInt(1)
    result.close()
    stmt.close()
    number == 0
  }

  @throws[SQLException]
  def extractUser(id: Long): Boolean = {
    val update_query =
      s"""
         |UPDATE ${DatabaseDAO.USERS_TABLENAME}
         |SET queue = 0
         |WHERE id = $id
      """.stripMargin

    val stmt = conn.createStatement
    val ret = stmt.executeUpdate(update_query)
    stmt.close()
    ret == 1
  }

  @throws[SQLException]
  def insertRecipe(id: Long): Unit = {
    val query =
      s"""
         |INSERT INTO ${DatabaseDAO.RECIPES_TABLENAME}
         |VALUES ($id)
      """.stripMargin
    val stmt = conn.createStatement
    stmt.execute(query)
    stmt.close()
  }

  @throws[SQLException]
  def insertReview(id: Long): Unit = {
    val query =
      s"""
         |INSERT INTO ${DatabaseDAO.REVIEWS_TABLENAME}
         |VALUES ($id)
      """.stripMargin
    val stmt = conn.createStatement
    stmt.execute(query)
    stmt.close()
  }

  @throws[SQLException]
  def existsUser(id: Long): Boolean = {
    val query =
      s"""
         |SELECT COUNT(*) as number
         |FROM ${DatabaseDAO.USERS_TABLENAME}
         |WHERE id = $id
      """.stripMargin
    val stmt = conn.createStatement
    val result = stmt.executeQuery(query)
    result.next
    val number = result.getInt(1)
    result.close()
    stmt.close()
    number != 0
  }

  @throws[SQLException]
  def isUserProcessed(id: Long): Boolean = {
    val query =
      s"""
         |SELECT COUNT(*) as number
         |FROM ${DatabaseDAO.USERS_TABLENAME}
         |WHERE id = $id
         |AND queue = 0
      """.stripMargin
    val stmt = conn.createStatement
    val result = stmt.executeQuery(query)
    result.next
    val number = result.getInt(1)
    result.close()
    stmt.close()
    number != 0
  }

  @throws[SQLException]
  def existsRecipe(id: Long): Boolean = {
    val query =
      s"""
         |SELECT COUNT(*) as number
         |FROM ${DatabaseDAO.RECIPES_TABLENAME}
         |WHERE id = $id
      """.stripMargin
    val stmt = conn.createStatement
    val result = stmt.executeQuery(query)
    result.next
    val number = result.getInt(1)
    result.close()
    stmt.close()
    number != 0
  }

  @throws[SQLException]
  def existsReview(id: Long): Boolean = {
    val query =
      s"""
         |SELECT COUNT(*) as number
         |FROM ${DatabaseDAO.RECIPES_TABLENAME}
         |WHERE id = $id
       """.stripMargin
    val stmt = conn.createStatement
    val result = stmt.executeQuery(query)
    result.next
    val number = result.getInt(1)
    result.close()
    stmt.close()
    number != 0
  }

  @throws[SQLException]
  def countUsers: Int = {
    val query =
      s"""
         |SELECT COUNT(*) as number FROM ${DatabaseDAO.USERS_TABLENAME}
         |WHERE queue = 0
      """.stripMargin
    val stmt = conn.createStatement
    val result = stmt.executeQuery(query)
    result.next
    val number = result.getInt(1)
    result.close()
    stmt.close()
    number
  }

  @throws[SQLException]
  def countRecipes: Int = {
    val query =
      s"""
         |SELECT COUNT(*) as number FROM ${DatabaseDAO.RECIPES_TABLENAME}
       """.stripMargin
    val stmt = conn.createStatement
    val result = stmt.executeQuery(query)
    result.next
    val number = result.getInt(1)
    result.close()
    stmt.close()
    number
  }

  @throws[SQLException]
  def countReviews: Int = {
    val query =
      s"""
         |SELECT COUNT(*) as number FROM ${DatabaseDAO.REVIEWS_TABLENAME}
       """.stripMargin
    val stmt = conn.createStatement
    val result = stmt.executeQuery(query)
    result.next
    val number = result.getInt(1)
    result.close()
    stmt.close()
    number
  }
}