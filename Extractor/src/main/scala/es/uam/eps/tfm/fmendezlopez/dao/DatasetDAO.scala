package es.uam.eps.tfm.fmendezlopez.dao

import java.io.File
import java.util.Properties

import com.github.tototoshi.csv.CSVWriter
import es.uam.eps.tfm.fmendezlopez.dto.{Recipe, Review, User}
import es.uam.eps.tfm.fmendezlopez.utils.{CSVManager, Utils}
import org.apache.commons.configuration2.Configuration

/**
  * Created by franm on 10/06/2018.
  */
object DatasetDAO {

  private var writers: Map[String, CSVWriter] = _

  def initialize(properties: Configuration): Unit = {
    val prop: String => String = str => properties.getString(str)
    val csvDelimiter = prop("stage4.stage1.output.csv.delimiter")
    val hostname = Utils.getHostName(properties.getString("general.extraction.default.hostname"))
    val outputPath = Utils.resolvePath("AllrecipesExtractor", hostname)
    val fromScratch = !new File(outputPath).exists()
    writers = Map(
      "favourites" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.favourites.filename"), csvDelimiter.charAt(0)),
      "fellowship" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.fellowship.filename"), csvDelimiter.charAt(0)),
      "ingredients" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.ingredients.filename"), csvDelimiter.charAt(0)),
      "madeit" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.madeit.filename"), csvDelimiter.charAt(0)),
      "nutrition" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.nutrition.filename"), csvDelimiter.charAt(0)),
      "publications" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.publications.filename"), csvDelimiter.charAt(0)),
      "recipes" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.recipes.filename"), csvDelimiter.charAt(0)),
      "reviews" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.reviews.filename"), csvDelimiter.charAt(0)),
      "similar" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.similar.filename"), csvDelimiter.charAt(0)),
      "steps" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.steps.filename"), csvDelimiter.charAt(0)),
      "users" -> CSVManager.openCSVWriter(outputPath, prop("stage4.stage1.dataset.users.filename"), csvDelimiter.charAt(0))
    )

    if(fromScratch) {
      writers("favourites").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.favourites.header"), csvDelimiter.charAt(0)))
      writers("fellowship").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.fellowship.header"), csvDelimiter.charAt(0)))
      writers("ingredients").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.ingredients.header"), csvDelimiter.charAt(0)))
      writers("madeit").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.madeit.header"), csvDelimiter.charAt(0)))
      writers("nutrition").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.nutrition.header"), csvDelimiter.charAt(0)))
      writers("publications").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.publications.header"), csvDelimiter.charAt(0)))
      writers("recipes").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.recipes.header"), csvDelimiter.charAt(0)))
      writers("reviews").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.reviews.header"), csvDelimiter.charAt(0)))
      writers("similar").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.similar.header"), csvDelimiter.charAt(0)))
      writers("steps").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.steps.header"), csvDelimiter.charAt(0)))
      writers("users").writeRow(Utils.headerToSeq(prop("stage4.stage1.dataset.users.header"), csvDelimiter.charAt(0)))
    }
  }

  def close(): Unit = {
    writers.values.foreach(_.close())
  }

  def addRecipes(recipes: Seq[Recipe]): Unit = {
    recipes foreach {recipe =>
      writers("recipes").writeRow(Utils.flatten(Seq(recipe.toSeq())))
      recipe.ingredients.foreach(ingredient => writers("ingredients").writeRow(Utils.flatten(Seq(recipe.id, ingredient.toSeq()))))
      recipe.steps.foreach({ case (number, text) => writers("steps").writeRow(Seq(recipe.id, number, text)) })
      if (recipe.nutritionInfo.isDefined)
        writers("nutrition").writeRow(Utils.flatten(Seq(recipe.id, recipe.nutritionInfo.get.toSeq())))
      writers("similar").writeAll(recipes.map(similar => Seq(similar.id, recipe.id)))
    }
  }

  def addFavourites(userID: Long, recipes: Seq[Long]): Unit = {
    writers("favourites").writeAll(recipes.map(id => Seq(id, userID)))
  }

  def addMadeIt(userID: Long, recipes: Seq[Long]): Unit = {
    writers("madeit").writeAll(recipes.map(id => Seq(id, userID)))
  }

  def addPublications(userID: Long, recipes: Seq[Long]): Unit = {
    writers("publications").writeAll(recipes.map(id => Seq(id, userID)))
  }

  def addReviews(reviews: Seq[Review]): Unit = {
    writers("reviews").writeAll(reviews.map(review => review.toSeq()))
  }

  def addUser(user: User): Unit = {
    writers("users").writeRow(user.toSeq)
  }

  def addFollowers(user: User, users: Seq[Long]): Unit = {
    writers("fellowship").writeAll(users.map(Seq(_, user.id)))
  }

  def addFollowing(user: User, users: Seq[Long]): Unit = {
    writers("fellowship").writeAll(users.map(Seq(user.id, _)))
  }
}
