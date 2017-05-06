package com.cereopsis.shape

import org.scalatest.{FlatSpec,MustMatchers}

class ShapeTest extends FlatSpec with MustMatchers {
	
	"A generic CsvEncoder" should "be able to encode any case class (a product type)" in {
		case class Movie(title: String, year: Int, rating: String)
		// TODO: check the movie dates :-)
		val movies = List(Movie("The Shining", 1978, "R"), Movie("Emmanuel", 1977, "18"))
		
		import CsvEncoder._
		val csv = CsvEncoder.writeCsv(movies)
		csv must equal("""The Shining,1978,R
			             |Emmanuel,1977,18""".stripMargin)
		
	}
	
	it should("""handle nested classes""") in {
		case class Fullname(first: String, last: String)
		case class Person(name: Fullname, age: Int, employed: Boolean)
		
		val people = List(Person(Fullname("John","Walker"),53,true),Person(Fullname("Renny","Walker"),15,false))
		val csv = CsvEncoder.writeCsv(people)
		csv must equal("""John,Walker,53,yes
			              |Renny,Walker,15,no""".stripMargin)
	}
	
	"Co-product types" should "work even if they have different numbers of fields" in {
		sealed trait Coords
		final case class TwoD(x: Int, y: Int) extends Coords
		final case class ThreeD(x: Int, y: Int, z: Int) extends Coords
		val points: List[Coords] = List(TwoD(1,2),ThreeD(3,4,5))
		val csv = CsvEncoder.writeCsv(points)
		csv must equal("""1,2
			             |3,4,5""".stripMargin)
	}
}