package com.cereopsis

import play.api.libs.json.{Json,Writes}

/*
 *   Problem: Rather than have to define a toJson method on each class
 *   find a way of mixing in this behaviour e.g with a stackable trait.
 *   What makes this slightly more interesting is that Play, for example,
 *   requires the use of implicits.
 */
trait JsonWritable[A] {
	this: A =>
	def toJson(implicit writer: Writes[A]): String = Json.toJson(this).toString
}

// Now all we need to do is define a case class and extend our trait

case class Car(make: String, model: String, yearsOfManufacture: List[Int]) extends JsonWritable[Car]

object Car {
  implicit val carwrites = Json.writes[Car]
}

val mustang = Car("Ford", "Mustang", (1964 to 2016).toList)

val serialised = mustang.toJson
