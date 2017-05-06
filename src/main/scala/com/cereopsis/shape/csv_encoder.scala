package com.cereopsis.shape

import shapeless._

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
	def pure[A](func: A => List[String]): CsvEncoder[A] =
	new CsvEncoder[A] {
	  def encode(value: A): List[String] =
	    func(value)
	}

	implicit val stringEnc: CsvEncoder[String] = pure(str => List(str))
	implicit val intEnc: CsvEncoder[Int] = pure(num => List(num.toString))
	implicit val doubleEnc: CsvEncoder[Double] = pure(num => List(num.toString))
	implicit val booleanEnc: CsvEncoder[Boolean] = pure(bool => List(if(bool) "yes" else "no"))
	
	/* Product */
	implicit val hnilEncoder: CsvEncoder[HNil] = pure(hnil => Nil)
	implicit def hlistEncoder[H, T <: HList](
	  implicit
	  hEncoder: Lazy[CsvEncoder[H]],
	  tEncoder: CsvEncoder[T]
	): CsvEncoder[H :: T] =
	     pure {
	       case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
	     }
	
	/* Co-Product */
	implicit val cnilEncoder: CsvEncoder[CNil] = 
		pure(cnil => throw new Exception("Inconceivable!"))
	
	implicit def coproductEncoder[H, T <: Coproduct](
	  implicit
	  hEncoder: Lazy[CsvEncoder[H]],
	  tEncoder: CsvEncoder[T]
	): CsvEncoder[H :+: T] = 
		pure {
		  case Inl(h) => hEncoder.value.encode(h)
		  case Inr(t) => tEncoder.encode(t)
		}
	
	/* Generic.Aux[A,R] is shorthand for: Generic[A] { type Repr = R } */
	implicit def genericEncoder[A, R](
	  implicit
	  gen: Generic.Aux[A, R],
	  env: Lazy[CsvEncoder[R]]
	): CsvEncoder[A] = 
		pure { a => 
			env.value.encode(gen.to(a))
		}
	
	def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
	   values.map(value => enc.encode(value).mkString(",")).mkString("\n") 
	
}

object Main extends App {
	
	case class Quaternion(x: Double, y: Double, z: Double, w: Double = 1.0)
	val quats = List(Quaternion(0.8, 0.67, 0.3), Quaternion(0.91,0.67,0.77))
	import CsvEncoder._
	
	println(CsvEncoder.writeCsv(quats))
}