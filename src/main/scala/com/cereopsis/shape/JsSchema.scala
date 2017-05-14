package com.cereopsis

import play.api.libs.json._

/*
 *  Parse a JSON structure like this...
 *
 *	{
 *		"blah": {
 *			"url": "/x/:id/y/:id/z",
 *			"methods": ["post", "get"],
 *			"schema": "blah",
 *			"queue": {
 *				"default": 404,
 *				"blah": "/queue/some-queue-blah"
 *			}
 *		}, {
 *        ...
 *.     }
 *	}
 *
 *  ...and convert it to a csv format like this
 *
 *  <Queue>, <Schema>, <URL>, <Methods>
 */


object JsSchema extends App {
	
	/*
	 * Note: We don't have to cover our entire structure here. Anything we're
	 * not interested in will be silently dropped.
	 */
	case class Queue(b2b: String)
	case class API(schema: String, url: String, methods: List[String], queue: Queue)
	object API {
		/*
		 * Declare the Reads inside the companion object. A strange null pointer
		 * occurred with no companion and the implicits in the outer scope. I can
		 * only assume they weren't being imported into the main function.
		 */ 
		implicit val queueReads = Json.reads[Queue]
		implicit val apiReads   = Json.reads[API]
	}

	
	override def main(args: Array[String]) {
		val output = 
			 args.toList
         .flatMap(xs => readFile(xs))
				 .flatMap(o  => o.asOpt[JsObject])
				 .flatMap(o => apis(o))
				 /*
				  * Let the implicit json conversion take care
				  * of filtering stuff out that we might not want
				  * e.g no 'b2b' queue
				  */
				 .flatMap(o => o.asOpt[API])
		
		output.foreach { api =>
			/*
			 * We could use Shapeless here to employ a generic csv encoder...
			 */
			val methods = api.methods.sorted.mkString(" ")
			println(s"${api.queue.b2b},${api.schema},${api.url},${methods}")
		}
	}
	
	/*
	 *  Extracts the top level objects e.g standardAutoAttendants
	 * in the example above.
	 */
	def apis(obj: JsObject): List[JsObject] =
		obj.keys.flatMap(key => (obj \ key).asOpt[JsObject]).toList
	
	/*
	 * Here's a classic example of not letting the langauge take care
	 * of the heavy lifting. I was pattern matching on the Try and
	 * returning Some/None when the obvious thing to do is toOption.
	 */
	def readFile(path: String): Option[JsValue] = {
		import scala.util.{Try,Success,Failure}
		import java.io.FileInputStream
		Try(Json.parse(new FileInputStream(path))).toOption
	}
}
