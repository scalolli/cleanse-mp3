/**
 * Created with IntelliJ IDEA.
 * User: basu
 * Date: 30/12/12
 * Time: 11:24 AM
 * To change this template use File | Settings | File Templates.
 */


trait Function {
  def name: String
}

case class ShellFunction(name: String) extends Function
case class ScalaFunction(name: String) extends Function

val l: List[Option[Function]] = List(None, Some(ShellFunction("build")), None)

val resultUsingForConstruct = for (
  i <- l;
  j <- i
) yield (Some(ScalaFunction(j.name)))

val resultUsingFlatMap = l flatMap {
  i: Option[Function] => i map {
    j: Function => Some(ScalaFunction(j.name))
  }
}

println("Result using for construct: " + resultUsingForConstruct)
println("Result using flatmap construct: " + resultUsingFlatMap)