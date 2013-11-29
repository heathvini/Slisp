import scala.util.parsing.combinator._

case class Value(v: Any)
case class Variable(v: String)

object LispParser extends JavaTokenParsers {
  def parse(line: String) = parseAll(list, line).get

  // grammar
  def program: Parser[List[Any]] = rep(exp)
  def list: Parser[List[Any]] = "("~>rep(exp)<~")"
  def exp: Parser[Any] = ( 
      integer
    | "t" ^^ (b => Value(true))
    | "nil" ^^ (b => Value(false))
    | real
    | quote
    | literal
    | list
    )
  def integer: Parser[Value] = wholeNumber ^^ (n => Value(n.toInt))
  def real: Parser[Value] = floatingPointNumber  ^^ (d => Value(d.toDouble))
  def quote = "'"~>exp ^^ (e => Value(e))
  def literal: Parser[Variable] = """[^() ]+""".r ^^ (t => Variable(t.toString))
  
}