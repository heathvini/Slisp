object LispInterpreter {

  var env = new LispEvaluator.Env(LispEvaluator.builtin, null)
  def main(args: Array[String]) {
	  var input = ""
	  print("Slisp - Lisp interpreter in a Scala external DSL")
      while(input != "exit") {
        print("\n> ")
        input = readLine()
        input match {
          case "" => {}
          case "exit" => {}
          case _ => {
            var ast: List[Any] = null
            var result: Any = ""
            try {ast = LispParser.parse(input)} 
            catch {case e: Exception => print("Unable to parse expression: " + input)}
        	if (ast != null)
        	  try {result = LispEvaluator.eval(ast,env); print(pretty(result))} 
        	  catch {case e: Exception => print("Unable to evaluate expression: " + input)}
          }
        }
      }
  }
  
  def load(filename: String) {
    val source = scala.io.Source.fromFile(filename)
    var lines = source.mkString
    source.close()
    lines = "(eval " + lines.replace("\t","").replace("\r\n"," ").replace("\n"," ") + ")"
    var ast = LispParser.parse(lines)
    LispEvaluator.eval(ast,env)
  }
  
  def pretty(s: Any):String = {
    s match {
      case true => "t"
      case false => "nil"
      case s:List[Any] => {
        var result = "("
        for (i <- 0 to s.length-1) {
          if (i == 0) result += pretty(s(i))
          if (i > 0) result += " " + pretty(s(i))
        }
        return result + ")"
      }
      case Value(v) => pretty(v)
      case _ => s.toString
    }
  }
}