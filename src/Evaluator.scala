object LispEvaluator {
  
  class Env(var inner:scala.collection.mutable.Map[String,Any],var outer:Env)
        {
                def set(v:String,e:Any) {
                	inner(v) = e
                	}
                def get(v:String):Any={
                    if(inner.contains(v))
                    	inner(v)
                        else outer.get(v)
                }
        }
  
  var builtin=scala.collection.mutable.Map[String,Any](
                ("+",(a:Any,b:Any) => (a,b) match {case (a:Int,b:Int) => a+b }),
                ("-",(a:Any,b:Any) => (a,b) match {case (a:Int,b:Int) => a-b }),
                ("*",(a:Any,b:Any) => (a,b) match {case (a:Int,b:Int) => a*b }),
                ("/",(a:Any,b:Any) => (a,b) match {case (a:Int,b:Int) => a/b }),
                ("eq",(a:Any,b:Any) => a==b ),
                (">",(a:Any,b:Any) => (a,b) match {case (a:Int,b:Int) => a>b }),
                ("<",(a:Any,b:Any) => (a,b) match {case (a:Int,b:Int) => a<b }),
                (">=",(a:Any,b:Any) => (a,b) match {case (a:Int,b:Int) => a>=b }),
                ("<=",(a:Any,b:Any) => (a,b) match {case (a:Int,b:Int) => a<=b }),
                ("cons",(a:Any,b:Any) => (a,b) match {case (a:List[Any],b:List[Any]) => List(a,b) case (a:Any,b:List[Any]) => a::b case (a:Any,false) => List(a) case _ => List(a,b)}),
                ("car",(a:Any) => a match {case a:List[Any] => a.head case _ => false}),
                ("cdr",(a:Any) => a match {case a:List[Any] => a.length match {case 1 => false case _ => a.tail} case _ => false}),
                ("atom",(a:Any) => a match {case l:List[Any] => false case _ => true})
  )
  
  def eval(exp: Any, env:Env):Any = {
	//println("Evaluating " + exp)
    var result = exp match {
      case v: Integer => v
      case v: String => v
      case v: Float => v
      case v: Boolean => v
      case Value("t") => true
      case Value("nil") => false
      case Value(v) => v
      case Variable(v) => env.get(v)
      case () => ()
      
      case l: List[any] => l.head match {
        // Basic List manipulation
        case Variable("eval") => {
          var result: Any = null; for (x <- l.tail) result = eval(eval(x,env),env); result} // Maybe not working?
      	case Variable("list") => l.tail.map(x => eval(x,env))
      	case Variable("print") => println(eval(l(1),env))
      	
      	// Logic Operations
      	case Variable("if") => if(evalb(l(1),env)) eval(l(2),env) else eval(l(3),env)
      	case Variable("or") => if(evalb(l(1),env)) evalb(l(1),env) else evalb(l(2),env)
      	case Variable("and") => if(evalb(l(1),env)) evalb(l(2),env) else evalb(l(1),env)
      	//case Variable("cond") => while 
      	
      	// Let / Set
      	case Variable("let") => {
      	  val args = l(1)
      	  val inner = collection.mutable.Map[String, Any]()
      	  args match {
      	    case argList: List[List[Any]] => {
      	      for (argPair <- argList) {
      	        argPair(0) match {case Variable(x: String) => inner.put(x,eval(argPair(1),env))}
      	      }
      	    }
      	  }
      	  return eval(l(2),new LispEvaluator.Env(inner,env))
      	}
      	case Variable("set") => l(1) match {case Variable(v) => env.set(v, eval(l(2),env))}
        
      	// Import
      	case Variable("import") => {
      	  val lib = eval(l(1),env) match {case Variable(f) => f}
      	  val filename = "C:/Users/Heath/code/eclipse-4.3.1/Slisp/lib/" + lib + ".lsp"
      	  LispInterpreter.load(filename)
      	  "Library " + lib + " successfully imported"
      	}
      	
      	// Defining Functions
      	case Variable("defun") => {
      	  val name = l(1) match {case Variable(f) => f}
      	  val args = l(2)
      	  val x = l(3)
      	  env.set(name,List(Variable("lambda"),args,x))
      	}
      	
      	// Lambdas
      	case Variable("lambda") => {
      	  val args = l(1)
      	  val x = l(2)
      	  val innerEnv = new LispEvaluator.Env(collection.mutable.Map[String, Any](),env)
      	  args match { 
      	    case List() => (a:Unit) => {eval(x,innerEnv)}
      	    case List(Variable(arg1)) => (a:Any) => {innerEnv.set(arg1,a); eval(x,innerEnv)}
      	    case List(Variable(arg1), Variable(arg2)) => (a:Any,b:Any) => {innerEnv.set(arg1,a); innerEnv.set(arg2,b); eval(x,innerEnv)}
      	    case List(Variable(arg1), Variable(arg2), Variable(arg3)) => (a:Any,b:Any,c:Any) => {innerEnv.set(arg1,a); innerEnv.set(arg2,b); innerEnv.set(arg3,c); eval(x,innerEnv)}
      	  }
      	}
      	
      	// Applying Functions
      	case _ => {
          var f = eval(l.head,env)
          if (f match {case List(Variable("lambda"),args:Any,x:Any) => true case _ => false})
            f = eval(f,env)
          f match {
            case f:List[any] => f
            case f:Function0[Any] => f()
            case f:Function1[Any,Any] => f(eval(l(1),env))
          	case f:Function2[Any,Any,Any] => f(eval(l(1),env),eval(l(2),env))
          	case f:Function3[Any,Any,Any,Any] => f(eval(l(1),env),eval(l(2),env),eval(l(3),env))
          	case f:Int => f
          	case f:Float => f
          	case f:Boolean => f
          	case _ => f // Used for functions with only side effects
          }
        }   
      }
    }
    //println("Evaluated " + exp + " to be " + result)
    return result
  }
  
  def evalb(exp: Any, env:Env):Boolean = {
    var result = eval(exp,env) match {
      case Value(false) => false
      case false => false
      case Variable(v) => evalb(eval(v,env), env)
      case List(l: List[Any]) => evalb(l, env)
      case _ => true
    }
    return result
  }
}
