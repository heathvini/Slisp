object LispEvaluator {
  
  class Env(var inner:scala.collection.mutable.Map[String,Any],var outer:Env)
        {
                def set(v:String,e:Any) {inner(v) = e}
                def get(v:String):Any={
                        if(inner.contains(v))
                                inner(v)
                        else outer.get(v)
                }
        }
  
  var builtin=scala.collection.mutable.Map[String,Any](
                ("+",(a:Any,b:Any) =>(a,b) match {case (a:Int,b:Int) => a+b }),
                ("-",(a:Any,b:Any) =>(a,b) match {case (a:Int,b:Int) => a-b }),
                ("*",(a:Any,b:Any) =>(a,b) match {case (a:Int,b:Int) => a*b }),
                ("/",(a:Any,b:Any) =>(a,b) match {case (a:Int,b:Int) => a/b }),
                ("eq",(a:Any,b:Any) => a==b ),
                (">",(a:Any,b:Any) =>(a,b) match {case (a:Int,b:Int) => a>b }),
                ("<",(a:Any,b:Any) =>(a,b) match {case (a:Int,b:Int) => a<b }),
                (">=",(a:Any,b:Any) =>(a,b) match {case (a:Int,b:Int) => a>=b }),
                ("<=",(a:Any,b:Any) =>(a,b) match {case (a:Int,b:Int) => a<=b }),
                ("cons",(a:Any,b:List[Any]) => a::b),
                ("car",(a:List[Any]) => a.head),
                ("cdr",(a:List[Any]) => a.tail),
                ("atom",(a:Any) => a match {case l: List[Any] => false case _ => true})
  )
  
  def eval(exp: Any, env:Env):Any = {
	//println("Evaluating " + exp)
    var result = exp match {
      case v: Integer => v
      case v: String => v
      case v: Float => v
      case Value(v) => v
      case Variable(v) => env.get(v)
       
      case l: List[any] => l.head match {
        // Basic List manipulation
        case Variable("eval") => {var result: Any = null; for (x <- l.tail) result = eval(x,env); return result} // Maybe not working?
      	case Variable("list") => l.tail.map(x => eval(x,env))
      	
      	// Logic Operations
      	case Variable("if") => if(evalb(l(1),env)) eval(l(2),env) else eval(l(3),env)
      	case Variable("or") => if(evalb(l(1),env)) evalb(l(1),env) else evalb(l(2),env)
      	case Variable("and") => if(evalb(l(1),env)) evalb(l(2),env) else evalb(l(1),env)
      	
      	// Let
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
      	
      	// Defining Functions
      	case Variable("defun") => {
      	  val name = l(1) match {case Variable(f) => f}
      	  val args = l(2)
      	  val x = l(3)
      	  env.set(name,eval(List(Variable("lambda"),args,x),env))
      	}
      	
      	// Lambdas
      	case Variable("lambda") => {
      	  val args = l(1)
      	  val x = l(2)
      	  val inner = collection.mutable.Map[String, Any]()
      	  args match { 
      	    case List(Variable(arg1)) => (a:Any) => {inner.put(arg1,a); eval(x,new LispEvaluator.Env(inner,env))}
      	    case List(Variable(arg1), Variable(arg2)) => (a:Any,b:Any) => {inner.put(arg1,a); inner.put(arg2,b); eval(x,new LispEvaluator.Env(inner,env))}
      	    case List(Variable(arg1), Variable(arg2), Variable(arg3)) => (a:Any,b:Any,c:Any) => {inner.put(arg1,a); inner.put(arg2,b); inner.put(arg3,c); eval(x,new LispEvaluator.Env(inner,env))}
      	  }
      	}
      	
      	// Applying Functions
      	case _ => {
          val f = eval(l.head,env)
          f match {
            case f:List[any] => f
            case f:Function1[Any,Any] => f(eval(l(1),env))
          	case f:Function2[Any,Any,Any] => f(eval(l(1),env),eval(l(2),env))
          	case f:Function3[Any,Any,Any,Any] => f(eval(l(1),env),eval(l(2),env),eval(l(3),env))
          	case f:Int => f
          	case f:Float => f
          	case f:Boolean => f     
          }
        }   
      }
    }
    return result
  }
  
  def evalb(exp: Any, env:Env):Boolean = {
    var result = exp match {
      case Value(false) => false
      case false => false
      case Variable(v) => evalb(eval(v,env), env)
      case List(l: List[Any]) => evalb(l, env)
      case _ => true
    }
    return result
  }
}
