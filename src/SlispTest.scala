object ParserTest {
  
	def main(args:Array[String]) {
		println(LispParser.parse("(if 1 (if 2 0 1) 0)"))
		println(LispParser.parse("('abc 'b 'c)"))
		println(LispParser.parse("(1.2 1.4)"))
		println(LispParser.parse("(nil)"))
	}
}

object EvaluatorTest {
  
	def main(args:Array[String]) {
	 
	    assertResult(1,eval("(eval 1)"))
	    
		assertResult(List(1,2,3),eval("(list 1 2 3)"))
		assertResult(List(0,1,2,3),eval("(cons 0 (list 1 2 3))"))
		assertResult(false,eval("(atom (list 1 2 3))"))
		assertResult(false,eval("(atom (list 1))"))
		assertResult(true,eval("(atom 1)"))
		assertResult(1,eval("(car (list 1 2 3))"))
		assertResult(1,eval("(car (list 1))"))
		assertResult(List(2,3),eval("(cdr (list 1 2 3))"))
		assertResult(List(),eval("(cdr (list 1))"))
		
		assertResult(2,eval("(if nil 1 2)"))
		assertResult(1,eval("(if t 1 2)"))
		assertResult(3,eval("(if (list 3 4) 3 4)"))
		assertResult(true,eval("(eq 1 1)"))
		assertResult(false,eval("(eq 1 2)"))
		assertResult(true,eval("(eq (list 1 2 3) (list 1 2 3))"))
		assertResult(false,eval("(eq (list 1 2 3) (list 1 2 3 4))"))
		assertResult(true,eval("(or nil 1)"))
		assertResult(true,eval("(or t 1)"))
		assertResult(true,eval("(or 1 2)"))
		assertResult(true,eval("(or 1 nil)"))
		assertResult(false,eval("(or nil nil)"))
		assertResult(false,eval("(and nil 1)"))
		assertResult(true,eval("(and t 1)"))
		assertResult(true,eval("(and 1 2)"))
		assertResult(false,eval("(and 1 nil)"))
		assertResult(false,eval("(and nil nil)"))
		
		assertResult(5,eval("(+ 2 3)"))
		assertResult(3,eval("(let ((a 1)(b 2)) (+ a b))"))
		assertResult(10,eval("(let ((a 10)(b 2)) (let ((a 20)) (/ a b)))"))
		assertResult(16,eval("((lambda (a) (+ a 3)) 13)"))
		assertResult(15,eval("((lambda (a b) (* a b)) 5 3)"))
		assertResult(15,eval("(eval (defun f (a b) (* a b)) (f 5 3))"))
		println("Testing complete")
	}
	
	def eval(s: String):Any = {
	  return LispEvaluator.eval(LispParser.parse(s), new LispEvaluator.Env(LispEvaluator.builtin, null))
	}
	
	def assertResult(result:Any,exp:Any) {
	  assert(exp == result, "Expected " + result + ", but got " + exp + ".")
	}
}