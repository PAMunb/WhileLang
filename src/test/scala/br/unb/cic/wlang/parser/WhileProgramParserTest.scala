package br.unb.cic.wlang.parser

import br.unb.cic.wlang._
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class WhileProgramParserTest extends  AnyFunSuite with BeforeAndAfter {
  val p: WhileProgramParser = new WhileProgramParser()

  before {
    p.reset()
  }

  test("Test for the skip parser") {
    p.parse(p.skip, "skip") match {
      case p.Success(Skip(1), _) => succeed
      case p.Failure(msg,_) => println(s"FAILURE: $msg")
      case p.Error(msg,_) => println(s"ERROR: $msg")
    }
  }

  test("Test for the assignment parser") {
    p.parse(p.assignment, "x := 123") match {
      case p.Success(Assignment("x", Const(123), 1), _) => succeed
      case p.Failure(msg,_) => println(s"FAILURE: $msg")
      case p.Error(msg,_) => println(s"ERROR: $msg")
    }
  }

  test("Test for the sequence parser") {
    p.parse(p.sequenceStatement, "x := x + 1; x := y") match {
      case p.Success(c, _) => assert(c == Sequence(
        Assignment("x", Add(Variable("x"), Const(1)), 1),
        Assignment("x", Variable("y"), 2)))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail()
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail()
    }
  }

  test("Test for the conditional parser") {
    p.parse(p.conditional, "if(x < 10) then x := x + 1 else x := y endif") match {
      case p.Success(c, _) => assert(c == IfThenElse(Condition(LT(Variable("x"), Const(10)), 1),
        Assignment("x", Add(Variable("x"), Const(1)), 2),
        Assignment("x", Variable("y"), 3)))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail()
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail()
    }
  }

  test("Test for the repetition parser") {
    p.parse(p.repetition, "while(x < 10) do x := x + 1 end") match {
      case p.Success(c, _) => assert(c == While(Condition(LT(Variable("x"), Const(10)), 1),
        Assignment("x", Add(Variable("x"), Const(1)), 2)))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail()
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail()
    }
  }

  test("Test for the const parser") {
    p.parse(p.const, "123") match {
      case p.Success(Const(123), _) => succeed
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail()
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail()
    }
  }

  test("Test for a simple arithmetic expression") {
    p.parse(p.aExp, "123 + 456 * 3") match {
      case p.Success(exp, _) => assert(exp == Add(Const(123), Mult(Const(456), Const(3))))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail()
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail()
    }
  }

  test("Test for arithmetic expressions with brackets") {
    p.parse(p.aExp, "(123 + 456) / 3") match {
      case p.Success(exp, _) => assert(exp == Div(Add(Const(123), Const(456)), Const(3)))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail()
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail()
    }
  }

  test("Test for a simple LT expression") {
    p.parse(p.rel, "x < 0") match {
      case p.Success(exp, _) => assert(exp == LT(Variable("x"), Const(0)))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail()
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail()
    }
  }

  test("Test for non trivial boolean expressions") {
    p.parse(p.bExp, "x < 0 || (y > x + 1 && y < 100)") match {
      case p.Success(exp, _) => assert(exp == Or(LT(Variable("x"), Const(0)), And(GT(Variable("y"), Add(Variable("x"), Const(1))), LT(Variable("y"), Const(100)))))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }
  }

  test("Test for procedure call") {
    p.parse(p.statement, "fib(a - 1, 0,c)") match {
      case p.Success(call, _) => succeed
      case p.Failure(msg, _) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }
  }

  test("Test for the fibonacci module") {
    val content = ResourceHandle.getContent("Fibonacci.wp")

    assert(content != null)

    p.parse(p.whileProgram, content) match {
      case p.Success(program, _) => succeed
      case p.Failure(msg, _) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }
  }

  test("Test for the factorial module") {
    val content = ResourceHandle.getContent("Factorial.wp")

    assert(content != null)

    p.parse(p.whileProgram, content) match {
      case p.Success(program, _) => succeed
      case p.Failure(msg, _) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }
  }

  test("Test for the swapvars module") {
    val content = ResourceHandle.getContent("Swapvars.wp")

    assert(content != null)

    p.parse(p.whileProgram, content) match {
      case p.Success(program, _) => succeed
      case p.Failure(msg, _) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }
  }
}
