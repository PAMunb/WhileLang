package br.unb.cic.wlang.semantics

import br.unb.cic.wlang.WhileProgram
import br.unb.cic.wlang.parser.{ResourceHandle, WhileProgramParser}
import org.scalatest.funsuite.AnyFunSuite

class StructuralSemanticsTest extends AnyFunSuite {


  test("Test for the sum program") {
    val content = ResourceHandle.getContent("Sum.wp")

    assert(content != null)

    val p: WhileProgramParser = new WhileProgramParser()

    val wp: WhileProgram = p.parse(p.whileProgram, content) match {
      case p.Success(program: WhileProgram, _) => program
      case p.Failure(msg, _) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }

    assert(wp != null)

    val interpreter = new StructuralSemantics()

    val res = interpreter.run(wp)

    println(res)
  }

  test("Test for the factorial program") {
    val content = ResourceHandle.getContent("Factorial.wp")

    assert(content != null)

    val p: WhileProgramParser = new WhileProgramParser()

    val wp: WhileProgram = p.parse(p.whileProgram, content) match {
      case p.Success(program: WhileProgram, _) => program
      case p.Failure(msg, _) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }

    val interpreter = new StructuralSemantics()

    interpreter.run(wp) match {
      case TC(e, s) => assert(s(e("y")) == 0); assert(s(e("z")) == 120)
      case _ => fail()
    }
  }



  test("Test for the fibonacci program") {
    val content = ResourceHandle.getContent("Fibonacci.wp")

    assert(content != null)

    val p: WhileProgramParser = new WhileProgramParser()

    val wp: WhileProgram = p.parse(p.whileProgram, content) match {
      case p.Success(program: WhileProgram, _) => program
      case p.Failure(msg, _) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }

    assert(wp != null)

    val interpreter = new StructuralSemantics()

     interpreter.run(wp) match {
      case TC(e, s) => assert(s(e("y")) == 34)  
      case _ => fail()
    }

  }


  }
