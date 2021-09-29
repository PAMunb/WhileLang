package br.unb.cic.wlang.semantics

import br.unb.cic.wlang.WhileProgram
import br.unb.cic.wlang.parser.{ResourceHandle, WhileProgramParser}
import org.scalatest.funsuite.AnyFunSuite

class StructuralSemanticsTest extends AnyFunSuite {

  test("Test for the simple program") {
    val content = ResourceHandle.getContent("Simple.wp")

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

    res match {
      case TC(e, s) => println(res); assert(s(e("y")) == 10)
      case _ => fail()
    }
  }

  test("Test for the program that checks if a number is positive, negative or zero") {
    val content = ResourceHandle.getContent("CheckPosNegZero.wp")

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

    res match {
      case TC(e, s) => println(res); assert(s(e("z")) == 3)
      case _ => fail()
    }
  }

  test("Test for the cube program") {
    val content = ResourceHandle.getContent("Cube.wp")

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

    res match {
      case TC(e, s) => println(res); assert(s(e("z")) == 27)
      case _ => fail()
    }
  }

  test("Test for the square program") {
    val content = ResourceHandle.getContent("Square.wp")

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

    res match {
      case TC(e, s) => println(res); assert(s(e("z")) == 9)
      case _ => fail()
    }
  }

  test("Test for the swap variables program") {
    val content = ResourceHandle.getContent("Swapvars.wp")

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

    res match {
      case TC(e, s) => println(res); assert(s(e("y")) == 20); assert(s(e("x")) == 10); assert(s(e("tmp")) == 20)
      case _ => fail()
    }
  }

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

    interpreter.run(wp) match {
      case TC(e, s) => assert(s(e("x")) == 5)
      case _ => fail()
    }
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
