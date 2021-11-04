package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.cfg.PathBuilder._
import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.wlang.cfg.CFGBuilder._


class PathBuilderTest extends AnyFunSuite {

  val flow1: CFG = Set((1,2), (2,3), (3,4), (4,5), (5,3), (3,6))
  val flow2: CFG = Set((0,1), (0,3), (1,3), (2,0), (2,1))
  val flow3: CFG = Set(
    (1,2), (2,3), (3,8),
    (2,4), (4,1), (8,5), (5,6), (6,1), (8,7), (7,8),
    (9, 1), (8,10)
  )
  val interFlow: InterCFG = Set((9, 1, 8, 10), (4, 1, 8, 5), (6, 1, 8, 7))

  test("Test for path 1 flow1") {
    val expected = List(1)
    assert(path(1, 1, flow1).contains(expected))
  }

  test("Test for path 1 2 flow1") {
    val expected = List(1,2)
    assert(path(1, 2, flow1).contains(expected))
  }

  test("Test for path 1 3 flow1") {
    val expected1 = List(1,2,3)
    val expected2 = List(1,2,3,4,5,3)
    assert(path(1, 3, flow1).contains(expected1))
    assert(path(1, 3, flow1).contains(expected2))
  }

  test("Test for path 2 3 flow1") {
    val expected1 = List(2,3)
    val expected2 = List(2, 3, 4, 5, 3)
    assert(path(2, 3, flow1).contains(expected1))
    assert(path(2, 3, flow1).contains(expected2))
  }

  test("Test for path 1 4 flow1") {
    val expected1 = List(1,2,3,4)
    val expected2 = List(1,2,3,4,5,3,4)
    assert(path(1, 4, flow1).contains(expected1))
    assert(path(1, 4, flow1).contains(expected2))
  }

  test("Test for path 1 5 flow1") {
    val expected1 = List(1,2,3,4,5)
    val expected2 = List(1,2,3,4,5,3,4,5)
    assert(path(1, 5, flow1).contains(expected1))
    assert(path(1, 5, flow1).contains(expected2))
  }

  test("Test for path 1 6 flow1") {
    val expected1 =List(1,2,3,4,5,3,6)
    val expected2 = List(1,2,3,6)
    assert(path(1, 6, flow1).contains(expected1))
    assert(path(1, 6, flow1).contains(expected2))
  }


  // val flow2: CFG = Set((2,0), (2,1), (0,3), (0,1), (1,3))

  test("Test for flow2") {
    val expected1 = List(2, 1, 3)
    val expected2 = List(2, 0, 1, 3)
    assert(path(2, 3, flow2).contains(expected1))
    assert(path(2, 3, flow2).contains(expected2))
  }


  test("Test for path 9 10 flow3") {
    val expected1 = List(9, 1, 2, 4, 1, 2, 3, 8, 5, 6, 1, 2, 3, 8, 7, 8, 10)
    val expected2 = List(9, 1, 2, 4, 1, 2, 3, 8, 10)
    assert(path(9, 10, flow3).contains(expected1))
    assert(path(9, 10, flow3).contains(expected2))
    assert(completePath(expected1,flow3, interFlow))
    assert(!completePath(expected2,flow3, interFlow))
  }

  test("Test for complete paths") {
    val expected1 = List(9, 1, 2, 4, 1, 2, 3, 8, 5, 6, 1, 2, 3, 8, 7, 8, 10)
    val expected2 = List(9, 1, 2, 4, 1, 2, 3, 8, 10)
    assert(completePath(expected1,flow3, interFlow))
    assert(!completePath(expected2,flow3, interFlow))
  }

  test("Test for an invalid path (it does not make sense to call CP on it)") {
    intercept[java.lang.AssertionError] {
      val expected1 = List(9, 1, 2, 3, 10)
      completePath(expected1,flow3, interFlow)
    }
  }

}
