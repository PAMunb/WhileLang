package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.cfg.PathBuilder._
import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.wlang.cfg.CFGBuilder._


class PathBuilderTest extends AnyFunSuite {

  val flow1: CFG = Set((1,2), (2,3), (3,4), (4,5), (5,3), (3,6))

  test("Test for path 1 2 flow1") {
    val expected : Set[Path] = Set(List(1,2))
    assert(path(1, 2, flow1) == expected)
  }

  test("Test for path 1 3 flow1") {
    val expected : Set[Path] = Set(List(1,2,3), List(1,2,3,4,5,3))
    assert(expected == path(1, 3, flow1))
  }

  test("Test for path 1 4 flow1") {
    val expected : Set[Path] = Set(List(1,2,3,4), List(1,2,3,4,5,3,4))
    assert(expected == path(1, 4, flow1))
  }

  test("Test for path 1 5 flow1") {
    val expected : Set[Path] = Set(List(1,2,3,4,5), List(1,2,3,4,5,3,4,5))
    assert(expected == path(1, 5, flow1))
  }

  test("Test for path 1 6 flow1") {
    val expected : Set[Path] = Set(List(1,2,3,4,5,3,6), List(1,2,3,6))
    assert(expected == path(1, 6, flow1))
  }
}
