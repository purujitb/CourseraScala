package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  def emptySet(): Set = {
    def setFun(elem: Int): Boolean = false
    setFun
  }
  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = {
    def setFun(num: Int): Boolean = 
      elem == num
      
    setFun
  }

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = {
    def setFun(elem: Int): Boolean = 
      contains(s, elem) || contains(t, elem) 
      
    setFun
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = {
    def setFun(elem: Int): Boolean = 
      contains(s, elem) && contains(t, elem) 
      
    setFun
  }

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = {
    def setFun(elem: Int): Boolean = 
      contains(s, elem) && !contains(t, elem) 
      
    setFun
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = {
    def setFun(elem: Int): Boolean = 
      contains(s, elem) && p(elem) 
      
    setFun
  }

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) true
      else if (s(a)) {
        p(a) && iter(a-1)
      }
      else iter(a -1)
    }
    iter(bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) false
      else if (s(a)) forall(singletonSet(a), p) || iter(a - 1)
      else iter(a -1)
    }
    iter(bound)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    
    def mappedSet(i: Int): Set = {
      if (i < -bound) emptySet()
      else if (contains(s, i)) union(singletonSet(f(i)), mappedSet(i - 1))
      else mappedSet(i - 1)
    }
    
    mappedSet(bound)
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
