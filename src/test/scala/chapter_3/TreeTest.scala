package chapter_3

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers{
  def mockTree = Branch(Branch(Leaf(4), Branch(Leaf(1), Leaf(2))), Leaf(1))
  it should "return the number of nodes from size" in
    assert(Tree.size(mockTree) == 7)

  it should "return the leaf with maximum value from maximum" in
    assert(Tree.maximum(mockTree) == 4)

  it should "return the depth of the given tree" in
    assert(Tree.depth(mockTree) == 4)

  it should "return a true with modified values" in
    assert(Tree.map(mockTree)(a => a * 2) == Branch(Branch(Leaf(8), Branch(Leaf(2), Leaf(4))), Leaf(2)))

  it should "return the number of nodes from sizeViaFold" in
    assert(Tree.sizeViaFold(mockTree) == 7)

  it should "return the depth of the given tree from depthViaFold" in
    assert(Tree.depthViaFold(mockTree) == 4)

  it should "return the leaf with maximum value from maxViaFold" in
    assert(Tree.maxViaFold(mockTree) == 4)

  it should "return a true with modified values from mapViaFold" in
    assert(Tree.mapViaFold(mockTree)(a => a * 2) == Branch(Branch(Leaf(8), Branch(Leaf(2), Leaf(4))), Leaf(2)))
}
