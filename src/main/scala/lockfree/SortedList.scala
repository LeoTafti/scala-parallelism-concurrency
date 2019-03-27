package lockfree

import scala.annotation.tailrec

class SortedList extends AbstractSortedList {

  // The sentinel node at the head.
  private val _head: Node = createNode(0, None, isHead = true)

  // The first logical node is referenced by the head.
  def firstNode: Option[Node] = _head.next

  // Finds the first node whose value satisfies the predicate.
  // Returns the predecessor of the node and the node.
  def findNodeWithPrev(pred: Int => Boolean): (Node, Option[Node]) = {
    def findRec(startFrom : Node) : (Node, Option[Node]) = {
      startFrom.next match {
        case None => (startFrom, None)
        case Some(nextNode) =>
          if(nextNode.deleted){
            startFrom.atomicState.compareAndSet((Some(nextNode), false), (nextNode.next, false))
            findNodeWithPrev(pred)
          }
          else if(pred(nextNode.value)) (startFrom, Some(nextNode))
          else findRec(nextNode)
      }
    }
    findRec(_head)
  }

  // Insert an element in the list.
  def insert(e: Int): Unit = {
    val (pred, next) = findNodeWithPrev(_ >= e);
    val n = createNode(e, next)
    if(!pred.atomicState.compareAndSet((next, false), (Some(n), false))) insert(e)
  }

  // Checks if the list contains an element.
  def contains(e: Int): Boolean = {
    findNodeWithPrev(_ == e)._2 match {
      case None => false
      case _ => true;
    }
  }

  // Delete an element from the list.
  // Should only delete one element when multiple occurences are present.
  def delete(e: Int): Boolean = {
    val (pred, next) = findNodeWithPrev(_ == e)
    next match{
      case None => false
      case Some(x) => if(x.mark) true else delete(e)
    }
  }
}
