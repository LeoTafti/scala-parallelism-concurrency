package pubsub.collection

class BoundedBuffer[T](size: Int) extends AbstractBoundedBuffer[T](size) {

  override def put(e: T): Unit = synchronized{
    while(isFull) wait()
    buffer(head) = e
    head = nextHeadIndex;
    count = count + 1;
    notifyAll()
  }

  override def take(): T = synchronized{
    while(isEmpty) wait()

    val e = buffer(tailIndex);
    buffer.delete(tailIndex);
    count = count - 1;
    notifyAll()
    e
  }

  def isEmpty : Boolean = count == 0;
  def isFull : Boolean = count == size
  def tailIndex : Int = {
    val diff = head - count
    if(diff >= 0) diff
    else diff + size
  }
  def nextHeadIndex : Int = (head + 1) % size
}
