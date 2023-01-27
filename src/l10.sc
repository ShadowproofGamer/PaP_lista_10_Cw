

// 1.a Mutator error -
// x jest zarówno źródłem jak i odbiornikiem, co umożliwia jedną z 2 niemożliwych sytuacji
// A -> B lub B -> A

// 1.b
// var -> val
// +T -> T
// private[this] - bez sensu?

// 1.c
//
// Accesor error
// Error:(19, 32) contravariant type T occurs in covariant position in type => T of variable x

class GenericCellMut3[-T](var x: T) {}

class Mutable[-T](private[this] var x: T) {}
val s = new Mutable(List());




// 2
abstract class Sequence[+A] {
  def append[B >: A](x: Sequence[B]): Sequence[B]
}




//zad 3.
class UnderflowException(msg: String) extends Exception(msg);

class MyQueue[+T] private(private val queue: (List[T], List[T])) {
  def enqueue[S >: T](element: S): MyQueue[S] = {
    val (beginningOfQueue, endOfQueue) = queue
    normalize(beginningOfQueue, element :: endOfQueue)
  }

  private def normalize[S >: T](list1: List[S], list2: List[S]) =
    (list1, list2) match {
      case (Nil, endOfQueue) => new MyQueue[S]((endOfQueue.reverse, Nil))
      case normalQueue => new MyQueue(normalQueue)
    }

  def dequeue: MyQueue[T] =
    queue match {
      case (_ :: tail, endOfQueue) => normalize(tail, endOfQueue)
      case _ => MyQueue.empty
    }


  def first: T =
    queue._1 match {
      case (head :: _) => head
      case _ => throw new UnderflowException("first")
    }

  def isEmpty: Boolean =
    queue._1 == Nil

  //only for tests
  override def equals(obj: Any): Boolean =
    obj match {
      case obj: MyQueue[_] => queue._1 ++ queue._2.reverse == obj.queue._1 ++ obj.queue._2.reverse
      case _ => false
    }
}
object MyQueue {
  def apply[T](xs: T*) = new MyQueue[T](xs.toList, Nil)

  def empty[T] = new MyQueue[T](Nil, Nil)
}





//4.

import scala.collection.mutable

def copy[T](dest: mutable.Seq[T], src: mutable.Seq[T]): Unit = {
  require(dest.length >= src.length)
  var index = 0
  src.foreach(element => {
    dest.update(index, element)
    index += 1
  })
};

//(ArrayBuffer)
