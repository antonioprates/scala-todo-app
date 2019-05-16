/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import akka.actor.{ActorRef, ActorSystem, Props, Terminated}

import scala.concurrent.Future

import TodoBook.{ListReference, ContextIndex}

// TodoBook stores akka context and actor references and provides simple interface

class TodoBook {

  private var lists: ContextIndex = List()
  private val context = ActorSystem("TodoBook")

  private def secureName(name: String): String =
    name.trim().replace(' ', '-').toUpperCase()

  def select(name: String): ListReference = {
    val safeName = secureName(name)
    lists.find(reference => reference._1 == safeName) match {
      case None =>
        val list: ListReference =
          (safeName, context.actorOf(Props[TodoActor], name = safeName))
        lists = lists :+ list
        println(s"New $safeName list was created and is now selected")
        list
      case Some(list) =>
        println(s"Existing $safeName list is now selected")
        list
    }
  }

  def list(selectedList: Option[ListReference]): Unit = {
    if (lists.isEmpty) println("You don't have any lists yet")
    else {
      println("Available lists:")
      selectedList match {
        case None => lists.foreach(list => println(s"=> ${list._1}"))
        case Some(selected) =>
          lists.foreach(list => {
            if (selected._1 == list._1) println(s"=> ${list._1} *")
            else println(s"=> ${list._1}")
          })
      }

    }
  }

  def clear(list: ListReference): Unit = {
    lists.find(reference => reference._1 == list._1) match {
      case None => println("[unexpected error] list not found!")
      case Some(reference) =>
        context.stop(reference._2)
        lists = lists.filter(reference => reference._1 != list._1)
        println(s"${list._1} list was cleared")
    }
  }

  def shutdown: () => Future[Terminated] = () => context.terminate()

}

object TodoBook {
  type ListReference = (String, ActorRef)
  type ContextIndex = List[ListReference]
}
