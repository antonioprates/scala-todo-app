import akka.actor.{ActorRef, ActorSystem, Props, Terminated}

import scala.concurrent.Future

// TodoBook stores akka context and actor references and provides simple interface

class TodoBook {

  type ListReference = (String, ActorRef)
  type ContextIndex = List[ListReference]

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
        println(s"a new $safeName todo list was created and is now selected")
        list
      case Some(list) =>
        println(s"existing $safeName todo list is now selected")
        list
    }
  }

  def list(): Unit = {
    if (lists.isEmpty) println("you don't have any lists yet")
    else {
      println("available lists:")
      lists.foreach(list => println(s"=> ${list._1}"))
    }
  }

  def clear(list: ListReference): Unit = {
    lists.find(reference => reference._1 == list._1) match {
      case None => println("[unexpected error] list not found!")
      case Some(reference) =>
        context.stop(reference._2)
        lists = lists.filter(reference => reference._1 != reference._1)
        println(s"${list._1} todo list was cleared")
    }
  }

  def shutdown: () => Future[Terminated] = () => context.terminate()

}
