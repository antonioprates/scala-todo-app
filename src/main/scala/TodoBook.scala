/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import Todo._
import TodoBook._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

// TodoBook stores akka context and actor references and provides simple interface
// designed to log any feedback directly to Terminal

class TodoBook {

  private val context = ActorSystem("TodoBook")

  private val persistentTodoBook: ActorRef =
    context.actorOf(Props[TodoActor], name = "root")

  private def loadLists(): ContextIndex = {
    implicit val timeout: Timeout = Timeout(5 seconds)
    val future = persistentTodoBook ? GetListCmd
    val root =
      Await.result(future, timeout.duration).asInstanceOf[TaskList]
    root.map(entry =>
      (entry._2, context.actorOf(Props[TodoActor], name = entry._2)))
  }

  private var lists: ContextIndex = loadLists()

  private def secureName(name: String): String =
    name.replace(' ', '-').toUpperCase()

  def select(name: String): ListReference = {
    val safeName = secureName(name)
    lists.find(reference => reference._1 == safeName) match {
      case None =>
        val list: ListReference =
          (safeName, context.actorOf(Props[TodoActor], name = safeName))
        lists = lists :+ list
        persistentTodoBook ! AddTaskCmd(safeName, ack = false)
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
        reference._2 ! ClearFFCmd
        persistentTodoBook ! RemoveTaskFFCmd(reference._1)
        lists = lists.filter(reference => reference._1 != list._1)
        println(s"${list._1} list was cleared")
    }
  }

  def save(): Unit = {
    println("Persisting state snapshots...")
    lists.foreach(list => list._2 ! SaveListFFCmd)
    persistentTodoBook ! SaveListFFCmd
  }

  def shutdown: () => Future[Terminated] = () => context.terminate()

}

object TodoBook {
  type ListReference = (String, ActorRef)
  type ContextIndex = List[ListReference]
}
