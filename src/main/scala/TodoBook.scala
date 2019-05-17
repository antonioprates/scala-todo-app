/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import Todo._
import akka.actor.{ActorRef, _}
import akka.pattern.ask

import scala.concurrent.{Await, Future}
import TodoBook.{ContextIndex, ListReference}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.language.postfixOps

// TodoBook stores akka context and actor references and provides simple interface

class TodoBook {

  private val context = ActorSystem("TodoBook")

  private val persistentTodoBook: ActorRef =
    context.actorOf(Props[TodoActor], name = "root")

  private def loadLists(): ContextIndex = {
    implicit val timeout: Timeout = Timeout(5 seconds)
    val future = persistentTodoBook ? GetPlainListCmd
    val root =
      Await.result(future, timeout.duration).asInstanceOf[List[String]]
    root.map(entry => (entry, context.actorOf(Props[TodoActor], name = entry)))
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
        persistentTodoBook ! AddTaskFFCmd(safeName)
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
        Thread.sleep(1000)
        reference._2 ! PoisonPill
        persistentTodoBook ! RemoveTaskFFCmd(reference._1)
        lists = lists.filter(reference => reference._1 != list._1)
        println(s"${list._1} list was cleared")
    }
  }

  def save(): Unit = {
    lists.foreach(list => list._2 ! SaveListFFCmd)
    persistentTodoBook ! SaveListFFCmd
  }

  def shutdown: () => Future[Terminated] = () => context.terminate()

}

object TodoBook {
  type ListReference = (String, ActorRef)
  type ContextIndex = List[ListReference]
}
