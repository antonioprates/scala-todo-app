package api

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import core.TaskList
import core.ListReference
import core.ContextIndex

import actors.TodoActor
import actors.TodoActor._
import actors.TodoPersistentFSM

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.language.postfixOps

// TodoBook operates akka context and actor references providing a basic API

class TodoBook(implicit context: ActorSystem) {

  private val persistentTodoBook: ActorRef =
    context.actorOf(TodoActor.props(), name = "root")

  private def loadLists(): ContextIndex = {
    implicit val timeout: Timeout = Timeout(5 seconds)
    val future = persistentTodoBook ? GetListCmd
    val root =
      Await.result(future, timeout.duration).asInstanceOf[TaskList]
    root.map(entry =>
      (entry._2, context.actorOf(TodoPersistentFSM.props(), name = entry._2)))
  }

  private var lists: ContextIndex = loadLists()

  private def secureName(name: String): String =
    name.replace(' ', '-').toUpperCase()

  def select(name: String): (ListReference, Boolean) = {
    val safeName = secureName(name)
    lists.find(reference => reference._1 == safeName) match {
      case None =>
        val list: ListReference =
          (safeName,
           context.actorOf(TodoPersistentFSM.props(), name = safeName))
        lists = lists :+ list
        persistentTodoBook ! AddTaskCmd(safeName, ack = false) // FF mode
        (list, false) // isExisting = false
      case Some(list) =>
        (list, true) // isExisting = true
    }
  }

  def list(): ContextIndex = { lists }

  def clear(list: ListReference): Unit = {
    lists.find(reference => reference._1 == list._1) match {
      case Some(reference) =>
        reference._2 ! ClearFFCmd
        Thread.sleep(1000)
        reference._2 ! PoisonPill
        persistentTodoBook ! RemoveTaskFFCmd(reference._1)
        lists = lists.filter(reference => reference._1 != list._1)
      case None => // do nothing
    }
  }

  def save(): Unit = {
    lists.foreach(list => list._2 ! SaveListFFCmd)
    persistentTodoBook ! SaveListFFCmd
  }

}
