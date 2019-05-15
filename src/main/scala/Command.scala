import TodoActor.{AddTask, AskViewList, MarkTask, TaskList}
import akka.actor.{ActorRef, _}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

class Command(val notes: TodoBook) {

  private var selectedList = None: Option[notes.ListReference]

  private def getLine: String = scala.io.StdIn.readLine()

  private def getCommand(line: String = getLine): (Char, String) = {
    line.length match {
      case 0     => (' ', "")
      case 1 | 2 => (line(0), "")
      case _     => (line(0), line.drop(2))
    }
  }

  private def toInt(s: String): Option[Int] = {
    try Some(s.toInt)
    catch {
      case _: Exception => None
    }
  }

  private def getName(list: notes.ListReference): String = list._1

  private def getActor(list: notes.ListReference): ActorRef = list._2

  def process(): Boolean = getCommand() match {

    case ('s', name: String) =>
      name.length match {
        case 0 => notes.list()
        case _ => selectedList = Some(notes.select(name))
      }
      true

    case ('a', task: String) =>
      selectedList match {
        case None       => println("[error] no list selected!")
        case Some(list) => getActor(list) ! AddTask(task)
      }
      true

    case ('l', _) =>
      selectedList match {
        case None => println("[error] no list selected!")
        case Some(list) =>
          implicit val timeout: Timeout = Timeout(2 seconds)
          val future = getActor(list) ? AskViewList
          val tasks =
            Await.result(future, timeout.duration).asInstanceOf[TaskList]
          if (tasks.isEmpty) println(s"List ${getName(list)} is empty")
          else {
            println(s"Listing content of ${getName(list)} list:")
            tasks.zipWithIndex.foreach {
              case (task, index) =>
                if (task._1) println(s"${index + 1} - [X] ${task._2}")
                else println(s"${index + 1} - [ ] ${task._2}")
            }
          }
      }
      true

    case ('m', strIndex: String) =>
      toInt(strIndex) match {
        case None => println("[error] invalid number provided!")
        case Some(taskIndex: Int) =>
          selectedList match {
            case None       => println("[error] no list selected!")
            case Some(list) => getActor(list) ! MarkTask(taskIndex)
          }
      }
      true

    case ('c', _) =>
      selectedList match {
        case None => println("[error] no list selected!")
        case Some(list) =>
          notes.clear(list)
          selectedList = None
      }
      true

    case ('x', _) | ('e', _) =>
      println("exiting TodoBook...")
      false

    case _ =>
      println("[error] unknown command!")
      true
  }

}
