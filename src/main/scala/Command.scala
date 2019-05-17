/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */

import Todo._
import akka.actor.{ActorRef, _}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import TodoBook.ListReference

class Command(val notes: TodoBook) {

  def printHelp(): Unit = println(
    "\nAvailable commands:" +
      "\nl 'list name' => Creates/selects a todo list by name" +
      "\nl (or ls)     => Displays all available todo lists" +
      "\na 'task'      => Adds a task to the selected list" +
      "\np (or print)  => Prints selected list contents" +
      "\nr #number     => Removes a task by its number" +
      "\nm #number     => Marks/unmarks a task as done" +
      "\nc (or clear)  => Clears whole selected list" +
      "\nh (or help)   => Prints this help" +
      "\nx (or exit)   => Exits app" +
      "\n")

  implicit val timeout: Timeout = Timeout(2 seconds)

  private var selectedList = None: Option[ListReference]

  private def getLine: String = scala.io.StdIn.readLine().trim()

  private def getCommand(line: String = getLine): (Char, String) = {
    line.length match {
      case 0 => (' ', "")
      case 1 | 2 => (line(0), "")
      case _ => (line(0), line.drop(2).trim())
    }
  }

  private def toInt(s: String): Option[Int] = {
    try Some(s.toInt)
    catch {
      case _: Exception => None
    }
  }

  private def getName(list: ListReference): String = list._1

  private def getActor(list: ListReference): ActorRef = list._2

  def processCommand(): Boolean = getCommand() match {

    case ('l', name: String) =>
      if (name.isEmpty) notes.list(selectedList)
      else selectedList = Some(notes.select(name))
      true

    case ('a', task: String) =>
      selectedList match {
        case None => println("[error] No list selected!")
        case Some(list) =>
          val future = getActor(list) ? AddTaskCmd(task)
          val response =
            Await.result(future, timeout.duration).asInstanceOf[String]
          println(response)
      }
      true

    case ('p', _) =>
      selectedList match {
        case None => println("[error] No list selected!")
        case Some(list) =>
          val future = getActor(list) ? PrintListCmd
          val tasks =
            Await.result(future, timeout.duration).asInstanceOf[String]
          if (tasks.isEmpty) println(s"${getName(list)} list is empty")
          else println(s"Printing contents of ${getName(list)}:\n$tasks")
      }
      true

    case ('m', strIndex: String) =>
      toInt(strIndex) match {
        case None => println("[error] Invalid number provided!")
        case Some(index: Int) =>
          selectedList match {
            case None => println("[error] No list selected!")
            case Some(list) =>

              val future = getActor(list) ? MarkTaskCmd(index)
              val response =
                Await.result(future, timeout.duration).asInstanceOf[String]
              println(response)
          }
      }
      true

    case ('r', strIndex: String) =>
      toInt(strIndex) match {
        case None => println("[error] Invalid number provided!")
        case Some(index: Int) =>
          selectedList match {
            case None => println("[error] No list selected!")
            case Some(list) =>
              val future = getActor(list) ? RemoveTaskCmd(index)
              val response =
                Await.result(future, timeout.duration).asInstanceOf[String]
              println(response)
          }
      }
      true

    case ('c', _) =>
      selectedList match {
        case None => println("[error] No list selected!")
        case Some(list) =>
          notes.clear(list)
          selectedList = None
      }
      true

    case ('h', _) =>
      printHelp()
      true

    case ('x', _) | ('e', _) =>
      notes.save()
      println("Exiting TodoBook...")
      false

    case _ =>
      println("[error] Command unknown!")
      true
  }

}
