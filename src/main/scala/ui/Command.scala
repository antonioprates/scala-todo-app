package ui

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import core.TaskList
import core.ListReference
import api.TodoBook
import actors.TodoActor._

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

// Command is a simple UI that inputs/outputs directly to/from Terminal console

class Command(val notes: TodoBook) {

  implicit val timeout: Timeout = Timeout(2 seconds)

  private val keepalive = true

  private var selectedList = None: Option[ListReference]

  def printHelp(): Unit =
    println(
      "\nAvailable commands:" +
        "\nL/l (or list)   => Displays all available todo lists" +
        "\nS/s 'list name' => Creates/selects a todo list by name" +
        "\nA/a 'task'      => Adds a task to the selected list" +
        "\nP/p (or print)  => Prints selected list contents" +
        "\nR/r (or remove) => Removes a task by its number" +
        "\nM/m (or mark)   => Marks/unmarks a task as done" +
        "\nC/c (or clear)  => Clears whole selected list" +
        "\nH/h (or help)   => Prints this help" +
        "\nX/x (or exit)   => Exits app" +
        "\n")

  def list(): Unit = {
    val lists = notes.list()
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

  private def printRsp(response: TaskRsp): Unit = response match {
    case AddTaskRsp(list, task) => println(s"'$task' added to $list list")
    case TaskStatusRsp(task, isDone) =>
      if (isDone) println(s"Task '$task' marked as done")
      else println(s"Task '$task' marked as incomplete")
  }

  private def printErr(error: ErrorMsg): Unit = error match {
    case NoListErr        => println("[error] No list selected!")
    case TaskExistsErr    => println("[error] Task already exists on list")
    case InvalidNumberErr => println("[error] Invalid number provided!")
    case OutOfRangeErr(index, range) =>
      println(s"[error] Index $index is out of range, try between 1 and $range")
  }

  private def printList(list: String, tasks: TaskList): Unit =
    if (tasks.isEmpty) println(s"$list list has no tasks yet")
    else {
      println(s"Contents of $list:")
      tasks.zipWithIndex.foreach {
        case (task, index) =>
          if (task._1) println(s"${index + 1} - [X] ${task._2}")
          else println(s"${index + 1} - [ ] ${task._2}")
      }
    }

  private def getTasks(list: ListReference): TaskList = {
    val future = getActor(list) ? GetListCmd
    val tasks = Await.result(future, timeout.duration).asInstanceOf[TaskList]
    tasks
  }

  private def addTask(list: ListReference, task: String): Any = {
    val future = getActor(list) ? AddTaskCmd(task, ack = true)
    val response = Await.result(future, timeout.duration)
    response
  }

  private def getLine: String = scala.io.StdIn.readLine().trim()

  private def getCommand(line: String = getLine): (Char, String) = {
    line.length match {
      case 0     => (' ', "")
      case 1 | 2 => (line.toLowerCase()(0), "")
      case _     => (line.toLowerCase()(0), line.drop(2).trim())
    }
  }

  private def toInt(s: String): Option[Int] = {
    try Some(s.toInt)
    catch {
      case _: Exception => None
    }
  }

  private def getValidNumber(range: Int): Option[Int] = {
    toInt(getLine) match {
      case None =>
        printErr(InvalidNumberErr)
        None
      case Some(index: Int) =>
        if (index > range | index < 1) {
          printErr(OutOfRangeErr(index, range))
          None
        } else Some(index - 1)
    }
  }

  private def getName(list: ListReference): String = list._1

  private def getActor(list: ListReference): ActorRef = list._2

  def processCommand(): Boolean = getCommand() match {

    case ('l', _) =>
      list()
      keepalive

    case ('s', name: String) =>
      val (list, isExisting) = notes.select(name)
      selectedList = Some(list)
      if (isExisting)
        println(s"Existing ${getName(list)} list is now selected")
      else
        println(s"New ${getName(list)} list was created and is now selected")
      keepalive

    case ('a', task: String) =>
      selectedList match {
        case None => printErr(NoListErr)
        case Some(list) =>
          addTask(list, task) match {
            case error: ErrorMsg  => printErr(error)
            case success: TaskRsp => printRsp(success)
          }
      }
      keepalive

    case ('p', _) =>
      selectedList match {
        case None => printErr(NoListErr)
        case Some(list) =>
          getTasks(list) match {
            case tasks: TaskList => printList(getName(list), tasks)
          }
      }
      keepalive

    case ('m', _: String) =>
      selectedList match {
        case None => printErr(NoListErr)
        case Some(list) =>
          getTasks(list) match {
            case tasks: TaskList =>
              if (tasks.nonEmpty) {
                printList(getName(list), tasks)
                print("Enter task number to mark/unmark: ")
                getValidNumber(tasks.length) match {
                  case Some(index) =>
                    val task = tasks(index)._2
                    val future = getActor(list) ? MarkTaskCmd(task)
                    val response = Await.result(future, timeout.duration)
                    response match {
                      case status: TaskStatusRsp =>
                        printRsp(status)
                        printList(getName(list), getTasks(list))
                    }
                  case None => // do nothing
                }
              }
          }
      }
      keepalive

    case ('r', _) =>
      selectedList match {
        case None => printErr(NoListErr)
        case Some(list) =>
          getTasks(list) match {
            case tasks: TaskList =>
              if (tasks.nonEmpty) {
                printList(getName(list), tasks)
                print("Enter task number to remove: ")
                getValidNumber(tasks.length) match {
                  case Some(index) =>
                    val task = tasks(index)._2
                    getActor(list) ! RemoveTaskFFCmd(task)
                    println(s"'$task' removed from ${getName(list)} list")
                    printList(getName(list), getTasks(list))
                  case None => // do nothing
                }
              }
          }
      }
      keepalive

    case ('c', _) =>
      selectedList match {
        case None => printErr(NoListErr)
        case Some(list) =>
          println(s"Do you want to clear ${getName(list)} list (y/n)?")
          getCommand() match {
            case ('y', _) =>
              notes.clear(list)
              selectedList = None
              println(s"${getName(list)} list was cleared")
            case _ => // do nothing
          }
      }
      keepalive

    case ('h', _) =>
      printHelp()
      true

    case ('x', _) | ('e', _) | ('q', _) =>
      println("Persisting state as snapshot...")
      notes.save()
      !keepalive

    case _ =>
      println("[error] Command unknown!")
      keepalive
  }

}
