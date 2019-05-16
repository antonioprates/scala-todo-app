/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import TodoActor.{AddTask, AskViewList, MarkTask, Task, TaskList}
import akka.actor.Actor

class TodoActor extends Actor {

  private var tasks: TaskList = List()

  private def toggleTask(index: Int): TaskList = {
    val flippedItem: Task = (!tasks(index)._1, tasks(index)._2)
    if (flippedItem._1) println(s"Task '${flippedItem._2}' marked as done")
    else println(s"Task '${flippedItem._2}' marked as incomplete")
    tasks.updated(index, flippedItem)
  }

  def receive: PartialFunction[Any, Unit] = {

    case AddTask(task) =>
      println(s"Adding '$task' to ${self.path.name} list")
      tasks = tasks :+ (false, task)

    case AskViewList =>
      sender ! tasks

    case MarkTask(taskIndex) =>
      if (tasks.isEmpty)
        println(s"${self.path.name} list has no tasks yet")
      else if (taskIndex > tasks.length)
        println(s"[error] Index $taskIndex is out of range!")
      else if (taskIndex == 0)
        println(s"[error] Invalid index provided!")
      else tasks = toggleTask(taskIndex - 1)

    case _ => println("Huh?")

  }
}

object TodoActor {

  // a Task is a pair of boolean, indicating isDone, and a string, for description
  type Task = (Boolean, String)
  type TaskList = List[Task]

  case object AskViewList
  case class AddTask(task: String)
  case class MarkTask(taskIndex: Int)
}
