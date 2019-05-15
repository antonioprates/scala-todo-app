import TodoActor.{AddTask, AskViewList, MarkTask, Task, TaskList}
import akka.actor.Actor

class TodoActor extends Actor {

  private var tasks: TaskList = List()

  private def toggleTask(index: Int): TaskList = {
    val flippedItem: Task = (!tasks(index)._1, tasks(index)._2)
    if (flippedItem._1) println(s"task '${flippedItem._2}' marked as done.")
    else println(s"task '${flippedItem._2}' marked as incomplete.")
    tasks.updated(index, flippedItem)
  }

  def receive: PartialFunction[Any, Unit] = {

    case AddTask(task) =>
      println(s"adding '$task' to ${self.path.name} list")
      tasks = tasks :+ (false, task)

    case AskViewList =>
      sender ! tasks

    case MarkTask(taskIndex) =>
      if (tasks.isEmpty)
        println(s"list ${self.path.name} has no tasks yet")
      else if (taskIndex > tasks.length)
        println(s"[Error] index $taskIndex is out of range!")
      else if (taskIndex == 0)
        println(s"[Error] invalid index provided!")
      else tasks = toggleTask(taskIndex - 1)

    case _ => println("huh?")

  }
}

object TodoActor {

  // A Task is a pair of boolean indicating isDone and a string for task description
  type Task = (Boolean, String)
  type TaskList = List[Task]

  case object AskViewList
  case class AddTask(task: String)
  case class MarkTask(taskIndex: Int)
}
