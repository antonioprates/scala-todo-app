/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object Todo {

  // a Task is a pair of boolean, indicating isDone, and a string, for description
  type Task = (Boolean, String)
  type TaskList = List[Task]

  trait TaskEvt

  case class AddTaskEvt(task: String) extends TaskEvt

  case class MarkTaskEvt(index: Int) extends TaskEvt

  case class RemoveTaskEvt(task: String) extends TaskEvt

  case object ClearEvt extends TaskEvt

  trait TaskCmd

  case class AddTaskCmd(task: String) extends TaskCmd

  case class AddTaskFFCmd(task: String) extends TaskCmd

  case class MarkTaskCmd(index: Int) extends TaskCmd

  case object PrintListCmd extends TaskCmd

  case object SaveListFFCmd extends TaskCmd

  case object GetPlainListCmd extends TaskCmd

  case class RemoveTaskCmd(index: Int) extends TaskCmd

  case class RemoveTaskFFCmd(task: String) extends TaskCmd

  case object ClearFFCmd extends TaskCmd

  case class ListState(tasks: TaskList = List()) {

    // mutation methods
    def add(task: String): ListState = copy(tasks :+ (false, task))

    def remove(task: String): ListState =
      copy(tasks.filter(entry => entry._2 != task))

    def mark(index: Int): ListState =
      copy(tasks.updated(index, (!tasks(index)._1, tasks(index)._2)))

    def clear(): ListState = copy(List())

    // read only methods
    def isDone(index: Int): Boolean = tasks(index)._1

    def getTask(index: Int): String = tasks(index)._2

    def fullList: TaskList = tasks

    def isEmpty: Boolean = tasks.isEmpty

    def size: Int = tasks.length

    def hasTask(task: String): Boolean =
      tasks.find(t => t._2 == task) match {
        case None => false
        case _    => true
      }

  }

}
