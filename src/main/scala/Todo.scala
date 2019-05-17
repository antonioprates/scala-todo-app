/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object Todo {

  // a Task is a pair of boolean, indicating isDone, and a string, for description
  type Task = (Boolean, String)
  type TaskList = List[Task]

  sealed trait TaskEvt
  case class AddTaskEvt(task: String) extends TaskEvt
  case class MarkTaskEvt(task: String) extends TaskEvt
  case class RemoveTaskEvt(task: String) extends TaskEvt
  case object ClearEvt extends TaskEvt

  sealed trait TaskCmd
  case class AddTaskCmd(task: String, ack: Boolean) extends TaskCmd
  case class MarkTaskCmd(task: String) extends TaskCmd
  case object GetListCmd extends TaskCmd
  case object SaveListFFCmd extends TaskCmd
  case class RemoveTaskFFCmd(task: String) extends TaskCmd
  case object ClearFFCmd extends TaskCmd

  sealed trait TaskRsp
  case class AddTaskRsp(list: String, task: String) extends TaskRsp
  case class TaskStatusRsp(task: String, isDone: Boolean) extends TaskRsp

  sealed trait ErrorMsg
  case object NoListErr extends ErrorMsg
  case object TaskExistsErr extends ErrorMsg
  case object InvalidNumberErr extends ErrorMsg
  case class OutOfRangeErr(index: Int, range: Int) extends ErrorMsg

  case class ListState(tasks: TaskList = List()) {

    // mutation methods

    def add(task: String): ListState = copy(tasks :+ (false, task))

    def remove(task: String): ListState =
      copy(tasks.filterNot(entry => entry._2 == task))

    def mark(task: String): ListState =
      tasks.find(entry => entry._2 == task) match {
        case None => copy(tasks)
        case Some(entry) => {
          val index = tasks.indexOf(entry)
          copy(tasks.updated(index, (!tasks(index)._1, tasks(index)._2)))
        }
      }

    def clear(): ListState = copy(List())

    // read only methods

    private val blank: Task = (false, "")

    def isDone(task: String): Boolean =
      tasks.find(entry => entry._2 == task).getOrElse(blank)._1

    def fullList: TaskList = tasks

    def hasTask(task: String): Boolean =
      tasks.find(t => t._2 == task) match {
        case None => false
        case _    => true
      }

  }

}
