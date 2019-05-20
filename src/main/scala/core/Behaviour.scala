package core

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object Behaviour {

  // a Task is a pair of boolean, indicating isDone, and a string, for description
  type Task = (Boolean, String)
  type TaskList = List[Task]

  // read only

  private def isTask(task: String)(entry: Task): Boolean = entry._2 == task

  def getTask(list: TaskList, task: String): Option[Task] =
    list.find(isTask(task))

  def hasTask(list: TaskList, task: String): Boolean =
    getTask(list, task) match {
      case None    => false
      case Some(_) => true
    }

  def isDone(list: TaskList, task: String): Boolean =
    getTask(list, task) match {
      case None              => false
      case Some(entry: Task) => entry._1
    }

  // mutation methods

  def addTask(list: TaskList, task: String): TaskList = list :+ (false, task)

  def removeTask(list: TaskList, task: String): TaskList =
    list.filterNot(isTask(task))

  def markTask(list: TaskList, task: String): TaskList =
    getTask(list, task) match {
      case None => list
      case Some(entry) => {
        val index = list.indexOf(entry)
        list.updated(index, (!list(index)._1, list(index)._2))
      }
    }

}
