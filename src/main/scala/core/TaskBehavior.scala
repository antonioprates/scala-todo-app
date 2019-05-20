package core

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object TaskBehavior {

  // read only

  private def isTask(task: String)(entry: Task): Boolean = entry._2 == task

  private def getTask(list: TaskList, task: String): Option[Task] =
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

  def addTask(list: TaskList, task: String): TaskList =
    if (hasTask(list, task)) list else list :+ (false, task)

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
