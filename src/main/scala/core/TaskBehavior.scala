package core

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object TaskBehavior {

  // internal methods

  private def _isTask(task: String)(entry: Task): Boolean = entry._2 == task

  private def _isDone(entry: Task): Boolean = entry._1

  private def _isNotDone(entry: Task): Boolean = !entry._1

  private def _getTask(list: TaskList, task: String): Option[Task] =
    list.find(_isTask(task))

  // read only methods

  def hasTask(list: TaskList, task: String): Boolean =
    _getTask(list, task) match {
      case None    => false
      case Some(_) => true
    }

  def hasOnlyTask(list: TaskList, task: String): Boolean =
    list.length == 1 && hasTask(list, task)

  def isDone(list: TaskList, task: String): Boolean =
    _getTask(list, task) match {
      case None              => false
      case Some(entry: Task) => _isDone(entry)
    }

  def hasDoneAll(list: TaskList): Boolean =
    if (list.isEmpty) false
    else
      list.find(_isNotDone) match {
        case Some(_) => false
        case None    => true
      }

  def hasDoneAllExcept(list: TaskList, task: String): Boolean =
    !hasDoneAll(list) && hasDoneAll(removeTask(list, task))

  // mutation methods

  def addTask(list: TaskList, task: String): TaskList =
    if (hasTask(list, task)) list else list :+ (false, task)

  def removeTask(list: TaskList, task: String): TaskList =
    list.filterNot(_isTask(task))

  def markTask(list: TaskList, task: String): TaskList =
    _getTask(list, task) match {
      case None => list
      case Some(entry) => {
        list.updated(list.indexOf(entry), (_isNotDone(entry), task))
      }
    }

}
