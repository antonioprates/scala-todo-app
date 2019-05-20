package actors

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import core.TaskList
import core.TaskBehavior._
import TodoActor._

import akka.actor._
import akka.persistence._

class TodoActor extends PersistentActor {

  override def persistenceId: String = self.path.name

  private var state = ListState()

  def updateState(event: TaskEvt): Unit = {
    event match {
      case AddTaskEvt(task)  => state = ListState(addTask(state.list, task))
      case MarkTaskEvt(task) => state = ListState(markTask(state.list, task))
      case RemoveTaskEvt(task) =>
        state = ListState(removeTask(state.list, task))
      case ClearEvt => state = ListState()
    }
  }

  val receiveRecover: Receive = {
    case event: TaskEvt                        => updateState(event)
    case SnapshotOffer(_, snapshot: ListState) => state = snapshot
  }

  def persistEvent(event: TaskEvt)(handler: TaskEvt => Unit = _ => ()): Unit = {
    persist(event) { persistedEvent =>
      updateState(persistedEvent)
      context.system.eventStream.publish(persistedEvent)
      handler(persistedEvent)
    }
  }

  val receiveCommand: Receive = {

    case AddTaskCmd(task, ack) =>
      if (hasTask(state.list, task)) {
        if (ack) sender ! TaskExistsErr
      } else
        persistEvent(AddTaskEvt(task)) { _ =>
          if (ack) sender ! AddTaskRsp(self.path.name, task)
        }

    case MarkTaskCmd(task) =>
      persistEvent(MarkTaskEvt(task)) { _ =>
        sender ! TaskStatusRsp(task, isDone(state.list, task))
      }

    case GetListCmd => sender ! state.list

    case RemoveTaskFFCmd(task) =>
      persistEvent(RemoveTaskEvt(task))()

    case ClearFFCmd =>
      persistEvent(ClearEvt) { _ =>
        self ! PoisonPill
      }

    case SaveListFFCmd => saveSnapshot(state)

  }

}

object TodoActor {
  def props(): Props = Props(new TodoActor())

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

  case class ListState(list: TaskList = Nil)

}
