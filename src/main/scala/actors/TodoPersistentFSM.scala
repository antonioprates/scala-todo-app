package actors

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import core.TaskBehavior._
import TodoActor._
import TodoPersistentFSM._

import akka.actor.Props
import akka.persistence.fsm.PersistentFSM
import akka.persistence.fsm.PersistentFSM._

import scala.reflect.ClassTag

class TodoPersistentFSM(implicit val domainEventClassTag: ClassTag[TaskEvt])
    extends PersistentFSM[TodoState, ListState, TaskEvt] {

  override def persistenceId: String = self.path.name

  startWith(Empty, ListState())

  when(Empty) {

    case Event(AddTaskCmd(task, ack), _) => {
      val res = goto(Active).applying(AddTaskEvt(task))
      if (ack) res.replying(AddTaskRsp(self.path.name, task))
      else res
    }

    case Event(MarkTaskCmd(_), _) => stay.replying(EmptyListErr)

    case Event(GetListCmd, _) => stay.replying(Nil)

    case Event(SaveListFFCmd, _) =>
      saveStateSnapshot()
      stay

    case Event(GetTodoStateCmd, _) =>
      stay.replying(EmptyListRsp(self.path.name))

    case _ => stay

  }

  when(Active) {

    case Event(AddTaskCmd(task, ack), state) => {
      if (hasTask(state.list, task)) {
        stay.replying(TaskExistsErr)
      } else {
        val res = stay.applying(AddTaskEvt(task))
        if (ack) res.replying(AddTaskRsp(self.path.name, task))
        else res
      }
    }

    case Event(RemoveTaskFFCmd(task), state) =>
      if (hasOnlyTask(state.list, task)) goto(Empty).applying(ClearEvt)
      else if (hasDoneAllExcept(state.list, task))
        goto(Completed).applying(RemoveTaskEvt(task))
      else stay.applying(RemoveTaskEvt(task))

    case Event(MarkTaskCmd(task), state) => {
      if (hasOnlyTask(state.list, task))
        goto(Completed).applying(MarkTaskEvt(task))
      else if (hasDoneAllExcept(state.list, task))
        goto(Completed).applying(MarkTaskEvt(task))
      else stay.applying(MarkTaskEvt(task))
    }.replying(TaskStatusRsp(task, !isDone(state.list, task)))

    case Event(GetListCmd, state) => stay.replying(state.list)

    case Event(ClearFFCmd, _) =>
      goto(Empty).applying(ClearEvt)

    case Event(SaveListFFCmd, _) =>
      saveStateSnapshot()
      stay

    case Event(GetTodoStateCmd, _) =>
      stay.replying(ActiveStateRsp(self.path.name))

    case _ => stay

  }

  when(Completed) {

    case Event(AddTaskCmd(_, ack), _) => {
      val res = stay
      if (ack) res.replying(AllDoneRsp(self.path.name))
      else res
    }

    case Event(RemoveTaskFFCmd(_), _) => stay

    case Event(MarkTaskCmd(_), _) => stay.replying(AllDoneRsp(self.path.name))

    case Event(GetListCmd, state) => stay.replying(state.list)

    case Event(ClearFFCmd, _) =>
      goto(Empty).applying(ClearEvt)

    case Event(SaveListFFCmd, _) =>
      saveStateSnapshot()
      stay

    case Event(GetTodoStateCmd, _) => stay.replying(AllDoneRsp(self.path.name))

    case _ => stay
  }

  override def applyEvent(event: TaskEvt, state: ListState): ListState =
    event match {
      case AddTaskEvt(task) => ListState(addTask(state.list, task))
      case RemoveTaskEvt(task) =>
        ListState(removeTask(state.list, task))
      case MarkTaskEvt(task) => ListState(markTask(state.list, task))
      case ClearEvt          => ListState()
    }

}

object TodoPersistentFSM {

  def props(): Props = Props(new TodoPersistentFSM())

  sealed trait TodoState extends FSMState
  case object Empty extends TodoState {
    override def identifier: String = "Empty"
  }
  case object Active extends TodoState {
    override def identifier: String = "Active"
  }
  case object Completed extends TodoState {
    override def identifier: String = "Completed"
  }
}
