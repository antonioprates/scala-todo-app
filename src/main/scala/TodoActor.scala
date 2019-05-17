/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import Todo._

import akka.actor._
import akka.persistence._

class TodoActor extends PersistentActor {

  override def persistenceId: String = self.path.name

  private var state = ListState()

  def updateState(event: TaskEvt): Unit = {
    event match {
      case AddTaskEvt(task)    => state = state.add(task)
      case MarkTaskEvt(task)   => state = state.mark(task)
      case RemoveTaskEvt(task) => state = state.remove(task)
      case ClearEvt            => state = state.clear()
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
      if (state.hasTask(task)) {
        if (ack) sender ! TaskExistsErr
      } else
        persistEvent(AddTaskEvt(task)) { _ =>
          if (ack) sender ! AddTaskRsp(self.path.name, task)
        }

    case MarkTaskCmd(task) =>
      persistEvent(MarkTaskEvt(task)) { _ =>
        sender ! TaskStatusRsp(task, state.isDone(task))
      }

    case GetListCmd => sender ! state.fullList

    case RemoveTaskFFCmd(task) =>
      persistEvent(RemoveTaskEvt(task)) { _ =>
        }

    case ClearFFCmd =>
      persist(ClearEvt) { event =>
        updateState(event)
        context.system.eventStream.publish(event)
        self ! PoisonPill
      }

    case SaveListFFCmd => saveSnapshot(state)

  }

}
