/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */

import Todo._
import akka.persistence.PersistentActor
import akka.actor._
import akka.persistence._

class TodoActor extends PersistentActor {

  override def persistenceId: String = self.path.name

  private var state = ListState()

  def updateState(event: TaskEvt): Unit = {
    event match {
      case AddTaskEvt(task) => state = state.add(task)
      case MarkTaskEvt(index) => state = state.mark(index)
      case RemoveTaskEvt(task) => state = state.remove(task)
      case ClearEvt => state = state.clear()
    }
    context.system.eventStream.publish(event)
  }

  val receiveRecover: Receive = {
    case event: TaskEvt => updateState(event)
    case SnapshotOffer(_, snapshot: ListState) => state = snapshot
  }

  val receiveCommand: Receive = {

    case AddTaskFFCmd(task) => persist(AddTaskEvt(task)) { event =>
      updateState(event)
    }

    case AddTaskCmd(task) =>
      if (state.hasTask(task)) sender ! "Task already exist on list"
      else
        persist(AddTaskEvt(task)) { event =>
          updateState(event)
          sender ! s"'$task' added to ${self.path.name} list"
        }

    case MarkTaskCmd(index) =>
      if (state.isEmpty)
        sender ! s"${self.path.name} list has no tasks yet"
      else if (index > state.size | index < 1)
        sender ! s"Index $index is out of range for ${self.path.name} list, try between 1 and ${state.size}"
      else {
        val internalIndex = index - 1
        persist(MarkTaskEvt(internalIndex)) { event =>
          updateState(event)
          if (state.isDone(internalIndex))
            sender ! s"Task '${state.getTask(internalIndex)}' marked as done"
          else
            sender ! s"Task '${state.getTask(internalIndex)}' marked as incomplete"
        }
      }

    case PrintListCmd =>
      var list = ""
      state.fullList.zipWithIndex.foreach {
        case (task, index) =>
          if (task._1) list = list + s"${index + 1} - [X] ${task._2}\n"
          else list = list + s"${index + 1} - [ ] ${task._2}\n"
      }
      sender ! list

    case RemoveTaskCmd(index) =>
      if (state.isEmpty)
        sender ! s"${self.path.name} list has no tasks yet"
      else if (index > state.size | index < 1)
        sender ! s"Index $index is out of ${self.path.name} range, try a number from 1 to ${state.size}"
      else {
        val task = state.getTask(index - 1)
        persist(RemoveTaskEvt(task)) { event =>
          updateState(event)
          sender ! s"Task '$task' removed from ${self.path.name} list"
        }
      }

    case RemoveTaskFFCmd(task) =>
      persist(RemoveTaskEvt(task)) { event =>
        updateState(event)
      }

    case ClearFFCmd => persist(ClearEvt) { event =>
      updateState(event)
    }

    case GetPlainListCmd => sender ! state.fullList.map(entry => entry._2)

    case SaveListFFCmd => saveSnapshot(state)

  }

}
