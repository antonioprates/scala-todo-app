/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import Todo.{AddTaskCmd, AddTaskEvt, GetPlainListCmd, ListState, MarkTaskCmd, MarkTaskEvt, PrintListCmd, RemoveTaskCmd, RemoveTaskEvt, SaveListCmd, TaskEvt}
import akka.persistence.PersistentActor
import akka.actor._
import akka.persistence._

import scala.collection.script.Remove

class TodoActor extends PersistentActor {

  override def persistenceId: String = self.path.name

  private var state = ListState()

  def updateState(event: TaskEvt): Unit = {
    event match {
      case AddTaskEvt(task)   => state = state.add(task)
      case MarkTaskEvt(index) => state = state.mark(index)
      case RemoveTaskEvt(index) => state = state.remove(index)
    }
  }

  val receiveRecover: Receive = {
    case event: TaskEvt                        => updateState(event)
    case SnapshotOffer(_, snapshot: ListState) => state = snapshot
  }

  val receiveCommand: Receive = {

    case AddTaskCmd(task) =>
      if (state.hasTask(task)) sender ! "Task already exist on list"
      else
        persist(AddTaskEvt(task)) { event =>
          updateState(event)
          context.system.eventStream.publish(event)
          sender ! s"'$task' added to ${self.path.name} list"
        }

    case MarkTaskCmd(index) =>
      if (state.isEmpty)
        println(s"${self.path.name} list has no tasks yet")
      else if (index > state.size | index < 1)
        println(s"[error] Index $index is out of range!")
      else {
        val internalIndex = index - 1
        persist(MarkTaskEvt(internalIndex)) { event =>
          updateState(event)
          context.system.eventStream.publish(event)
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
      val internalIndex = index - 1
      persist(RemoveTaskEvt(internalIndex)) { event =>
      updateState(event)
      context.system.eventStream.publish(event)

    }

    case GetPlainListCmd => sender ! state.fullList.map(entry => entry._2)

    case SaveListCmd => saveSnapshot(state)

  }

}
