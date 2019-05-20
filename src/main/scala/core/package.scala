import akka.actor.ActorRef

package object core {

  // a Task is a pair of boolean, indicating isDone, and a string, for description
  type Task = (Boolean, String)

  // a TaskList is just a List of Tasks
  type TaskList = List[Task]

  // a ListReference stores the name of a TodoList and a ActorRef for that TodoList
  type ListReference = (String, ActorRef)

  // a ContextIndex is a List of ListReferences, meaning a List of TodoLists
  type ContextIndex = List[ListReference]
}
