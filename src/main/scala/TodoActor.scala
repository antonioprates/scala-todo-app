import akka.actor.Actor


class TodoActor extends Actor {

  private var tasks: List[(String, Boolean)] = List()


  /*can change to def method
  * def toggleTask(index: Int) = {body}*/
  private def toggleTask = (index: Int) => {
    val flippedItem = (tasks(index)._1, !tasks(index)._2)
    if (flippedItem._2) println(flippedItem._1 + " marked as done.")
    else println(flippedItem._1 + " marked as incomplete.")
    tasks.updated(index, flippedItem)
  }

  def receive = {

    case "get name" => {


      self.path.name
    }

    case ("add task", task: String) => {
      println(s"adding ${task} to ${self.path.name} list")
      tasks = tasks :+ (task, false)
    }

    case "view list" => {
      if (tasks.length == 0) println(s"List ${self.path.name} is empty.")
      else {
        println(s"Listing content of ${self.path.name}...")
        tasks.zipWithIndex.foreach {
          case (task, index) => {
            if (task._2) println((index + 1) + " - [X] " + task._1)
            else println((index + 1) + " - [ ] " + task._1)
          }
        }
      }
    }

    case ("mark", index: Int) => {
      if (tasks.length == 0)
        println(s"[Error] List ${self.path.name} has no tasks yet.")
      else if (index > tasks.length)
        println(s"[Error] Index ${index} is out of range.")
      else if (index == 0)
        println(s"[Error] Invalid index provided.")
      else tasks = toggleTask(index - 1)
    }

    case "hello" => {
      println(s"hello back at you from ${self.path.name} list")
    }

    case _ => {
      println("huh?")
    }

  }
}
