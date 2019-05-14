import akka.actor.{ActorRef, ActorSystem, Props}

// TodoBook class stores actor references with select and clear simple interface
class TodoBook {

  private val system = ActorSystem("TodoBook")

  var lists = List[(String, ActorRef)]()

  def select = (listName: String) => {
    lists.find(item => item._1 == listName) match {
      case None => {
        val todoActor = this.system.actorOf(Props[TodoActor], name = listName)
        lists = lists :+ (listName, todoActor)
        println(s"${listName} Todo list was created.")
        todoActor
      }
      case item: Option[(String, ActorRef)] => {
        println(s"${listName} Todo list is selected.")
        item.get._2
      }
    }
  }

  def clear =
    (actor: ActorRef) => {
      val listName: Unit = actor ! "get name"
      println(s"Clearing ${listName}...")
      lists = lists.filter(item => item._1 != listName)
      system.stop(actor)
    }

  def shutdown = () => {

    system.terminate()
  }
}

class Command(val notebook: TodoBook) {

  var selectedActor = None: Option[ActorRef]

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }

  //change to def with no args
  def process = () => {

    val line = this.getLine()

    this.getCommand(line) match {

      case ('s', listName: String) => {
        if (listName.length > 0) {
          selectedActor = Some(notebook.select(listName))
        } else {
          println("[Error] Invalid list name!")
        }
        true
      }

      case ('a', task: String) => {
        selectedActor match {
          case Some(list: ActorRef) => list ! ("add task", task)
          case None                 => println("[Error] No list selected!")
        }
        true
      }

      case ('l', _) => {
        selectedActor match {
          case Some(list: ActorRef) => list ! "view list"
          case None                 => println("[Error] No list selected!")
        }
        true
      }

      case ('m', strIndex: String) => {
        toInt(strIndex) match {
          case Some(index: Int) =>
            selectedActor match {
              case Some(list: ActorRef) => list ! ("mark", index)
              case None                 => println("[Error] No list selected!")
            }
          case None => println("[Error] Invalid number provided!")
        }
        true
      }

      case ('c', _) => {
        selectedActor match {
          case Some(actor: ActorRef) => {
            notebook.clear(actor)
            selectedActor = None: Option[ActorRef]
            println("Done removing list.")
          }
          case None => println("[Error] No list selected!")
        }
        true
      }

      case ('x', _) | ('e', _) => {
        println(s"Exiting TodoBook...")
        false
      }

      case _ => {
        println("[Error] Unknown command!")
        true
      }
    }
  }

  def getLine = () => scala.io.StdIn.readLine()

  def getCommand =
    (line: String) =>
      line.length match {
        case 0 => (' ', "")
        case 1 => (line(0), "")
        case 2 => (line(0), "")
        case _ => (line(0), line.drop(2))
    }

}

object Main extends App {

  // create a list of actors using TodoBook
  val notepad = new TodoBook

  // create terminal interface using Command
  val terminalInterface = new Command(notepad)

  // print some help information
  println(
    "\n[TodoBook] commands:" +
      "\ns 'list name' => select a list by name (no spaces)" +
      "\na 'todo'      => add entry to list" +
      "\nl or list     => list entries of selected list" +
      "\nm #number     => mark/unmark a entry as done by it's number" +
      "\nc or clear    => remove/clear whole selected list" +
      "\nx or exit     => exit")

  while (terminalInterface.process()) {}

  Thread.sleep(5000)
  notepad.shutdown()

}
