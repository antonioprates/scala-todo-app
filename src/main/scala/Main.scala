/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import akka.actor.ActorSystem
import api.TodoBook
import ui.Command

object Main extends App {

  println("\n\nTodoBook: Let's get some stuff done! ;)\n")

  implicit val actorSystem = ActorSystem("TodoBook")

  // creates a TodoBook API on top of actorSystem
  val notes = new TodoBook

  // create UI for TodoBook instance using Command
  val terminalInterface = new Command(notes)

  // list recovered TaskLists from snapshot/replay
  terminalInterface.list()

  // print some help information
  terminalInterface.printHelp()

  // get an process user commands on Terminal
  while (terminalInterface.processCommand()) {
    // do stuff while keepalive signal
  }

  // gracefully terminate
  Thread.sleep(5000)
  notes.shutdown()

  println("Goodbye :D")

}
