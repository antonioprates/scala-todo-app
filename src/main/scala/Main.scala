/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import akka.actor.ActorSystem
import api.TodoBook
import ui.Command

object Main extends App {

  println("\nTodoBook: Let's get some stuff done! ;)\n")

  // fire up the akka ActorSystem (context)
  implicit val context = ActorSystem("TodoBook")

  // create a TodoBook API on top of actorSystem
  // note: as TodoBook implements persistence, it
  // will recreate previous context automatically.
  val notes = new TodoBook

  // create UI for TodoBook instance using Command
  val terminalInterface = new Command(notes)

  // list recovered TaskLists from snapshot/replay
  terminalInterface.list()

  // print some help information
  terminalInterface.printHelp()

  // get and process user commands on Terminal
  do {
    print("> ")
    // do stuff while keepalive signal
  } while (terminalInterface.processCommand())

  // gracefully terminate
  Thread.sleep(5000)
  context.terminate()

  println("Goodbye :D")

}
