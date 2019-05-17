/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object Main extends App {

  println("\n\nTodoBook: Let's get some stuff done! ;)\n")

  // create a akka context using TodoBook API
  val notes = new TodoBook

  // list recovered TaskLists (with none selected)
  notes.list(None)

  // create UI for TodoBook instance using Command
  val terminalInterface = new Command(notes)

  // print some help information
  terminalInterface.printHelp()

  // get user commands on Terminal
  while (terminalInterface.processCommand()) {
    // do stuff while keepalive is true
  }

  // gracefully terminate
  Thread.sleep(5000)
  notes.shutdown()

  println("Goodbye :D")

}
