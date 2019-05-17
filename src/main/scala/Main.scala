/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object Main extends App {

  println("\n\nTodoBook: Let's get some stuff done! ;)\n")

  // create a akka context using TodoBook interface
  val notes = new TodoBook

  // print recovered list
  notes.list(None)

  // create terminal interface using Command
  val terminalInterface = new Command(notes)

  // print some help information
  terminalInterface.printHelp()

  while (terminalInterface.processCommand()) {
    // keep working
  }

  // gracefully terminate
  Thread.sleep(5000)
  notes.shutdown()
  println("Goodbye :D")

}
