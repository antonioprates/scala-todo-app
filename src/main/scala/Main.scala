/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object Main extends App {

  // create a akka context using TodoBook interface
  val notes = new TodoBook

  // create terminal interface using Command
  val terminalInterface = new Command(notes)

  // print some help information
  println(
    "\nTODOBOOK commands:" +
      "\nl 'list name' => Creates/selects a todo list by name" +
      "\nl (or ls)     => Lists all available todo lists" +
      "\na 'task'      => Adds a task to the selected list" +
      "\np (or print)  => Prints the selected list" +
      "\nm #number     => Marks/unmarks a task as done by its number" +
      "\nc (or clear)  => Clears whole selected list" +
      "\nx (or exit)   => Exits app" +
      "\n")

  while (terminalInterface.processCommand()) {
    // keep working
  }

  // gracefully terminate
  Thread.sleep(5000)
  notes.shutdown()
  println("Goodbye :D")

}
