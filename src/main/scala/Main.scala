object Main extends App {

  // create a akka context using TodoBook interface
  val notes = new TodoBook

  // create terminal interface using Command
  val terminalInterface = new Command(notes)

  // print some help information
  println(
    "\nTODOBOOK commands:" +
      "\ns 'list name' => select a todo list by name" +
      "\ns (empty)     => lists all available todo lists" +
      "\na 'task'      => adds a task to the selected list" +
      "\nl (or list)   => prints the selected list" +
      "\nm #number     => marks/unmarks a task as done by its number" +
      "\nc (or clear)  => clears whole selected list" +
      "\nx (or exit)   => exits app" +
      "\n")

  // work
  while (terminalInterface.process()) {}

  // gracefully terminate
  Thread.sleep(5000)
  notes.shutdown()
  println("Goodbye :D")

}
