package core

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
import TaskBehavior._

import org.scalatest.FunSuite

class TaskBehaviorTest extends FunSuite {
  val exampleTask1: Task = (false, "Example 1")
  val exampleTask1M: Task = (true, "Example 1")
  val exampleTask2: Task = (false, "Example 2")
  val exampleTask2M: Task = (true, "Example 2")
  val exampleTask3: Task = (false, "Example 3")
  val exampleTask4: Task = (false, "Example 4")
  val emptyList: TaskList = Nil
  val exampleList: TaskList = List(exampleTask1, exampleTask2, exampleTask3)
  val exampleListA: TaskList =
    List(exampleTask1, exampleTask2, exampleTask3, exampleTask4)
  val exampleListM: TaskList = List(exampleTask1M, exampleTask2, exampleTask3)
  val exampleListD: TaskList = List(exampleTask1M, exampleTask2M)
  val exampleListZ: TaskList = List(exampleTask1M, exampleTask2M, exampleTask3)

  test("TaskBehavior.hasTask") {
    assert(hasTask(emptyList, "Any Task") === false)
    assert(hasTask(exampleList, "Example 2") === true)
    assert(hasTask(exampleList, "Missing") === false)
  }

  test("TaskBehavior.hasOnlyTask") {
    assert(hasOnlyTask(emptyList, "Any Task") === false)
    assert(hasOnlyTask(List(exampleTask1), "Example 1") === true)
    assert(hasOnlyTask(exampleList, "Example 1") === false)
  }

  test("TaskBehavior.isDone") {
    assert(isDone(emptyList, "Any Task") === false)
    assert(isDone(exampleListM, "Example 1") === true)
    assert(isDone(exampleListM, "Example 3") === false)
    assert(isDone(exampleListM, "Missing") === false)
  }

  test("TaskBehavior.hasDoneAll") {
    assert(hasDoneAll(emptyList) === false)
    assert(hasDoneAll(exampleListM) === false)
    assert(hasDoneAll(exampleListD) === true)
  }

  test("TaskBehavior.hasDoneAllExcept") {
    assert(hasDoneAllExcept(emptyList, "Any Task") === false)
    assert(hasDoneAllExcept(exampleListD, "Example 2") === false)
    assert(hasDoneAllExcept(exampleListZ, "Example 3") === true)
  }

  test("TaskBehavior.addTask") {
    assert(addTask(emptyList, "Example 2") === List(exampleTask2))
    assert(addTask(exampleList, "Example 4") === exampleListA)
  }

  test("TaskBehavior.markTask") {
    assert(markTask(emptyList, "Any Task") === emptyList)
    assert(markTask(exampleList, "Example 1") === exampleListM)
    assert(markTask(exampleListM, "Example 1") === exampleList)
    assert(markTask(exampleListA, "Missing") === exampleListA)
  }

  test("TaskBehavior.removeTask") {
    assert(removeTask(emptyList, "Example 2") === emptyList)
    assert(removeTask(exampleListA, "Example 4") === exampleList)
    assert(removeTask(exampleListA, "Missing") === exampleListA)
  }

  test("Misc. Interpolated TaskBehaviors") {
    assert(
      markTask(markTask(exampleList, "Example 1"), "Example 1") === exampleList)
    assert(
      markTask(addTask(emptyList, "Example 1"), "Example 1") === List(
        exampleTask1M))
    val wip = addTask(addTask(addTask(emptyList, "Example 1"), "Example 2"),
                      "Remove Me")
    assert(addTask(markTask(removeTask(wip, "Remove Me"), "Example 1"),
                   "Example 3") === exampleListM)
  }
}
