package com.adventofcode.twentywentytwo

import com.adventofcode.twentywentytwo.Commands.{CommandParser, OutputParser}
import com.adventofcode.twentywentytwo.FileKind.{Directory, FileKind}
import com.adventofcode.{twentywentytwo => commands}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

final case class TreeNode(name: String, kind: FileKind, var children: Seq[TreeNode] = Seq.empty, var size: Long = -1)

final case class Command(query: CommandQuery, output: Seq[TreeNode])

sealed trait CommandQuery

case object ls extends CommandQuery

case class cd(dir: String) extends CommandQuery

object Day07 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromResource("inputs/day07.txt")) { source =>
      val iterator = source.getLines()
      val root = TreeNode("/", Directory)
      executeCommands(iterator, root)
      println(findDirSizes(root))

      val freeSpace = 70000000 - root.size
      val neededSpace = 30000000 - freeSpace
      println(findSmallestChildAtLeast(root, neededSpace))
    }
  }

  private def executeCommands(iterator: Iterator[String], root: TreeNode): Unit = {
    val currentPath = new mutable.Stack[TreeNode]
    currentPath.addOne(root)

    Commands.from(iterator).foreach { cmd =>
      cmd.query match {
        case commands.ls => currentPath.head.children = cmd.output
        case commands.cd("/") => while (currentPath.size > 1) currentPath.pop()
        case commands.cd("..") => currentPath.pop()
        case commands.cd(dir) =>
          val child = currentPath.head.children.filter(c => c.kind == Directory && c.name == dir).head
          currentPath.push(child)
      }
    }
  }

  private def findSmallestChildAtLeast(node: TreeNode, size: Long): Option[Long] = {
    if (!node.children.exists(_.kind == Directory)) return None
    val bestAmongChildren: Option[Long] = node.children
      .filter(_.kind == Directory)
      .flatMap(findSmallestChildAtLeast(_, size))
      .minOption

    Seq(Some(node.size), bestAmongChildren).flatten.filter(_ >= size).minOption
  }

  private def findDirSizes(node: TreeNode): Long = {
    val fromChildren = node.children.filter(_.kind == Directory).map(findDirSizes).sum
    node.size =  node.children.map(_.size).sum
    fromChildren + node.children.filter(_.kind == Directory).filter(_.size <= 100000).map(_.size).sum
  }
}

object FileKind extends Enumeration {
  type FileKind = Value
  val File, Directory = Value
}

class Commands(private val input: Iterator[String]) extends Iterator[Command] {
  private var lastLine: String = input.next()
  private val buffer: ListBuffer[String] = ListBuffer.empty

  override def hasNext: Boolean = lastLine != null

  override def next(): Command = {
    val cmdStr = lastLine
    buffer.clear()
    lastLine = input.next()
    while (input.hasNext && lastLine.isNotCommand) {
      buffer.addOne(lastLine)
      lastLine = input.next()
    }
    if (!input.hasNext && lastLine.isNotCommand) {
      buffer.addOne(lastLine)
      lastLine = null
    }

    Command(cmdStr.query, buffer.treeItems)
  }
}

object Commands {
  def from(input: Iterator[String]) = new Commands(input)

  implicit class CommandParser(str: String) {
    def isNotCommand: Boolean = !str.startsWith("$")

    val query: CommandQuery = if (str.trim == "$ ls") ls else cd(str.substring(4).trim)
  }

  implicit class OutputParser(lines: ListBuffer[String]) {
    def treeItems: Seq[TreeNode] = {
      lines.map(line => if (line.startsWith("dir "))
        TreeNode(
          name = line.substring(4).trim,
          kind = FileKind.Directory,
        )
      else
        TreeNode(
          name = line.substring(line.indexOf(' ')).trim,
          kind = FileKind.File,
          size = line.substring(0, line.indexOf(' ')).toLong,
        )
      ).toSeq
    }
  }
}
