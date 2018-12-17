package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Mkdir(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Directory " + name + " is already exists")
    } else if (name.contains(Directory.SEPARATOR)) {
      //mkdir smth/smth forbidden
      state.setMessage(name + " must not contain separators")
    } else if (checkIllegal(name)) {
      state.setMessage(name + " - Illegal entry name!")
    } else {
      doMkdir(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(state: State, name: String): State = {

    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        println(path)
        println(path.head)
        println(path.head.isEmpty)
        println(currentDirectory.findEntry(path.head).asDirectory)
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry  ))

      }
    }

    val wd = state.wd
    val fullPath = wd.path

    // 1. all the directories in the fullpath

    val allDirsInPath = wd.getAllFoldersInThePath

    // 2. create new directory entry in the wd

      val newDir = Directory.empty(wd.path, name)

    // 3. update the whole directory structure starting from the root
    //  (the directory structure is IMMUTABLE)

    val newRoot = updateStructure(state.root, allDirsInPath, newDir)

    // 4. find the new working directory INSTANCE given wd's full path, in the NEW directory structure

    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)

  }
}
