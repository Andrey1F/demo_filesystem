package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {

  override def apply(state: State): State = {
    /*
    cd /smth/smthelse/.../
    cd a/b/c = relative to the current
     */

    //find root

    val root = state.root
    val wd = state.wd

    //find abs path of the dir we want cd to

    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if(wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir



    //find the dir to cd to

    val destinationDirectory = doFindEntry(root, absolutePath)

    //change the state given the new dir
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + "no such directory")
    else
      State(root, destinationDirectory.asDirectory)
  }

  def doFindEntry(root: Directory, path: String): DirEntry = {

    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry =
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }

    /*

     */
    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)) {
        if (result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    //tokens
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

    //collapse relative tokemkdirns

    val newTokens = collapseRelativeTokens(tokens, List())


    //
    if (newTokens == null) null
    else findEntryHelper(root, newTokens)
  }


}
