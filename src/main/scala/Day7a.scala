import scala.collection.immutable.Queue
import scala.annotation.tailrec
import scala.io.Source
object Day7a:

  //val input = Source.fromFile("day6.in.txt").mkString

  def main(args: Array[String]): Unit =
    println("Hello Day7a!")
    //val lines = Source.fromString(input).getLines()
    val lines = Source.fromFile("day7.in.txt").getLines()
    val commands = lines.foldLeft(Vector.empty[(String, Vector[String])]) {
      (cmds, line) =>
        if line.startsWith("$ ") then
          cmds.appended((line.drop(2), Vector.empty))
        else
          cmds.lastOption
            .map((cmd, lines) => (cmd, lines.appended(line)))
            .map(last => cmds.updated(cmds.length-1, last))
            .getOrElse(cmds)
          
    }
    val tree = commands.foldLeft(Dir("/", None)) {
      case (curDir, (command, commandOut)) =>
        command match
          case cmd if cmd.startsWith("cd ") =>
            cmd.drop(3) match
              case ".." =>
                curDir.parent.getOrElse(curDir)
              case "/" =>
                curDir.root
              case dirname if dirname.nonEmpty =>
                curDir.dirs.find(_.name == dirname) match
                  case Some(dir) => dir
                  case None =>
                    throw new IllegalStateException(s"Dir $dirname not found.") 
              case _ => throw new IllegalStateException("No arg for cd command.") 
            
          case "ls" =>
            val words = commandOut.map(_.split("\\s+"))
            require(words.forall(_.length == 2))
            val (dirWords, fileWords) = words.partition(a => a(0) == "dir")
            val dirs = dirWords.map(word => Dir(word(1), Some(curDir)))
            val files = fileWords.map(fdesc => File(fdesc(1), fdesc(0).toLong, Some(curDir)))
            curDir.dirs = dirs
            curDir.files = files
            curDir

          case _ => throw new IllegalStateException("Unknown command.") 
    }
    val root = tree.root
    println(s"root size: ${root.calculateTotalSize}")
    val filtered = root.findAll(_.calculateTotalSize <= 100000)
    println(s"found ${filtered.length} matching dirs")
    println(s"found ${filtered.map(_.calculateTotalSize).sum} total bytes")
    println(s"root size: ${root.calculateTotalSize}")
    
  class Dir(val name: String, val parent: Option[Dir]):
    var dirs: Vector[Dir] = Vector.empty
    var files: Vector[File] = Vector.empty
    private var sizeMemo: Option[Long] = None

    def calculateTotalSize: Long =
      sizeMemo.getOrElse {
        val total = files.map(_.size).sum + dirs.map(_.calculateTotalSize).sum
        sizeMemo = Some(total)
        total
      }
    
    def findAll(pred: Dir => Boolean): Vector[Dir] =
      dirs.flatMap(_.findAll(pred))
        :++ (if pred(this) then Vector(this) else Vector.empty)

    def ls: String = 
      s">> Dir: $name ($fullPath)\n" +
      //s">> Parent: ${parent.map(_.name).getOrElse("<none>")}\n" +
      //s">> GrandParent: ${parent.flatMap(_.parent).map(_.name).getOrElse("<none>")}\n" +
      s"Files: ${files.map(f => s"${f.name}/${f.size}")}\n" +
      s"Dirs: ${dirs.map(_.name)}\n" +
      dirs.map(_.ls).mkString

    def root: Dir =
      @tailrec
      def next(dir: Dir): Dir = dir.parent match
        case None => dir
        case Some(par) => next(par)
      next(this)

    def fullPath: String = 
      @tailrec
      def next(dir: Dir, parts: List[String]): String =
        dir.parent match
          case None => (dir.name :: parts).mkString("/")
          case Some(p) => next(p, dir.name :: parts)
      next(this, Nil).drop(1) // because the name of root is really ""
  
  case class File(name: String, size: Long, parent: Option[Dir])

  val input = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""
