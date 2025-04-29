package metrics

import scala.quoted.*
import scala.tasty.inspector.*
import java.nio.file.*
import scala.jdk.StreamConverters.*
import scala.collection.mutable

object SimpleInspector extends Inspector:

  private case class Fun(
                          psiz: Int,
                          don:  Int,
                          outd: Int,
                          outdDistinct: Int,
                          npvs: Int,
                          owner: String
                        )

  override def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*

    val funs = mutable.ListBuffer.empty[Fun]

    tastys.foreach { tasty =>
      object Grab extends TreeTraverser:
        override def traverseTree(t: Tree)(owner: Symbol): Unit = t match
          case d: DefDef
            if !d.symbol.flags.is(Flags.Synthetic) && d.rhs.nonEmpty =>
            funs += analyse(d)
          case _ => super.traverseTree(t)(owner)
      Grab.traverseTree(tasty.ast)(Symbol.noSymbol)
    }

    // Group by File
    val byOwner = funs.groupBy(_.owner).view.toSeq.sortBy(_._1)
    byOwner.foreach { (owner, fs) =>
      println(s"${owner}: " + format(fs.toSeq))
    }

    // Global total
    println("format(funs.toSeq)")

  private def format(fs: Seq[Fun]): String =
    val n        = fs.size
    val asDouble = if n == 0 then 1.0 else n.toDouble

    def SumAverageMax(sel: Fun => Int): String =
      val nums   = fs.map(sel)
      val sum    = nums.sum
      val avr    = sum.toDouble / asDouble
      val maxVal = if nums.isEmpty then 0 else nums.max
      f"sum=$sum%5d avr=$avr%6.1f max=$maxVal%5d"

    val ps  = SumAverageMax(_.psiz)
    val dn  = SumAverageMax(_.don)
    val od  = SumAverageMax(_.outd)
    val odd = SumAverageMax(_.outdDistinct)
    val np  = SumAverageMax(_.npvs)

    s"PSIZ($ps), DON($dn), OUTD($od), OUTDd($odd), NPVS($np)"

  private def analyse(using Quotes)(d: quotes.reflect.DefDef): Fun =
    import quotes.reflect.*

    // PSIZ and DON
    var nodes = 0
    var curDepth = 0
    var maxDepth = 0

    object Walker extends TreeTraverser:
      override def traverseTree(t: Tree)(owner: Symbol): Unit =
        nodes += 1
        curDepth += 1
        maxDepth = math.max(maxDepth, curDepth)
        super.traverseTree(t)(owner)
        curDepth -= 1

    d.rhs.foreach(Walker.traverseTree(_)(d.symbol))

    // OUTD / OUTDd
    val calls = mutable.ListBuffer.empty[String]
    object CallGrab extends TreeTraverser:
      override def traverseTree(t: Tree)(owner: Symbol): Unit = t match
        case a: Apply =>
          calls += a.fun.symbol.fullName
          super.traverseTree(t)(owner)
        case _ => super.traverseTree(t)(owner)
    d.rhs.foreach(CallGrab.traverseTree(_)(d.symbol))

    // NPVS
    val names = mutable.Set.empty[String]
    // parameters
    d.paramss.flatMap(_.params).foreach(vd => names += vd.name)
    // local & pattern-binds
    object NameGrab extends TreeTraverser:
      override def traverseTree(t: Tree)(owner: Symbol): Unit = t match
        case v: ValDef =>
          names += v.name
          super.traverseTree(t)(owner)
        case b: Bind   =>
          names += b.name
          super.traverseTree(t)(owner)
        case _         =>
          super.traverseTree(t)(owner)
    d.rhs.foreach(NameGrab.traverseTree(_)(d.symbol))

    Fun(
      psiz         = nodes,
      don          = maxDepth,
      outd         = calls.size,
      outdDistinct = calls.distinct.size,
      npvs         = names.size,
      owner        = d.symbol.owner.fullName
    )

// Main
@main def runHardcoded(): Unit =
  val root = Paths.get(
    "C:/Users/Utilizador/Desktop/PREPD/TAP/base/target/scala-3.3.3/classes"
  )
  val tastyFiles = Files.walk(root)
    .filter(_.toString.endsWith("XMLReader.tasty"))
    .toScala(List)
    .map(_.toString)

  TastyInspector.inspectAllTastyFiles(tastyFiles, Nil, Nil)(SimpleInspector)
