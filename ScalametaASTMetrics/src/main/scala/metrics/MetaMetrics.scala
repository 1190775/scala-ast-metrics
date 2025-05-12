package metrics

import scala.meta._
import java.nio.file._
import scala.jdk.CollectionConverters._
import scala.collection.mutable

final case class Fun(
                      psiz: Int,
                      don:  Int,
                      outd: Int,
                      outdDistinct: Int,
                      npvs: Int,
                      owner: String
                    )

object MetaMetrics {

  implicit val dial: Dialect = dialects.Scala3

  private def processFile(path: Path): List[Fun] = {
    val input = Input.File(path)
    input.parse[Source] match {
      case Parsed.Success(tree) =>
        val buf = mutable.ListBuffer.empty[Fun]

        object Walk extends Traverser {
          private var owners: List[String] = Nil

          override def apply(t: Tree): Unit = t match {
            case p: Pkg =>
              owners ::= p.ref.syntax
              super.apply(t)
              owners = owners.tail

            case c: Defn.Class =>
              owners ::= c.name.value
              super.apply(t)
              owners = owners.tail

            case o: Defn.Object =>
              owners ::= o.name.value
              super.apply(t)
              owners = owners.tail

            case d: Defn.Def
              if !d.mods.exists(_.is[Mod.Inline]) =>
              buf += analyse(d, owners.reverse.mkString("."))
              super.apply(t)


            case _ =>
              super.apply(t)
          }
        }

        Walk(tree)
        buf.toList

      case Parsed.Error(_, msg, _) =>
        Console.err.println(s"$path: $msg")
        Nil
    }
  }

  private def analyse(d: Defn.Def, owner: String): Fun = {
    /* PSIZ & DON */
    var nodes, curDepth, maxDepth = 0
    object SizeDepth extends Traverser {
      override def apply(t: Tree): Unit = {
        nodes    += 1
        curDepth += 1
        maxDepth  = math.max(maxDepth, curDepth)
        super.apply(t)
        curDepth -= 1
      }
    }
    SizeDepth(d.body)

    /* OUTD & OUTDd */
    val calls = mutable.ListBuffer.empty[String]
    object CallGrab extends Traverser {
      override def apply(t: Tree): Unit = t match {
        case Term.Apply(fun, _) =>
          calls += extractName(fun)
          super.apply(t)
        case _ => super.apply(t)
      }
    }
    CallGrab(d.body)

    /* NPVS */
    val names = mutable.Set.empty[String]
    /* parameters */
    d.paramss.flatten.foreach(p => names += p.name.value)

    /* local vals/vars & pattern-binds */
    object NameGrab extends Traverser {
      override def apply(t: Tree): Unit = t match {
        case Defn.Val(_, pats, _, _) =>
          pats.foreach { case Pat.Var(n) => names += n.value; case _ => () }
          super.apply(t)

        case Defn.Var(_, pats, _, _) =>
          pats.foreach { case Pat.Var(n) => names += n.value; case _ => () }
          super.apply(t)

        case Pat.Var(n) =>
          names += n.value
          super.apply(t)

        case _ => super.apply(t)
      }
    }
    NameGrab(d.body)

    Fun(
      psiz         = nodes,
      don          = maxDepth,
      outd         = calls.size,
      outdDistinct = calls.distinct.size,
      npvs         = names.size,
      owner        = owner
    )
  }

  private def extractName(t: Term): String = t match {
    case Term.Name(n)                     => n
    case Term.Select(_, Term.Name(n))     => n
    case _                                => t.syntax
  }

  private def format(fs: Seq[Fun]): String = {
    val n = if (fs.isEmpty) 1.0 else fs.size.toDouble

    def sumAvgMax(sel: Fun => Int): String = {
      val xs  = fs.map(sel)
      val sum = xs.sum
      val avg = sum / n
      val max = if (xs.isEmpty) 0 else xs.max
      f"sum=$sum%5d avr=$avg%6.1f max=$max%5d"
    }

    val ps  = sumAvgMax(_.psiz)
    val dn  = sumAvgMax(_.don)
    val od  = sumAvgMax(_.outd)
    val odd = sumAvgMax(_.outdDistinct)
    val np  = sumAvgMax(_.npvs)

    s"PSIZ($ps), DON($dn), OUTD($od), OUTDd($odd), NPVS($np)"
  }

  def main(args: Array[String]): Unit = {
    val root = Paths.get(
      "C:/Users/Utilizador/Desktop/PREPD/TAP/base/src/main"
    )

    val scalaFiles: List[Path] =
      Files.walk(root).iterator().asScala
        .filter(_.toString.endsWith(".scala"))
        .toList

    val funs: List[Fun] = scalaFiles.flatMap(processFile)

    funs.groupBy(_.owner).toSeq.sortBy(_._1).foreach {
      case (owner, fs) => println(s"$owner: " + format(fs))
    }

    println(format(funs))
  }
}
