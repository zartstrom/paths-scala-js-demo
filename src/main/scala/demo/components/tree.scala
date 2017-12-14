package demo.components

import scala.scalajs.js
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all.{a, href, target, key, onClick}
import japgolly.scalajs.react.vdom.svg.all._
// import japgolly.scalajs.react.vdom.svg.all
import paths.high.{Tree, TreeNode}
import demo.colors._

object tree {
  case class Duck(name: String, descendants: List[Duck] = List())
  private def move(p: js.Array[Double]) = s"translate(${p(0)},${p(1)})"
  private def isLeaf(duck: Duck) = duck.descendants.length == 0
  private def circleColor(name: String): String =
    if (name == "Grandma") "blue" else "#dddddd"

  val displayName = "Hans"

  class Backend($ : BackendScope[tree.Duck, String]) {
    def blub(name: String): Any => Unit = { x: Any =>
      println(name)
      $.setState(name)
    }
    /*
    def randomize(ps: Points) = ps map { case (x, y) => (x, y - 25 + 50 * js.Math.random()) }
    def onClick(e: ReactEventI) = $.animateModState(randomize, AnimateOptions(done = goBack))
    def goBack(u: Unit) = $.animateState(points, AnimateOptions(easing = Easing.easeOutElastic))
   */
  }

  def nodeRepr(node: TreeNode[Duck]): String = {
    node.item.name + "(" + node.point.toString + ")"
  }

  val TreeChart = ReactComponentB[Duck]("Tree chart")
    .initialState(displayName)
    .backend(new Backend(_))
    .render((ducks, nameState, backend) => {
      val tree = Tree[Duck](
        data = ducks,
        children = _.descendants,
        width = 300,
        height = 300
      )

      val branches = tree.curves map { curve =>
        path(d := curve.connector.path.print)
      }
      val treeNodes: js.Array[TreeNode[Duck]] = tree.nodes
      val head: TreeNode[Duck] = treeNodes.head
      val myRect: ReactTag =
        g(transform := "translate(0,10)",
          rect(fill := "#eeeeee", width := 60, height := 30),
          text(`class` := "desc", transform := "translate(5, 20)", nameState))
      val myLink: ReactTag = g(
        transform := "translate(0,10)",
        a(text("brabbel"), href := "http://www.github.com", target := "_blank"))

      val nodes: js.Array[ReactTag] = tree.nodes map {
        node: TreeNode[Duck] =>
          g(
            transform := move(node.point),
            circle(onClick ==> backend.blub(nodeRepr(node)),
                   r := 30,
                   cx := 0,
                   cy := 0,
                   fill := circleColor(node.item.name)),
            text(
              transform := (if (isLeaf(node.item)) "translate(10,0)"
                            else "translate(-10,0)"),
              textAnchor := (if (isLeaf(node.item)) "start" else "end"),
              node.item.name
            )
          )
      }

      svg(width := 460,
          height := 400,
          g(transform := "translate(80,50)", branches, nodes, myRect, myLink))
    })
    .build

  def movie(t: Int, tmax: Int, tmin: Int): String = {
    val padding = 40
    val width = 450
    val ynew = 200
    val xnew = padding + ((width - 2 * padding) * (t - tmin).toFloat / (tmax - tmin).toFloat).toInt
    s"translate(${xnew},${ynew})"
  }

  case class Trace(timestamp: Int, referer: String, url: String)
  val firstTrace =
    (1,
     "https://www.google.de/search?q=kühlschrank+kaufen&oq=kühlschrank",
     "https://www.idealo.de/preisvergleich/ProductCategory/2800.html")
  class Backend2($ : BackendScope[List[Trace], (Int, String, String)]) {
    def blub(trace: Trace): Any => Unit = { x: Any =>
      $.setState((3, trace.referer, trace.url))
    }
  }

  val TreeChart2 = ReactComponentB[List[Trace]]("Traces")
    .initialState(firstTrace)
    .backend(new Backend2(_))
    .render((traces, firstTrace, backend) => {
      val myRect: ReactTag =
        g(transform := "translate(0,10)",
          rect(fill := "#eeeeee", width := 60, height := 30),
          text(`class` := "desc",
               transform := "translate(5, 20)",
               firstTrace.toString))
      val myLink: ReactTag = g(
        transform := "translate(0,10)",
        a(text("brabbel"), href := "http://www.github.com", target := "_blank"))

      val maxTime = traces.map(x => x.timestamp).max
      val minTime = traces.map(x => x.timestamp).min

      // val nodes: js.Array[ReactTag] = traces.map({
      val nodes: List[ReactTag] = traces.map({
        trace: Trace =>
          g(
            transform := movie(trace.timestamp, maxTime, minTime),
            circle(onClick ==> backend.blub(trace),
              r := 30,
              cx := 0,
              cy := 0
            ),
            text(
              transform := "translate(10,0)",
              textAnchor := "end",
              trace.url
            )
          )
      })

      svg(width := 460,
          height := 400,
          g(transform := "translate(0,0)", nodes, myRect, myLink))

    })
    .build

  val sampleTraces = List(
    Trace(2, "a", "b"),
    Trace(5, "b", "c"),
    Trace(8, "c", "d")
  )

  val sampleTraces2 = List(
    Trace(2, "a", "b"),
    Trace(3, "b", "c"),
    Trace(6 , "c", "d"),
    Trace(7, "d", "e")
  )
}
