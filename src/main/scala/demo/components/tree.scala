package demo.components

import scala.scalajs.js
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all.{key, onClick}
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

  class Backend($: BackendScope[tree.Duck, String]) {
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
      val myRect: ReactTag = g(transform := "translate(0,10)",
                               rect(fill := "#eeeeee", width := 60, height := 30),
                               text(`class`:= "desc", transform := "translate(5, 20)", nameState))

      val nodes: js.Array[ReactTag] = tree.nodes map {
        node =>
          g(
            transform := move(node.point),
            circle(onClick ==> backend.blub(node.item.name),
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
          g(transform := "translate(80,50)", branches, nodes, myRect))
    })
    .build
}
