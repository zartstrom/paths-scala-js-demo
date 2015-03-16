package demo.components

import scala.scalajs.js
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all._

import panel._
import logo._
import piechart._
import radar._
import tree._
import bar._
import graph._


object toplevel {
  val countries = List(
    Country("Italy", 59859996),
    Country("Mexico", 118395054),
    Country("France", 65806000),
    Country("Argentina", 40117096),
    Country("Japan", 127290000)
  )
  val pokemons = List(
    Pokemon("Bulbasaur", 45, 49, 49, 65, 65, 45),
    Pokemon("Ivysaur", 60, 62, 63, 80, 80, 60),
    Pokemon("Venusaur", 80, 82, 83, 100, 100, 80),
    Pokemon("Kakuna", 45, 25, 50, 25, 25, 35),
    Pokemon("Chameleon", 58, 64, 58, 80, 65, 80),
    Pokemon("Squirtle", 40, 48, 65, 50, 64, 43),
    Pokemon("Blastoise", 79, 83, 100, 85, 105, 78),
    Pokemon("Butterfree", 60, 45, 50, 90, 80, 70)
  )
  val tree = Duck("Grandma", List(
    Duck("Eider", List(
      Duck("Fethry"), Duck("Abner")
    )),
    Duck("Daphne", List(
      Duck("Gladstone")
    )),
    Duck("Quackmore", List(
      Duck("Donald"),
      Duck("Della", List(
        Duck("Huey"), Duck("Dewey"), Duck("Louie")
      ))
    ))
  ))
  val stats = Stats(
    values = List(
      List(1, 2.0, 3, 4),
      List(2, 3.0, 1, 4),
      List(2, 2.5, 3, 3)
    ),
    labels = List("2009", "2010", "2011", "2012")
  )
  def randomGraph(n: Int, density: Double) = {
    val characters = (1 to n) map { x => Character(x.toString) }
    val links = characters.combinations(2) filter { _ =>
      js.Math.random() < density
    } map { case Vector(Character(a), Character(b)) =>
      Link(a, b, 3 + 5 * js.Math.random())
    }

    Family(characters = characters.toList, links = links.toList)
  }

  val TopLevel = ReactComponentB[Unit]("Top level component")
    .render(_ =>
      div(className := "container",
        div(className := "row", Logo(())),
        div(className := "row",
          Panel(PanelContent(
            id = Some("pie"),
            title = "Pie Chart",
            text = "Here is a pie chart example"
          ), PieChart(countries)),
          Panel(PanelContent(
            id = Some("radar"),
            title = "Radar Chart",
            text = "Here is a radar chart showing Pokémon stats. Try changing Pokémon."
          ), RadarChart(pokemons))
        ),
        div(className := "row",
          Panel(PanelContent(
            id = Some("tree"),
            title = "Tree Chart",
            text = "Here is part of the duck family tree."
          ), TreeChart(tree)),
          Panel(PanelContent(
            id = Some("bar"),
            title = "Bar Chart",
            text = "Here is a bar chart example."
          ), BarChart(stats))
        ),
        div(className := "row",
          Panel(PanelContent(
            id = Some("graph"),
            title = "Graph Chart",
            text = "A preliminary example of force-directed graph."
          ), GraphChart(randomGraph(30, 0.15)))
        )
      )
    )
    .build
}