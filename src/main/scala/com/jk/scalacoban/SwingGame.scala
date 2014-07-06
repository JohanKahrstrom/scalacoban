package com.jk.scalacoban

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}
import scala.util.Random

class DataPanel(data: Array[Array[Color]]) extends Panel {
  val r = new Random()

  override def paintComponent(g: Graphics2D) {
    val dx = g.getClipBounds.width.toFloat  / data.length
    val dy = g.getClipBounds.height.toFloat / data.map(_.length).max
    for {
      x <- 0 until data.length
      y <- 0 until data(x).length
      x1 = (x * dx).toInt
      y1 = (y * dy).toInt
      x2 = ((x + 1) * dx).toInt
      y2 = ((y + 1) * dy).toInt
    } {
      data(x)(y) match {
        case c: Color => g.setColor(c)
        case _ => g.setColor(new Color(r.nextFloat(), 0f, 0f))
      }
      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }
}

object Draw extends SimpleSwingApplication {

  val data = Array.ofDim[Color](25, 25)

  // plot some points
  data(0)(0) = Color.BLACK
  data(4)(4) = Color.RED
  data(0)(4) = Color.GREEN
  data(4)(0) = Color.BLUE

  // draw a circle
  import math._
  {
    for {
      t <- Range.Double(0, 2 * Pi, Pi / 60)
      x = 12.5 + 10 * cos(t)
      y = 12.5 + 10 * sin(t)
      c = new Color(0.5f, 0f, (t / 2 / Pi).toFloat)
    } data(x.toInt)(y.toInt) = c
  }

  def top = new MainFrame {
    contents = new DataPanel(data) {
      preferredSize = new Dimension(300, 300)
    }
  }
}