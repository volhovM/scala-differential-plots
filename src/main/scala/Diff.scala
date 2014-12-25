package me.volhovm.differentialplots

import scalax.chart.api._
import math._

object Diff {
  def main(args: Array[String]) = {
    createChart(euler(0, 2, Seq(
                        (((_, ys) => cos(ys(0) * ys(1))), 0, "cos(y1 * y2)"),
                        (((_, ys) => sin(ys(0) + ys(1))), 0, "sin(y1 + y2)")
                      ), 0.01), "Приближенное решение задачи Коши методом Эйлера")
    createChart(rgk(-1, 1, Seq(
                      ((x, ys) => x / sqrt(1 + pow(x, 2) + pow(ys(1), 2)), 0.2,
                        "x / sqrt(1 + x^2 + y2^2)"),
                      (((x, ys) => ys(1) / sqrt(1 + pow(x, 2) + pow(ys(0), 2)), 0,
                        "y2 / sqrt(1 + x^2 + y1^2)"))
                      ), 0.01), "Приближенное решение задачи Коши методом Рунге-Кутта")
  }

  def euler2d(a: Double, b: Double, f1: Double, f2: Double,
            y1: (Double, Double, Double) => Double,
            y2: (Double, Double, Double) => Double,
            step: Double): Seq[(Double, (Double, Double))] =
    (a to b by step) zip (a + step to b + step by step)
      .scanLeft((f1, f2))((pair, x) => (pair._1 + step * y1(x - step, pair._1, pair._2),
                                        pair._2 + step * y2(x - step, pair._1, pair._2)))

  type Func = (Double, Seq[Double]) => Double

  def euler(a: Double, b: Double,
              f: Seq[(Func, //foo
                      Double, // foo(a)
                      String)], // descr
              step: Double): Seq[(String, Seq[(Double, Double)])] =
    (a + step to b + step by step)
      .scanLeft(f.map(x => (a, x._2, x._1, x._3)))(
      (values, x) => values.map(prev =>
        (x, prev._2 + step * prev._3(prev._1, values.map(x => x._2)), prev._3, prev._4)))
      .foldLeft(f.map(_ => ("", Seq[(Double, Double)]())))((a, b) =>
      (a, b).zipped map ((x, y) => (y._4, x._2 :+ ((y._1, y._2)))))

  def rgk(a: Double, b: Double,
              f: Seq[(Func, //foo
                      Double, // foo(a)
                      String)], // descr
              step: Double): Seq[(String, Seq[(Double, Double)])] =
    (a + step to b + step by step)
      .scanLeft(f.map(x => (a, x._2, x._1, x._3)))(
      (values, x) => values.map(prev =>
        (x,
         prev._2 + (step / 6) * ({
                                   val k1 = prev._3(prev._1,
                                                    values.map(x => x._2))
                                   val k2 = prev._3(prev._1 + step / 2,
                                                    values.map(x => x._2 + step * k1 / 2))
                                   val k3 = prev._3(prev._1 + step / 2,
                                                    values.map(x => x._2 + step * k2 / 2))
                                   val k4 = prev._3(prev._1 + step,
                                                    values.map(x => x._2 + step * k3))
                                   k1 + 2 * k2 + 2 * k3 + k4
                                 }
         ),
         prev._3,
         prev._4)))
      .foldLeft(f.map(_ => ("", Seq[(Double, Double)]())))((a, b) =>
      (a, b).zipped map ((x, y) => (y._4, x._2 :+ ((y._1, y._2)))))

  def createChart(charts: Seq[(String, Seq[(Double, Double)])],
                  chartsTitle: String) =
      XYLineChart(charts.toXYSeriesCollection(true),
                  title = chartsTitle).saveAsPNG("./" + chartsTitle + ".png")
}
