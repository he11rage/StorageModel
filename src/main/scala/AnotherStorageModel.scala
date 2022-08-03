import java.awt.geom._
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color}
import scala.annotation.tailrec

object AnotherStorageModel extends App {
  case class Point(x: Int, y: Int, z: Int)

  case class Storage(row: Int, cells: Int, height: Int)

  case class AllPoints(startPoint: Point, finishPoint: Point, pose: List[Point])

  val storage = Storage(12, 9, 2)

  val pass = List(1, 5, storage.cells)

  val rows = scala.collection.mutable.Map[Point, String]()
  for (i <- 1 to storage.row) {
    for (j <- 1 to storage.cells) {
      for (k <- 1 to storage.height) {
        rows.put(Point(i, j, k), "Product")
      }
    }
  }

  println(rows)

  def findShortPath(fX: Int, fY: Int, fZ: Int, sX: Int, sY: Int, sZ: Int, pass: List[Int]) = {

    def findPath(allPoints: AllPoints, way: Int) = {

      def func(aP: AllPoints, character: String, pass: Int, zP: String): AllPoints = character match {
        case "x" => if (aP.startPoint.x < pass)
          AllPoints(Point(aP.startPoint.x + 1, aP.startPoint.y, aP.startPoint.z),
            aP.finishPoint,
            aP.pose ++ List(Point(aP.startPoint.x + 1, aP.startPoint.y, aP.startPoint.z)))
        else
          AllPoints(Point(aP.startPoint.x - 1, aP.startPoint.y, aP.startPoint.z),
            aP.finishPoint,
            aP.pose ++ List(Point(aP.startPoint.x - 1, aP.startPoint.y, aP.startPoint.z)))

        case "y" => if (aP.startPoint.y < pass)
          AllPoints(Point(aP.startPoint.x, aP.startPoint.y + 1, aP.startPoint.z),
            aP.finishPoint,
            aP.pose ++ List(Point(aP.startPoint.x, aP.startPoint.y + 1, aP.startPoint.z)))
        else
          AllPoints(Point(aP.startPoint.x, aP.startPoint.y - 1, aP.startPoint.z),
            aP.finishPoint,
            aP.pose ++ List(Point(aP.startPoint.x, aP.startPoint.y - 1, aP.startPoint.z)))

        case "z" => if (zP == "up")
          AllPoints(Point(aP.startPoint.x, aP.startPoint.y, aP.startPoint.z + 1),
            aP.finishPoint,
            aP.pose ++ List(Point(aP.startPoint.x, aP.startPoint.y, aP.startPoint.z + 1)))
        else
          AllPoints(Point(aP.startPoint.x, aP.startPoint.y, aP.startPoint.z - 1),
            aP.finishPoint,
            aP.pose ++ List(Point(aP.startPoint.x, aP.startPoint.y, aP.startPoint.z - 1)))
      }

      // Спуск товара
      def downZ(AllP: AllPoints): AllPoints = {
        @tailrec
        def loop(aP: AllPoints): AllPoints = {
          if (aP.startPoint.z == 1) aP
          else loop(func(aP, "z", way, "down"))
        }

        loop(AllP)
      }

      def findPass(allP: AllPoints): AllPoints = {
        if ((allP.startPoint.x == allP.finishPoint.x) || ((allP.startPoint.x % 2 == 1) && (allP.startPoint.x + 1 == allP.finishPoint.x))) {
          def findYIfSameX(allP: AllPoints): AllPoints = {
            @tailrec
            def loop(aP: AllPoints): AllPoints = {
              if (aP.startPoint.y == aP.finishPoint.y) aP
              else loop(func(aP, "y", aP.finishPoint.y, "none"))
            }

            loop(allP)
          }

          findYIfSameX(allP)
        }

        else {
          def findYPass(allP: AllPoints): AllPoints = {
            @tailrec
            def loop(aP: AllPoints): AllPoints = {
              if (aP.startPoint.y == way) aP
              else loop(func(aP, "y", way, "none"))
            }

            loop(allP)
          }

          findYPass(allP)
        }
      }

      def findX(allP: AllPoints): AllPoints = {
        @tailrec
        def loop(aP: AllPoints): AllPoints = {
          if (aP.startPoint.x == aP.finishPoint.x) aP
          else loop(func(aP, "x", aP.finishPoint.x, "none"))
        }

        loop(allP)
      }

      def findY(allP: AllPoints): AllPoints = {
        @tailrec
        def loop(aP: AllPoints): AllPoints = {
          if (aP.startPoint.y == aP.finishPoint.y) aP
          else loop(func(aP, "y", aP.finishPoint.y, "none"))
        }

        loop(allP)
      }

      def upZ(allP: AllPoints): AllPoints = {
        @tailrec
        def loop(aP: AllPoints): AllPoints = {
          if (aP.startPoint.z == aP.finishPoint.z) aP
          else loop(func(aP, "z", aP.finishPoint.y, "up"))
        }

        loop(allP)
      }

      val result = upZ(findY(findX(findPass(downZ(allPoints)))))
      result
    }

    val allPoints = AllPoints(Point(fX, fY, fZ), Point(sX, sY, sZ), List(Point(fX, fY, fZ)))

    val all: IndexedSeq[AllPoints] = for (i <- pass.indices) yield findPath(allPoints, pass(i))

    val allPath: IndexedSeq[List[Point]] = for (i <- all.indices) yield all(i).pose

    val finalPath: List[Point] = {
      val elemLength = for (i <- allPath.indices) yield allPath(i).length
      println(elemLength)
      val path = allPath.find(x => x.length == elemLength.min)
      path match {
        case Some(k) => k
        case None => List(Point(0, 0, 0))
      }
    }
    println(finalPath)

    val size = (200, 200)

    val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)

    val g = canvas.createGraphics()

    // По умолчанию начало координат левый нижний угол
    g.transform(new AffineTransform(1, 0, 0, -1, 0, size._2))

    // Фон
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)

    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
      java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

    // Отрисовка рядов
    g.setStroke(new BasicStroke())
    g.setColor(new Color(0, 0, 0)) // Цвет

    for (j <- 1 to storage.row) {
      g.draw(new Line2D.Double(j * 10, 10, j * 10, storage.cells * 10 + 10))
    }

    for (j <- 1 to storage.cells + 1) {
      g.draw(new Line2D.Double(10, j * 10, 10 * storage.row, j * 10))
    }

    // Отрисовка шагов
    for (i <- 1 until finalPath.length) {
      if (i == 1) {
        g.setColor(Color.BLUE)
        g.fill(new Ellipse2D.Double(finalPath(i).x * 10, finalPath(i).y * 10, 10, 10))
      }

      else if (i == finalPath.length - 1) {
        g.setColor(Color.GREEN)
        g.fill(new Ellipse2D.Double(finalPath(i).x * 10, finalPath(i).y * 10, 10, 10))
      }

      else {
        g.setColor(Color.RED)
        g.fill(new Ellipse2D.Double(finalPath(i).x * 10, finalPath(i).y * 10, 10, 10))
      }
    }

    // Конец
    g.dispose()

    // запись
    javax.imageio.ImageIO.write(canvas, "png", new java.io.File("drawing.png"))
  }

  findShortPath(5, 6, 2, 11, 9, 2, pass)
}

