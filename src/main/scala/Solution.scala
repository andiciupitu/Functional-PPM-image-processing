import util.Pixel

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]
  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    // împărțim șirul de caractere în linii separate
    val lines = image.mkString.split("\n").toList
    // Extragem lățimea și înălțimea imaginii
    val width = lines(1).split(" ")(0).toInt
    val height = lines(1).split(" ")(1).toInt
    val maxColor = lines(2).toInt
    // Extragem pixelii și îi transformăm în obiecte Pixel
    val pixelData = lines.drop(3).map(_.split(" ").map(_.toInt))
    val pixels = pixelData.map(p => Pixel((p(0) * 255) / maxColor, (p(1) * 255) / maxColor, (p(2) * 255) / maxColor))
    // Grupăm pixelii în rânduri pentru a forma o listă de liste de pixeli
    pixels.grouped(width).toList
  }
  def toStringPPM(image: Image): List[Char] = {
    val width = image.head.length
    val height = image.length
    val maxColor = 255
    // creează antetul formatului PPM
    val header = s"P3\n$width $height\n$maxColor\n"
    // transformă matricea bidimensională de pixeli într-o listă liniară de șiruri de caractere pentru fiecare pixel
    val imageData = image.flatMap(row => row.map(p => s"${p.red} ${p.green} ${p.blue}\n"))
    // adaugă antetul la începutul listei de șiruri și adaugă o listă separată pentru fiecare linie a imaginii
    (header :: imageData).toList.flatMap(_.toList)
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    // combină rândurile din prima și a doua imagine într-o singură listă
    val combinedRows = image1 ++ image2
    // întoarce lista combinată de rânduri
    combinedRows
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    // Concatenăm rândurile corespunzătoare din cele două imagini
    val newRows = image1.zip(image2).map { case (row1, row2) => row1 ++ row2 }
    newRows
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    val rotated =
      degrees match {
        case 90 => image.transpose.reverse
        case 180 => image.reverse.map(_.reverse)
        case 270 => image.transpose.map(_.reverse)
        case _ => image
      }
    rotated
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = ???

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = ???

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    def pascalTriangle(n: Int): List[List[Int]] = {
         if (n == 0) List.empty
         else if (n == 1) List(List(1))
         else {
           val previousTriangle = pascalTriangle(n - 1)
           val previousRow = previousTriangle.last
           val newRow = (previousRow :+ 0).zip(0 +: previousRow).map { case (a, b) => (a + b) % m }
           previousTriangle :+ newRow
         }
    }
    def completeLists(lst: List[List[Int]], n: Int): List[List[Int]] = {
      def completeList(list: List[Int], size: Int): List[Int] = {
        if (list.size == size) list
        else completeList(5 :: list, size)
      }
      lst.map(list => completeList(list, n)).map(_.reverse)
    }
    val matrix = pascalTriangle(size)
    val newMatrix = completeLists(matrix,size).map(_.map(elem => funct(elem.toInt)))
    newMatrix
  }
}
