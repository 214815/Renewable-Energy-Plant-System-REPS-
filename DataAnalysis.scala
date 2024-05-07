import scala.util.{Failure, Success, Try}

object DataAnalysis {
  
  // 计算一系列数字的平均值
  def calculateMean(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data to compute.")
      return
    }

    val allValues = data.flatMap(_.drop(1)) // 排除表头行
    val validNumbers = allValues.flatMap(value => scala.util.Try(value.toDouble).toOption)

    if (validNumbers.nonEmpty) {
      val average = validNumbers.sum / validNumbers.size
      println(s"Average value: $average")
    } else {
      println("No valid numeric data found.")
    }
  }

  // 计算一系列数字的中位数
  def calculateMedian(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data to compute.")
      return
    }

    val allValues = data.flatMap(_.drop(1))
    val validNumbers = allValues.flatMap(value => scala.util.Try(value.toDouble).toOption).sorted

    if (validNumbers.nonEmpty) {
      val midIndex = validNumbers.size / 2
      val median = if (validNumbers.size % 2 == 0)
        (validNumbers(midIndex - 1) + validNumbers(midIndex)) / 2.0
      else
        validNumbers(midIndex)

      println(s"Median value: $median")
    } else {
      println("No valid numeric data found.")
    }
  }

  // 计算一系列数字的众数
  def calculateMode(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data to compute.")
      return
    }

    val allValues = data.flatMap(_.drop(1))
    val countMap = allValues.groupBy(identity).mapValues(_.size)

    if (countMap.nonEmpty) {
      val highestFrequency = countMap.values.max
      val modes = countMap.filter(_._2 == highestFrequency).keys.toList

      println("Mode(s) found:")
      modes.foreach(mode => println(s"Mode: $mode"))
    } else {
      println("No valid data found.")
    }
  }

  // 提取并转换数值数据
  private def extractNumericValues(data: List[List[String]]): List[Double] = {
    data.flatMap(_.drop(1)).flatMap(s => Try(s.toDouble).toOption)
  }

  // 计算数值范围
  def calculateRange(data: List[List[String]]): Unit = {
    val numericValues = extractNumericValues(data)
    if (numericValues.isEmpty) {
      println("No valid numeric data available.")
    } else {
      val (minValue, maxValue) = (numericValues.min, numericValues.max)
      println(s"Calculated Range: ${maxValue - minValue}")
    }
  }

  // 计算中距（中点范围）
  def calculateMidrange(data: List[List[String]]): Unit = {
    val numericValues = extractNumericValues(data)
    if (numericValues.isEmpty) {
      println("No valid numeric data available.")
    } else {
      val (minValue, maxValue) = (numericValues.min, numericValues.max)
      println(s"Calculated Midrange: ${(minValue + maxValue) / 2}")
    }
  }

  // 计算最小值
  def calculateMinimum(data: List[List[String]]): Unit = {
    val numericValues = extractNumericValues(data)
    if (numericValues.isEmpty) {
      println("No valid numeric data available.")
    } else {
      println(s"Calculated Minimum Value: ${numericValues.min}")
    }
  }
}
