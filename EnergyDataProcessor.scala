import com.github.tototoshi.csv._
import java.io.File
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}

object EnergyDataProcessor {

  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  
  def promptUserInput(prompt: String): Try[Int] = Try {
    print(prompt)
    scala.io.StdIn.readInt()
  }
  
  def parseEnergyData(records: Seq[Array[String]]): Seq[EnergyProduction] = records.map { fields =>
    EnergyProduction(
      LocalDateTime.parse(fields(0), formatter),
      LocalDateTime.parse(fields(1), formatter),
      fields(2).toDouble
    )
  }
  
  private def filterRecordsByPeriod(records: Seq[EnergyProduction], start: LocalDateTime, end: LocalDateTime): Seq[EnergyProduction] = 
    records.filter(record => record.start.isAfter(start) && record.end.isBefore(end))

  private def resolveTimeFilter(choice: Int, referenceTime: LocalDateTime): Try[(LocalDateTime, LocalDateTime)] = choice match {
    case 1 => Success((referenceTime.minusHours(1), referenceTime))
    case 2 => Success((referenceTime.minusDays(1), referenceTime))
    case 3 => Success((referenceTime.minusDays(7), referenceTime))
    case 4 => Success((referenceTime.minusDays(30), referenceTime))
    case 5 =>
      Try {
        val date = LocalDate.parse(scala.io.StdIn.readLine("Enter the date (yyyy-MM-dd): "), DateTimeFormatter.ISO_LOCAL_DATE)
        (date.atStartOfDay(), date.plusDays(1).atStartOfDay().minusSeconds(1))
      }
    case _ => Failure(new IllegalArgumentException("Invalid time filter option."))
  }


  private def selectDataToDisplay(dataChoice: Int, dataSets: Seq[Seq[EnergyProduction]]): Try[Seq[EnergyProduction]] = dataChoice match {
    case 1 => Success(dataSets(0))
    case 2 => Success(dataSets(1))
    case 3 => Success(dataSets(2))
    case _ => Failure(new IllegalArgumentException("Invalid data set choice."))
  }
  
  def displayData(dataSets: Seq[Seq[EnergyProduction]]): Unit = {
    val currentTime = LocalDateTime.now().minusHours(24)
    
    val plantChoice = promptUserInput("Select Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")
    val timeFilterChoice = promptUserInput("Filter By:\n1. Last hour\n2. Last day\n3. Last week\n4. Last month\n5. Search for a date\nPlease enter your choice: ")

    val period = timeFilterChoice.flatMap(choice => resolveTimeFilter(choice, currentTime))
    val filteredData = for {
      period <- period
      choice <- plantChoice
      data <- selectDataToDisplay(choice, dataSets)
    } yield data.filter(d => d.start.isAfter(period._1) && d.end.isBefore(period._2))

    filteredData match {
      case Success(data) =>
        val sortChoice = promptUserInput("Sort By:\n1. Time\n2. Production Value\nPlease enter your choice: ")
        val orderChoice = promptUserInput("Ascending or Descending:\n1. Ascending\n2. Descending\nPlease enter your choice: ")

        val sortedData = sortChoice.flatMap {
          case 1 => Success(data.sortBy(_.start))
          case 2 => Success(data.sortBy(_.production))
          case _ => Failure(new IllegalArgumentException("Invalid sorting option."))
        }

        sortedData.flatMap { sorted =>
          orderChoice.map {
            case 1 => sorted
            case 2 => sorted.reverse
            case _ => throw new IllegalArgumentException("Invalid ordering option.")
          }
        } match {
          case Success(finalData) =>
            finalData.foreach { d =>
              val formattedTime = d.start.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
              println(s"Time: $formattedTime | Value: ${d.production}")
            }
          case Failure(e) =>
            println(e.getMessage)
        }
      case Failure(e) =>
        println(e.getMessage)
    }
  }
  private def readDate(): LocalDate = {
    println("Enter date (yyyy-MM-dd):")
    LocalDate.parse(scala.io.StdIn.readLine(), DateTimeFormatter.ofPattern("yyyy-MM-dd"))
  }

  def convertToStringList(data: Seq[EnergyProduction]): List[List[String]] = {
  val header = List("Start Time", "End Time", "Production") // CSV 风格的头部
  val rows = data.map { record =>
    List(
      record.start.toString,
      record.end.toString,
      record.production.toString
    )
  }
  header :: rows.toList
}

   def analyzeEnergyData(energySources: (Seq[EnergyProduction], Seq[EnergyProduction], Seq[EnergyProduction])): Unit = {
    val (hydroData, solarData, windData) = energySources
    val currentTime = LocalDateTime.now().minusHours(24)
    val choiceResult = promptUserInput("Select Energy Source:\n1. Hydro\n2. Solar\n3. Wind\nPlease make your selection: ")
    val timeFilterResult = promptUserInput("Time Filter:\n1. Last hour\n2. Last day\n3. Last week\n4. Last month\n5. Specific date\nMake your choice: ")

    val (startTime, endTime) = timeFilterResult match {
      case Success(1) => (currentTime.minusHours(1), currentTime)
      case Success(2) => (currentTime.minusDays(1), currentTime)
      case Success(3) => (currentTime.minusWeeks(1), currentTime)
      case Success(4) => (currentTime.minusMonths(1), currentTime)
      case Success(5) =>
        val specificDate = readDate()
        (specificDate.atStartOfDay(), specificDate.plusDays(1).atStartOfDay().minusSeconds(1))
      case _ =>
        println("Invalid time filter selection.")
        return
    }

    val selectedData = choiceResult match {
      case Success(1) => hydroData
      case Success(2) => solarData
      case Success(3) => windData
      case _ =>
        println("Invalid energy source selection.")
        return
    }

    val filteredData = filterRecordsByPeriod(selectedData, startTime, endTime)
    val stringListData = convertToStringList(filteredData)
    displayAnalysis(stringListData)
  }

  private def displayAnalysis(data: List[List[String]]): Unit = {
    if (data.isEmpty) println("No data to display.")
    else {
      println("Analysis Report\n")
      // Assuming these methods are defined elsewhere and handle their respective calculations
      DataAnalysis.calculateMean(data)
      DataAnalysis.calculateMedian(data)
      DataAnalysis.calculateMode(data)
      DataAnalysis.calculateRange(data)
      DataAnalysis.calculateMidrange(data)
      DataAnalysis.calculateMinimum(data)
    }
  }

  def manageEnergyPlant(plants: List[EnergyPlant]): Unit = {
    val actionResult = promptUserInput("Plant Management:\n1. Shut down a plant\n2. Restart a plant\nPlease make your selection: ")
    actionResult match {
      case Success(1) => togglePlantState(plants, shutdown = true)
      case Success(2) => togglePlantState(plants, shutdown = false)
      case Failure(_) => println("Invalid selection. Please try again.")
    }
  }

  private def togglePlantState(plants: List[EnergyPlant], shutdown: Boolean): Unit = {
    val plantIndex = promptUserInput("Choose a plant:\n1. Hydro\n2. Solar\n3. Wind\nSelect plant: ") match {
      case Success(index) if index >= 1 && index <= 3 =>
        val plant = plants(index - 1)
        plant.isShutdown = shutdown
        val state = if (shutdown) "shut down" else "restarted"
        println(s"${plant.name} has been $state.")
      case _ =>
        println("Invalid plant selection.")
    }
  }
  
  def auditPlantData(dataHydro: Seq[EnergyProduction], dataSolar: Seq[EnergyProduction], dataWind: Seq[EnergyProduction], plants: List[EnergyPlant]): Unit = {
    val choice = promptUserInput("Select plant for data check:\n1. Hydro\n2. Solar\n3. Wind\nEnter choice: ")
    choice match {
      case Success(1) => checkPlant("Hydro", dataHydro, plants)
      case Success(2) => checkPlant("Solar", dataSolar, plants)
      case Success(3) => checkPlant("Wind", dataWind, plants)
      case _ =>
        println("Invalid choice. Please try again.")
    }
  }

  private def checkPlant(plantName: String, data: Seq[EnergyProduction], plants: List[EnergyPlant]): Unit = {
    val plant = plants.find(_.name == plantName).getOrElse(return println(s"$plantName plant not found."))
    val last24Hours = LocalDateTime.now().minusHours(24)
    val yesterday = last24Hours.minusHours(24)

    if (!plant.isShutdown) {
      if (isEquipmentFunctional(plantName.toLowerCase + ".csv")) {
        println(s"All equipment in $plantName power plant is functioning correctly.")
      } else {
        println(s"Warning: $plantName power plant is malfunctioning.")
      }
    } else {
      println(s"Warning: $plantName power plant is shut down.")
    }

    val relevantData = filterRecordsByPeriod(data, yesterday, last24Hours)
    analyzeData(relevantData, threshold = if (plantName == "Solar") 250 else 1500)
  }

  private def isEquipmentFunctional(fileName: String): Boolean = {
  val file = new File(fileName)
  if (file.exists()) {
    val reader = CSVReader.open(file)
    try {
      reader.all().nonEmpty
    } finally {
      reader.close()
    }
  } else {
    false
  }
}

private def analyzeData(data: Seq[EnergyProduction], threshold: Double): Unit = {
  val (belowThreshold, totalCount) = data.foldLeft((0, data.length)) { case ((count, total), record) =>
    if (record.production < threshold) (count + 1, total) else (count, total)
  }
  println(s"Out of $totalCount values, $belowThreshold were below the acceptable threshold of $threshold.")
}



}

