import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class EnergyProduction(start: LocalDateTime, end: LocalDateTime, production: Double)
case class EnergyPlant(name: String, var isShutdown: Boolean = false)

object EnergyManagementSystem extends App {
  private val (dataHydro, dataSolar, dataWind) = EnergyDataLoader.loadData()

  // Define a sequence of energy plants
  private val energyPlants = List(
    EnergyPlant("Hydro"),
    EnergyPlant("Solar"),
    EnergyPlant("Wind")
  )

  // Drop header and convert data
  val (hydroEnergy, solarEnergy, windEnergy) = (
    EnergyDataProcessor.parseEnergyData(dataHydro.drop(1).map(_.toArray)),
    EnergyDataProcessor.parseEnergyData(dataSolar.drop(1).map(_.toArray)),
    EnergyDataProcessor.parseEnergyData(dataWind.drop(1).map(_.toArray))
  )

  @tailrec
  private def displayMenu(): Unit = {
    println("\nSelect an Option:")
    println("1. Show Energy Data")
    println("2. Analyze Energy Data")
    println("3. Monitor Errors Last 24 Hours")
    println("4. Manage Energy Plant")
    println("5. Exit System")

    val userInput = EnergyDataProcessor.promptUserInput("Enter your choice: ")

    userInput match {
      case Success(option) =>
        handleOption(option)
        if (option != 5) displayMenu()
      case Failure(_) =>
        println("Invalid input. Please enter a number.")
        displayMenu()
    }
  }

  def handleOption(option: Int): Unit = {
    option match {
      case 1 => EnergyDataProcessor.displayData(Seq(hydroEnergy, solarEnergy, windEnergy))
      case 2 => EnergyDataProcessor.analyzeEnergyData(hydroEnergy, solarEnergy, windEnergy)
      case 3 => EnergyDataProcessor.auditPlantData(hydroEnergy, solarEnergy, windEnergy, energyPlants)
      case 4 => EnergyDataProcessor.manageEnergyPlant(energyPlants)
      case 5 => println("Exiting...")
      case _ => println("Please select a valid option.")
    }
  }

  displayMenu()
}
