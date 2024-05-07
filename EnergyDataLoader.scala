import com.github.tototoshi.csv._
import java.io.File

object EnergyDataLoader {
    
  // 封装读取CSV文件的逻辑
  private def loadCSVData(filePath: String): List[List[String]] = {
    val reader = CSVReader.open(new File(filePath))
    try {
      reader.all()
    } finally {
      reader.close()  // 确保文件读取器被正确关闭
    }
  }
  
  // 优化数据加载消息
  def loadData(): (List[List[String]], List[List[String]], List[List[String]]) = {
    val hydroData = loadCSVData("data/hydro.csv")
    val solarData = loadCSVData("data/solar.csv")
    val windData = loadCSVData("data/wind.csv")
    
    println("All energy data has been successfully loaded.")
    (hydroData, solarData, windData)
  }
}
