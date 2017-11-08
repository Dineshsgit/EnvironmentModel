package EnvironmentToyModel

/**
  * Created by Dinesh on 6/11/2017.
  */

import java.io._
import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import scala.io.Source

case class WeatherStationInfo(CityName: String, AvgHighTemp: Double, AverageLowTemp: Double,
                              Latitude: Double, Longitude: Double, Altitude: Double, IATA: String )



object EnvironmentalModel extends App {

  var output = ""

  /** This is the main function which will trigger the Temperature, Humidity, Pressure and Weather
    * calculations
    * @param inputpath - Input file location
    * @param datetime  - Date for which weather needs to be calculated
    * @param outputpath - Output file location
    */

  def calculateVariables(inputpath: String, datetime: String, outputpath:String) {

    val input = Source.fromFile(inputpath)
    val lines = input.getLines()
    var data = Array[WeatherStationInfo]()
    val format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

    /** Convert the date in string format into DateTime format
      *
      * @param datetime - Input Date
      * @
      */

    def convertDateTime(datetime: String) = {
      try {
        LocalDateTime.parse(datetime, format)
      } catch {
        case ex: DateTimeParseException => {
          println(ex)
          println("Input Date Error")
          null
        }
        case ex: Exception =>
          println(ex)
          null
      }
    }

    /** Processing each line in the input file
      *
      */

    for (l <- lines) {
      val inputsplit = l.split('|')

      try {
        if (inputsplit != null && inputsplit.length > 1) {

          val data1 =  WeatherStationInfo(inputsplit(0).toString, inputsplit(1).toDouble, inputsplit(2).toDouble,
            inputsplit(3).toDouble, inputsplit(4).toDouble, inputsplit(5).toDouble, inputsplit(6).toString)

          val actualdate = convertDateTime(datetime)

          /** Calling Temperature Module - Computing Temperature of location at given time*/
          val temp = TemparatureConditions.determineSeason(data1.Latitude, actualdate , data1.AvgHighTemp, data1.AverageLowTemp)

          /** Calling Pressure Module - Computing Pressure at the location */
          val pressure = pressureConditions.calculatePressure(data1.Altitude)

          /** Calling Humidity Module - Computing Humidity at the location */
          val humidity = humidityConditions.Humidity(temp.toDouble , actualdate)

          /** Calling Weather Conditions Module - To identify the weather in the location */
          val season =  WeatherConditions.Weather(temp.toDouble, humidity)


          output =  output + data1.IATA + "|" + data1.Latitude + "|" + data1.Longitude + "|" + data1.Altitude + "|" + actualdate + "|" +
            season + "|"+  temp + "|" +  pressure + "|" + humidity + '\n'


        }
      } catch {
        case ex: NumberFormatException => println(l + "\n" + ex)
        case ex: Exception => println(ex)
      }

    }

    println(output)
    val print = new PrintWriter(outputpath)
    print.write(output)
    print.close()

  }
  
}
