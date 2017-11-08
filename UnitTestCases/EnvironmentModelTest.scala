package EnvironmentToyModel

import java.time.LocalDateTime
import java.time.format.DateTimeParseException


/**
  * Created by xdvi2 on 8/11/2017.
  */
object EnvironmentModelTest extends App{
 
 val format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  println("\nPlease enter an latitude of Locations : ")
  val loclatitude = scala.io.StdIn.readLine()

  println("\nPlease enter the date and time in DD/MM/YYYY HH:MM:SS format : ")
  val loctime = scala.io.StdIn.readLine

  println("\nPlease enter the Altitude of the location : ")
  val localtitude = scala.io.StdIn.readLine

  println("\nPlease enter the Annual Average High Temperature at the Location : ")
  val AverageHighTemp = scala.io.StdIn.readLine()

  println("\nPlease enter the Annual Average Low Temperature at the Location : ")
  val AverageLowTemp = scala.io.StdIn.readLine()


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

  val actualdate = convertDateTime(loctime)

  /** Calling Temperature Module - Computing Temperature of location at given time*/
  val temp = temparatureConditions.determineSeason(loclatitude, actualdate , AverageHighTemp, AverageLowTemp)

  /** Calling Pressure Module - Computing Pressure at the location */
  val pressure = pressureConditions.calculatePressure(localtitude)

  /** Calling Humidity Module - Computing Humidity at the location */
  val humidity = humidityConditions.Humidity(temp.toDouble , actualdate)

  /** Calling Weather Conditions Module - To identify the weather in the location */
  val season =  WeatherConditions.Weather(temp.toDouble, humidity)

  println("Temperature :" + temp)
  println("Pressure :" + pressure)
  println("humidity :" + humidity)
  println("Weather Condition : " + season)

}
