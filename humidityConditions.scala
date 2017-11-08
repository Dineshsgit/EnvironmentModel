package EnvironmentToyModel

import java.time.LocalDateTime


/**
  * Created by Dinesh on 7/11/2017.
  */

/** https://en.wikipedia.org/wiki/Arden_Buck_equation
  *
  * Adren Buck has derived an equation to calculate the saturation vapour pressure at a particular Temperature
  */
object humidityConditions {

  def Humidity(temp: Double, datetime:LocalDateTime): Int = {

    /** Asumming dew point will not go beyond 45 degrees. Because as the dew point temperature
      * goes beyond 26C, It will be difficult for humans to survive. Since there is no mechanism
      * to derive the exact dew point temperature, I am using random Temperature
      */
    val getHour = datetime.getHour


    val rnd = new scala.util.Random()
    val dewPoint =  45 * rnd.nextDouble()

    val actualvaporpressure = 6.11 * math.pow(10, ((7.5 * dewPoint)/(237.3 + dewPoint)))

    val saturatedvaporpressure =  6.11 * math.pow(10, ((7.5 * temp)/(237.3 + temp)))


    val hum =  (actualvaporpressure / saturatedvaporpressure) * 100

    if (hum > 100) 100 else hum.toInt
  }


}