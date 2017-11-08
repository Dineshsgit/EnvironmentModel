package EnvironmentToyModel

/**
  * Created by Dinesh on 7/11/2017.
  */

object pressureConditions {

  val Atmospheric_pressure:Double = 101325
  val Standard_Temperature:Double = 288.15
  val gravity:Double = 9.80665
  val temperature_lapse_rate:Double = 0.0065
  val universal_gas_constant:Double = 8.31447
  val molar_mass:Double = 0.0289644

  def calculatePressure(height:Double)= {

    val hPa =  (Atmospheric_pressure * math.pow((1- ((temperature_lapse_rate * height)/Standard_Temperature)),
      ((gravity * molar_mass)/(universal_gas_constant * temperature_lapse_rate) )))/100

    "%.1f".format(hPa)
  }

}