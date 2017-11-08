package EnvironmentToyModel

/**
  * Created by Dinesh on 7/11/2017.
  */
object WeatherConditions {

  def Weather(temp: Double, humidity: Int) = {
    if (humidity >= 100) {
      if (temp > 0) "Rain" else "Snow"
    } else "Sunny"
  }


}
