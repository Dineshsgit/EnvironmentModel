package EnvironmentToyModel

import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}

/**
  * Created by Dinesh on 6/11/2017.
  */


/** Assumption : Temperature in day either increases or
  *              decreases by 10 %  as the days passes by
  *              i.e as the day progresses from day to night temperature
  *              decreases by 10% for each over. Whilst from day to night
  *              temperature increases by 10% for every hour
  *
  */

case class SpecialDay(SpringEquinox: LocalDateTime,
                      SummerSolistice: LocalDateTime,
                      AutumnEquinox: LocalDateTime,
                      WinterSolistice: LocalDateTime,
                      NextSpring:LocalDateTime)


object temparatureConditions {

  val Dateformat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

  def determineSeason(latitude: Double, datetime: LocalDateTime, HighestAvgTemp: Double, LowestAvgTemp:Double)={

    val dtAll = getDTOfSomeDay(datetime)
    var TempOfDay:Double = 0.0
    var TempInDay:Double = 0.0

    /** Identifying the season above Tropic of Cancer */

    if(latitude >= 23.45)
    {
      val AvgHighTempDay =  dtAll.SummerSolistice.plusDays(math.abs((dtAll.SummerSolistice.getDayOfYear - dtAll.AutumnEquinox.getDayOfYear )/ 2))
      val AvgLowTempDay =  dtAll.WinterSolistice.plusDays(math.abs((dtAll.NextSpring.getDayOfYear + 365 ) - dtAll.WinterSolistice.getDayOfYear)/2).minusDays(365)

      if ( datetime.isAfter(dtAll.SpringEquinox) && datetime.isBefore(dtAll.SummerSolistice))
        println("Spring above Tropic Of Cancer")
      else if( datetime.isAfter(dtAll.SummerSolistice) && datetime.isBefore(dtAll.AutumnEquinox))
        println("Summer above Tropic Of Cancer :"+ AvgHighTempDay)
      else if( datetime.isAfter(dtAll.AutumnEquinox) && datetime.isBefore(dtAll.WinterSolistice))
        println("Autumn above Tropic Of Cancer")
      else
        println("Winter above Tropic Of Cancer:" + AvgLowTempDay)

      val Amplitude  = (HighestAvgTemp - LowestAvgTemp) / 2
      val MeanValue = (HighestAvgTemp + LowestAvgTemp) / 2
      val DayPosition = { if (datetime.getDayOfYear > AvgHighTempDay.getDayOfYear)
        datetime.getDayOfYear - AvgHighTempDay.getDayOfYear
      else
        365 - AvgHighTempDay.getDayOfYear + datetime.getDayOfYear
      }
      TempOfDay = Amplitude * math.cos(((2 * math.Pi)/365)* DayPosition) + MeanValue
      TempInDay = TempOfDay * 0.1 * math.cos(getTotalSecondsOfDay(datetime) * 2 * math.Pi / (24 * 3600)) * (-1)
      println("Temperature : "+ TempOfDay)
      println("Temperature in Day :"+ (TempInDay + TempOfDay))

    }

    /** Identifying the season in between Cancer and Equator */

    if(latitude < 23.45 && latitude >= 0)
    {
      val AvgHighTempDay =  dtAll.SpringEquinox.plusDays(math.abs((dtAll.SummerSolistice.getDayOfYear - dtAll.SpringEquinox.getDayOfYear )/ 2))
      val AvgLowTempDay =  dtAll.WinterSolistice.plusDays(math.abs((dtAll.NextSpring.getDayOfYear + 365 ) - dtAll.WinterSolistice.getDayOfYear)/2 ).minusDays(365)

      if ( datetime.isAfter(dtAll.SpringEquinox) && datetime.isBefore(dtAll.SummerSolistice))
        println("Summer Between Cancer and Equator")
      else if((latitude < 23.45 && latitude >= 0)  && ( datetime.isAfter(dtAll.SummerSolistice) && datetime.isBefore(dtAll.AutumnEquinox)))
        println("Rainy Between Cancer and Equator")
      else if( datetime.isAfter(dtAll.AutumnEquinox) && datetime.isBefore(dtAll.WinterSolistice))
        println("Autumn Between Cancer and Equator")
      else
        println("Winter Between Cancer and Equator:" + AvgLowTempDay)

      val Amplitude  = (HighestAvgTemp - LowestAvgTemp) / 2
      val MeanValue = (HighestAvgTemp + LowestAvgTemp) / 2
      val DayPosition = { if (datetime.getDayOfYear > AvgHighTempDay.getDayOfYear)
        datetime.getDayOfYear - AvgHighTempDay.getDayOfYear
      else
        365 - AvgHighTempDay.getDayOfYear + datetime.getDayOfYear
      }
      TempOfDay = Amplitude * math.cos(((2 * math.Pi)/365)* DayPosition) + MeanValue
      TempInDay = TempOfDay * 0.1 * math.cos(getTotalSecondsOfDay(datetime) * 2 * math.Pi / (24 * 3600)) * (-1)

      println("Temperature : "+ TempOfDay)
      println("Temperature in Day :"+ (TempInDay + TempOfDay))

    }

    /** Identifying the season in between Capricorn and Equator */

    if(latitude < 0 && latitude > -23.45)
    {

      val AvgLowTempDay =  dtAll.SummerSolistice.plusDays(math.abs((dtAll.AutumnEquinox.getDayOfYear ) - dtAll.SummerSolistice.getDayOfYear)/2)
      val AvgHighTempDay =  dtAll.WinterSolistice.plusDays(math.abs((dtAll.NextSpring.getDayOfYear + 365 ) - dtAll.WinterSolistice.getDayOfYear)/2).minusDays(365)

      if ( datetime.isAfter(dtAll.SpringEquinox) && datetime.isBefore(dtAll.SummerSolistice))
        println("Autumn Between Equator and Capricorn")
      else if ( datetime.isAfter(dtAll.SummerSolistice) && datetime.isBefore(dtAll.AutumnEquinox))
        println("Winter Between Equator and Capricorn :"+ AvgLowTempDay)
      else if ( datetime.isAfter(dtAll.AutumnEquinox) && datetime.isBefore(dtAll.WinterSolistice))
        println("Spring Between Equator and Capricorn")
      else
        println("Summer Between Equator and Capricorn :" + AvgHighTempDay)

      val Amplitude  = (HighestAvgTemp - LowestAvgTemp) / 2
      val MeanValue = (HighestAvgTemp + LowestAvgTemp) / 2
      val DayPosition = { if (datetime.getDayOfYear > AvgHighTempDay.getDayOfYear)
        datetime.getDayOfYear - AvgHighTempDay.getDayOfYear
      else
        365 - AvgHighTempDay.getDayOfYear + datetime.getDayOfYear
      }
      TempOfDay = Amplitude * math.cos(((2 * math.Pi)/365)* DayPosition) + MeanValue
      TempInDay = TempOfDay * 0.1 * math.cos(getTotalSecondsOfDay(datetime) * 2 * math.Pi / (24 * 3600)) * (-1)

      println("Temperature : "+ TempOfDay)
      println("Temperature in Day :"+ (TempInDay + TempOfDay))

    }

    /** Identifying the season in between Capricorn and Equator */

    if(latitude <= -23.45)
    {
      val AvgLowTempDay =  dtAll.AutumnEquinox.plusDays(math.abs((dtAll.SummerSolistice.getDayOfYear - dtAll.AutumnEquinox.getDayOfYear )/ 2))
      val AvgHighTempDay =  dtAll.WinterSolistice.plusDays(math.abs((dtAll.NextSpring.getDayOfYear + 365 ) - dtAll.WinterSolistice.getDayOfYear)/2).minusDays(365)

      if ( datetime.isAfter(dtAll.SpringEquinox) && datetime.isBefore(dtAll.SummerSolistice))
        println("Autumn below Tropic Of Capricorn")
      else if  ( datetime.isAfter(dtAll.SummerSolistice) && datetime.isBefore(dtAll.AutumnEquinox))
        println("Winter below Tropic Of Capricorn :"+ AvgLowTempDay)
      else if ( datetime.isAfter(dtAll.AutumnEquinox) && datetime.isBefore(dtAll.WinterSolistice))
        println("Spring below Tropic Of Capricorn")
      else
        println("Summer below Tropic Of Capricorn:" + AvgHighTempDay)

      val Amplitude  = (HighestAvgTemp - LowestAvgTemp) / 2
      val MeanValue = (HighestAvgTemp + LowestAvgTemp) / 2
      val DayPosition = { if (datetime.getDayOfYear > AvgHighTempDay.getDayOfYear)
        datetime.getDayOfYear - AvgHighTempDay.getDayOfYear
      else
        365 - AvgHighTempDay.getDayOfYear + datetime.getDayOfYear
      }
      TempOfDay = Amplitude * math.cos(((2 * math.Pi)/365)* DayPosition) + MeanValue
      TempInDay = TempOfDay * 0.1 * math.sin((getTotalSecondsOfDay(datetime) ) * 2 * math.Pi / (24 * 3600)) * (-1)

      println("Temperature : "+ TempOfDay)

      println("Temperature in Day :"+ (TempInDay + TempOfDay))

    }
    "%.2f".format(TempInDay + TempOfDay)
  }

  /** To identify the given date falls in which season */

  def   getDTOfSomeDay(dt: LocalDateTime) = {

    try {
      val strYear = (dt.getYear).toString()
      val nxtyear = (strYear.toInt + 1).toString

      val spring = LocalDateTime.parse(strYear + "-03-21 00:00:00", Dateformat);
      val summer = LocalDateTime.parse(strYear + "-06-21 00:00:00", Dateformat);
      val autumn = LocalDateTime.parse(strYear + "-09-22 00:00:00", Dateformat);
      val winter = LocalDateTime.parse(strYear + "-12-22 00:00:00", Dateformat);
      val nextspring = LocalDateTime.parse(nxtyear + "-03-21 00:00:00", Dateformat);

      SpecialDay(spring, summer, autumn, winter, nextspring)
    } catch {
      case ex: DateTimeParseException => {
        println(ex)
        println("Invalid date/time format")
        null
      }
      case ex: Exception =>
        println(ex)
        null
    }
  }

  def getTotalSecondsOfDay(datetime: LocalDateTime): Int = {
    datetime.getHour * 3600 +
      datetime.getMinute * 60 +
      datetime.getSecond
  }


}
