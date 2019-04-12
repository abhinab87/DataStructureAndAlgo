package com.abhinab

/**
  * Created by abhin on 4/12/2019.
  */
object newCSV {

  /*def empIdPattern(str:String):String={
    val regex = "\\D*\\d{6}\\D*"
    str.split("\\(").map(x => if(Pattern.compile(regex).matcher(x).find()) Some(x.substring(0,x.length-2)) else None).flatten.mkString
  }

  def suprvIdIdPattern(str:String):String={
    str.split("\\(").map(x => if(Pattern.compile(supvrId_regex).matcher(x).find()) Some(x.substring(0,x.length-2)) else None).flatten.mkString
  }
  def getCleanSupAndHRIds(mapLengthContraints: Map[String,Int]): Seq[Column] = {
    val cleanedSelect = mapLengthContraints
      .map {
        case (c, _) if c == "Supervisory_Organization" => when(getBadSupvrIdPred(), concat(lit($"error in column ${c}|${supvr_id_pattern_error}|"), $"$c")).otherwise($"Supervisory_Organization").alias("Supervisory_Organization")
        case (c, _) if c == "HR_Patner" => when(getBadHrIdPred(), concat(lit($"error in column ${c}|${hr_id_pattern_error}|"), $"$c")).otherwise($"HR_Patner").alias("HR_Patner")
        case (c, _) => $"$c"
      }.toSeq
    cleanedSelect
  }
  def getBadHrIdPred(): Column = {
    regexp_extract($"Supervisory_Organization", HRID_regex, 1) === ""
  }
  def getNullSelectExpr(mapNullConstraints: Map[String,String]): Seq[Column] = {
    val nullSelectExpr = mapNullConstraints
      .map { case (c, b) => when($"${c}".isNull && b.equals("Y"),
        lit(s"error in column ${c}|${null_error}|")).otherwise($"$c").alias(c)
      }.toSeq
    nullSelectExpr
  }
  val mapSupNullConstraints = Map("Supervisory_Organization" -> "Y", "HR_Partner" -> "Y")
  val mapSupLengthContstraints = Map("Supervisory_Organization" -> 6, "HR_Partner" -> 6)
  Accounting Policy & Controls (Aaron Hutto (On Leave) (382478))
  Sammy Duff (111833)
  HR_patner:
    Sammy Duff (111833): mahesh Duff (126723)
*/
}
