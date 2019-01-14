## rclimdex.r - RClimDex
## R implementation of climdex using PCICt and R tcl/tk graphical interface
## Also provides quality control of datasets
## Last updated by Rodney Chan, December 2018
## Rewritten by Rodney Chan, July 2014
## Rewritten by Yang Feng, July 2004
## Intital version by Yujun Ouyang, Mar 2004

# Debug use
#source('ecqc.r')
#source('ecui.r')
#source('rclimdexpatch.r')

## Requires climdex.pcic and PCICt
library(climdex.pcic)
library(PCICt)

## Environment to store objects
rclimdex.env <- new.env()

## Perform indices calulation
IndicesCalculation <- function(climdexinput) {
  ## Prepare file path and file name
  file.path <- rclimdex.env$parameter$file.loc
  file.prefix <- rclimdex.env$parameter$station

  ## Perform calulation and outut results
#  WriteTrend(c("indices,start year,end year,slope,std of slope,p value"), paste(file.path, "trend/", file.prefix, "_trend.csv", sep = ""), label = TRUE)
  WriteIndice("tmaxmean", 1, file.path, file.prefix, climdexinput, monthly = TRUE, round = 2)
  WriteIndice("tminmean", 1, file.path, file.prefix, climdexinput, monthly = TRUE, round = 2)
  WriteIndice("summ", rclimdex.env$parameter$summ, file.path, file.prefix, climdexinput, pcicname = "su", custom = rclimdex.env$parameter$tmax.upper)
  WriteIndice("idmm", rclimdex.env$parameter$idmm, file.path, file.prefix, climdexinput, pcicname = "id", custom = rclimdex.env$parameter$tmax.lower)
  WriteIndice("trmm", rclimdex.env$parameter$trmm, file.path, file.prefix, climdexinput, pcicname = "tr", custom = rclimdex.env$parameter$tmin.upper)
  WriteIndice("fdmm", rclimdex.env$parameter$fdmm, file.path, file.prefix, climdexinput, pcicname = "fd", custom = rclimdex.env$parameter$tmin.lower)
  WriteIndice("su", rclimdex.env$parameter$su, file.path, file.prefix, climdexinput, pcicname = "su")
  WriteIndice("id", rclimdex.env$parameter$id, file.path, file.prefix, climdexinput, pcicname = "id")
  WriteIndice("tr", rclimdex.env$parameter$tr, file.path, file.prefix, climdexinput, pcicname = "tr")
  WriteIndice("fd", rclimdex.env$parameter$fd, file.path, file.prefix, climdexinput, pcicname = "fd")
  WriteIndice("gsl", rclimdex.env$parameter$gsl, file.path, file.prefix, climdexinput)
  WriteIndice("txx", rclimdex.env$parameter$txx, file.path, file.prefix, climdexinput, monthly = TRUE, round = 5)
  WriteIndice("txn", rclimdex.env$parameter$txn, file.path, file.prefix, climdexinput, monthly = TRUE, round = 5)
  WriteIndice("tnx", rclimdex.env$parameter$tnx, file.path, file.prefix, climdexinput, monthly = TRUE, round = 5)
  WriteIndice("tnn", rclimdex.env$parameter$tnn, file.path, file.prefix, climdexinput, monthly = TRUE, round = 5)
  WriteIndice("tx10p", rclimdex.env$parameter$tx10p, file.path, file.prefix, climdexinput, monthly = TRUE, round = 2)
  WriteIndice("tx90p", rclimdex.env$parameter$tx90p, file.path, file.prefix, climdexinput, monthly = TRUE, round = 2)
  WriteIndice("tn10p", rclimdex.env$parameter$tn10p, file.path, file.prefix, climdexinput, monthly = TRUE, round = 2)
  WriteIndice("tn90p", rclimdex.env$parameter$tn90p, file.path, file.prefix, climdexinput, monthly = TRUE, round = 2)
  WriteIndice("wsdi", rclimdex.env$parameter$wsdi, file.path, file.prefix, climdexinput)
  WriteIndice("csdi", rclimdex.env$parameter$csdi, file.path, file.prefix, climdexinput)
  WriteIndice("dtr", rclimdex.env$parameter$dtr, file.path, file.prefix, climdexinput, monthly = TRUE, round = 2)
  WriteIndice("rx1day", rclimdex.env$parameter$rx1day, file.path, file.prefix, climdexinput, monthly = TRUE, round = 4)
  WriteIndice("rx5day", rclimdex.env$parameter$rx5day, file.path, file.prefix, climdexinput, monthly = TRUE, custom = "center.mean.on.last.day = TRUE", round = 4)
  WriteIndice("r10mm", rclimdex.env$parameter$r10mm, file.path, file.prefix, climdexinput)
  WriteIndice("r20mm", rclimdex.env$parameter$r20mm, file.path, file.prefix, climdexinput)
  WriteIndice("rnnmm", rclimdex.env$parameter$rnnmm, file.path, file.prefix, climdexinput, custom = rclimdex.env$parameter$prcp.threshold)
  WriteIndice("cdd", rclimdex.env$parameter$cdd, file.path, file.prefix, climdexinput)
  WriteIndice("cwd", rclimdex.env$parameter$cwd, file.path, file.prefix, climdexinput)
  WriteIndice("sdii", rclimdex.env$parameter$sdii, file.path, file.prefix, climdexinput, round = 1)
  WriteIndice("r95p", rclimdex.env$parameter$r95p, file.path, file.prefix, climdexinput, pcicname = "r95ptot", round = 1)
  WriteIndice("r99p", rclimdex.env$parameter$r99p, file.path, file.prefix, climdexinput, pcicname = "r99ptot", round = 1)
  WriteIndice("prcptot", rclimdex.env$parameter$prcptot, file.path, file.prefix, climdexinput, round = 1)
}

## Operator to select correct climdex.pcic function for each calulation
## Also communicate with ecqc for correct output
WriteIndice <- function(selection, choice, outpath, outname, climdexinput, monthly = FALSE, pcicname = NULL, custom = "", round = NULL) {
  ## Get correct name of corresponding climdex.pcic function
  if(is.null(pcicname)) pcicname <- selection
  if(custom != "") {
    if(grepl("nn", selection)) {
      selection  <- gsub("nn", custom, selection)
    } else if(grepl("mm", selection)) {
      selection <- gsub("mm", custom, selection)
    }
  }
  if(custom != "") custom <- paste(",", custom, sep = "")

  ## If user selected then perform calulation
  if(as.logical(choice)) {
    label <- c(paste("year", selection, sep = ","))
    result <- eval(parse(text = paste("climdex.", pcicname, "(climdexinput", custom, ")", sep = "")))
    years <- names(result)
    ## Also perform monthly results if possible
    if(monthly) {
      result <- matrix(result, ncol = 12, byrow = TRUE)
      annual <- eval(parse(text = paste("climdex.", pcicname, "(climdexinput", custom, ", freq = 'annual')", sep = "")))
      years <- names(annual)
      result <- cbind(result, annual)
      label <- c("year,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,annual")
    }
    if(!is.null(round)) result <- round(result, digits = round)
    result <- cbind(years, result)

    ## Output results in table, plot and trend summary
    WriteTable(result, label, paste(outpath, "indices/", outname, "_", toupper(selection), ".csv", sep = ""))
#    WritePlot(cbind(result[, 1], result[, ncol(result)]), paste(outpath, "plots/", outname, "_", toupper(selection), ".pdf", sep = ""), main = paste(toupper(selection), outname, sep = "   "), xlab = "Year", ylab = toupper(selection))
#    WriteTrend(cbind(result[, 1], result[, ncol(result)]), paste(outpath, "trend/", outname, "_trend.csv", sep = ""), name = selection)
  }
}

## Prepare dataset for input format of climdex.pcic
ClimdexInput <- function() {
  base <- c(rclimdex.env$parameter$bp.first, rclimdex.env$parameter$bp.last)
  names(base) <- c("start", "end")
  base <- BaseShorten(base, rclimdex.env$data)
  nh <- FALSE
  if(rclimdex.env$parameter$lat < 2) nh <- TRUE
  climdexinput <- climdexInput.raw(FilterColumn(rclimdex.env$data, "tmax"), FilterColumn(rclimdex.env$data, "tmin"), FilterColumn(rclimdex.env$data, "prcp"), SetDates(rclimdex.env$data, "tmax"), SetDates(rclimdex.env$data, "tmin"), SetDates(rclimdex.env$data, "prcp"), base.range = c(base["start"], base["end"]), northern.hemisphere = nh)
#  ## Patch NA rules
#  climdexinput@namasks$annual <- lapply(climdexinput@data, get.na.mask, climdexinput@date.factors$annual, 15)
#  names(climdexinput@namasks$annual) <- names(climdexinput@namasks$monthly)
#  assign("climdexinput", climdexinput, envir = rclimdex.env)
#  ## Patch Ends
  return(climdexinput)
}

## Creates PCICt dates with correct calendar type
SetDates <- function(dataset, var) {
  cal <- c("gregorian", "365", "360")
  dates <- c(paste(FilterColumn(dataset, var, "year"), FilterColumn(dataset, var, "month"), FilterColumn(dataset, var, "day"), sep = "-"))
  return(as.PCICt(dates, cal = cal[rclimdex.env$parameter$cal.type]))
}

## Perform quality control
QualityControl <- function() {
  ## Prepare file path, file name and copy of original dataset
  implicit.missing <- -99.9
  file.path <- rclimdex.env$parameter$file.loc
  file.prefix <- rclimdex.env$parameter$station
  original <- rclimdex.env$data

  ## Apply missing mask
  rclimdex.env$data[rclimdex.env$data == rclimdex.env$parameter$missing.mark] <- NA
  rclimdex.env$data <- sapply(rclimdex.env$data, as.character)
  rclimdex.env$data[is.na(rclimdex.env$data)] <- -99.9
  rclimdex.env$data <- data.frame(rclimdex.env$data)

  ## Check for Non-numerics
  ## If Non-numerics in first row, then assume it is header
  ## If still fails then give error
  if(any(is.na(data.frame(sapply(rclimdex.env$data, AllNumeric))))) {
    rclimdex.env$data <- rclimdex.env$data[-1, ]
    if(any(is.na(data.frame(sapply(rclimdex.env$data, AllNumeric))))) {
      error <- paste("-Caution-", "Non-numeric values in data are flagged", sep = "\n")
      class(error) <- "error"
      AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
    }
  }
  ## Invalid dates gives error
  if(any(is.na(SetMissing(rclimdex.env$data[,1:3])))) {
    error <- paste("-Caution-", "All values of incorrect dates are flagged", sep = "\n")
    class(error) <- "error"
    AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  }

  ## Clean data before checks
  rclimdex.env$data[rclimdex.env$data == -99.9] <- NA
  rclimdex.env$data <- data.frame(sapply(rclimdex.env$data, AllNumeric))
  ## Negative values for precipitation to NA
  rclimdex.env$data <- QualityCheck(rclimdex.env$data, "rclimdex.env$data$prcp < 0", "Negative precipitation value (prcp < 0) are flagged", "prcp")
  ## Max temp less than min temp to NA, if available
  if(!(all(is.na(rclimdex.env$data$tmax)) | all(is.na(rclimdex.env$data$tmin)))) {
    rclimdex.env$data <- QualityCheck(rclimdex.env$data, "rclimdex.env$data$tmax < rclimdex.env$data$tmin", "Impossible temperature value (tmax < tmin) are flagged", list("tmax", "tmin"))
  }
  ## Temperature greater than 70C and less than -70C to NA, if available
  if(!(all(is.na(rclimdex.env$data$tmax)))) {
    rclimdex.env$data <- QualityCheck(rclimdex.env$data, "abs(rclimdex.env$data$tmax) > 70", "Maximum tempature value outside of acceptable range (-70C to 70C) are flagged", "tmax")
  }
  if(!(all(is.na(rclimdex.env$data$tmin)))) {
    rclimdex.env$data <- QualityCheck(rclimdex.env$data, "abs(rclimdex.env$data$tmin) > 70", "Minimum tempature value outside of acceptable range (-70C to 70C) are flagged", "tmin")
  }
  ## Duplicate dates to NA
  ## Keep the first occurance only
  rclimdex.env$data <- QualityCheck(rclimdex.env$data, "duplicated(rclimdex.env$data[, c('year', 'month', 'day')])", "The values of repeated dates are flagged", list("year", "month", "day", "tmax", "tmin", "prcp"))

  ## Output quality control results (removals from original)
  MakeDetailStat(rclimdex.env$data, "tmax", original, paste(file.path, "log/", file.prefix, "_tmaxQC.csv", sep = ""))
  MakeDetailStat(rclimdex.env$data, "tmin", original, paste(file.path, "log/", file.prefix, "_tminQC.csv", sep = ""))
  MakeDetailStat(rclimdex.env$data, "prcp", original, paste(file.path, "log/", file.prefix, "_prcpQC.csv", sep = ""))

  ## Sort and fix dates
  rclimdex.env$data <- FixSortDates(rclimdex.env$data)
  ## Fill out dates to end of calendar year
  rclimdex.env$data <- FillDates(rclimdex.env$data)

  ## Output more quality control results
  WriteTable(cbind(rclimdex.env$data$year, rclimdex.env$data$month, rclimdex.env$data$day, rclimdex.env$data$prcp, rclimdex.env$data$tmax, rclimdex.env$data$tmin), c("year,month,day,prcp,tmax,tmin"), paste(file.path, file.prefix, ".indcal.csv", sep = ""))
  MakeOutlierStat(rclimdex.env$data, paste(file.path, "log/", file.prefix, "_tepstdQC.csv", sep = ""), rclimdex.env$parameter$std.tmp)
#  ## Values exceeding upper limit for precipitation to NA
#  rclimdex.env$data <- QualityCheck(rclimdex.env$data, "rclimdex.env$data$prcp > rclimdex.env$parameter$prcp.limit ", "There are precipitation value that exceed upper limit", "prcp")
  WriteTimeSeries(cbind(rclimdex.env$data$year, rclimdex.env$data$month, rclimdex.env$data$day, rclimdex.env$data$prcp), paste(file.path, "log/", file.prefix, "_prcpPLOT.pdf", sep = ""), type = "h", name = file.prefix, var = "prcp", unit = "mm", yzero = TRUE, distribution = TRUE, dtitle = paste("Histogram for Station: ", file.prefix," of prcp >= 1mm", sep = ""))
  WriteTimeSeries(cbind(rclimdex.env$data$year, rclimdex.env$data$month, rclimdex.env$data$day, rclimdex.env$data$tmax), paste(file.path, "log/", file.prefix, "_tmaxPLOT.pdf", sep = ""), name = file.prefix, var = "tmax", unit = bquote(~degree*C))
  WriteTimeSeries(cbind(rclimdex.env$data$year, rclimdex.env$data$month, rclimdex.env$data$day, rclimdex.env$data$tmin), paste(file.path, "log/", file.prefix, "_tminPLOT.pdf", sep = ""), name = file.prefix, var = "tmin", unit = bquote(~degree*C))
  WriteTimeSeries(cbind(rclimdex.env$data$year, rclimdex.env$data$month, rclimdex.env$data$day, rclimdex.env$data$tmax-rclimdex.env$data$tmin), paste(file.path, "log/", file.prefix, "_dtrPLOT.pdf", sep = ""), name = file.prefix, var = "dtr", yzero = TRUE)
  MakeNAStat(rclimdex.env$data, paste(file.path, "log/", file.prefix, "_nastatistic.csv", sep = ""))
}

## Get new base period excluding all missing years at the start and end
BaseShorten <- function(base, dataset) {
  bp <- base
  names(bp) <- names(base)
  valid <- unique(dataset[, "year"])
  allna <- unique(dataset[which(is.na(dataset[,"prcp"]) & is.na(dataset[,"tmax"]) & is.na(dataset[,"tmin"])), "year"])
  if(length(allna)>=1) {
    valid <- valid[!valid%in%allna]
    if(bp["start"] < min(valid)) {
      bp["start"] <- min(valid)
    }
    if(bp["end"] > max(valid)) {
      bp["end"] <- max(valid)
    }
  }
  if(bp["start"] != base["start"] | bp["end"] != base["end"]) {
    AddLog(paste("The base period for this file is redefined as ", bp["start"], " to ", bp["end"], " due to too many missing values",sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  }
  return(bp)
}

## Remove invalid calender for the plots and NA summary
## Sort the remaining dataset
FixSortDates <- function(dataset) {
  result <- data.frame(sapply(dataset, AllNumeric))
  names(result) <- names(dataset)
  if(any(is.na(result$year))) {
    result <- result[-which(is.na(result$year)), ]
  }
  result <- result[order(result$year, result$month, result$day), ]
  return(result)
}

## Fill dates to correspond with full year and remove leap days as well as erroraeous dates
FillDates <- function(dataset) {
  monthdays <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  startyear <- min(dataset$year, na.rm=T)
  endyear <- max(dataset$year, na.rm=T)
  blanks <- data.frame(cbind(sort(rep(seq(startyear,endyear),366)),rep(rep(seq(1,12),monthdays),(endyear-startyear+1)),rep(c(seq(1,monthdays[1]),seq(1,monthdays[2]),seq(1,monthdays[3]),seq(1,monthdays[4]),seq(1,monthdays[5]),seq(1,monthdays[6]),seq(1,monthdays[7]),seq(1,monthdays[8]),seq(1,monthdays[9]),seq(1,monthdays[10]),seq(1,monthdays[11]),seq(1,monthdays[12])),(endyear-startyear+1)),rep(NA,366*(endyear-startyear+1)),rep(NA,366*(endyear-startyear+1)),rep(NA,366*(endyear-startyear+1))))
  names(blanks) <- names(dataset)
  blankskey <- c(blanks$year*100000+blanks$month*100+blanks$day)
  datakey <- c(dataset$year*100000+dataset$month*100+dataset$day)
  result <- blanks
  keys <- match(datakey,blankskey)
  if(any(is.na(keys))) {
    nokeys <- which(is.na(keys))
    keys <- keys[!is.na(keys)]
    result[keys,"prcp"] <- dataset[-nokeys,"prcp"]
    result[keys,"tmax"] <- dataset[-nokeys,"tmax"]
    result[keys,"tmin"] <- dataset[-nokeys,"tmin"]
  } else {
    result[keys,"prcp"] <- dataset$prcp
    result[keys,"tmax"] <- dataset$tmax
    result[keys,"tmin"] <- dataset$tmin
  }
  result <- data.frame(sapply(result, AllNumeric))
  ## Leap years
  result <- result[-which((result$month == 2 & result$year%%4 != 0 & result$day > 28) | (result$month == 2 & result$year%%4 == 0 & result$year%%100 == 0 & result$year%%400 != 0 & result$day > 28)), ]
  return(result)
}

## Set non-numeric, invalid dates and missing mark to NA
SetMissing <- function(dataset) {
  result <- data.frame(sapply(dataset, AllNumeric))
  names(result) <- names(dataset)
  result[which(result$month > 12 | result$month < 1), ] <- NA
  result[which(result$day > 31 | result$day < 1), ] <- NA
  result[which(result$month == 2 & result$day > 29), ] <- NA
  result[which(result$month == 2 & result$year%%4 != 0 & result$day > 28), ] <- NA
  result[which(result$month == 2 & result$year%%4 == 0 & result$year%%100 == 0 & result$year%%400 != 0 & result$day > 28), ] <- NA
  result[which(result$month == 4 & result$day > 30), ] <- NA
  result[which(result$month == 6 & result$day > 30), ] <- NA
  result[which(result$month == 9 & result$day > 30), ] <- NA
  result[which(result$month == 11 & result$day > 30), ] <- NA
  result[which(result$year < 1), ] <- NA
  result[result == rclimdex.env$parameter$missing.mark] <- NA
  result[is.na(result$year), ] <- NA
  result[is.na(result$month), ] <- NA
  result[is.na(result$day), ] <- NA
  flg <- any(is.na(result$year))
  if(!is.na(flg)) {
     flg <- any(is.na(result$month))
     if(!is.na(flg)) {
       flg <- any(is.na(result$day))
       if(!is.na(flg)) {
         return(result)
       }
     }
  }
  return(NA)
}

## Write table for outlier stats
MakeOutlierStat <- function(dataset, outpath, threshold) {
  tmax <- round(FindOutlier(dataset$tmax, dataset$month, dataset$day, threshold), digits = 2)
  tmin <- round(FindOutlier(dataset$tmin, dataset$month, dataset$day, threshold), digits = 2)
  dtr <- round(FindOutlier(dataset$tmax-dataset$tmin, dataset$month, dataset$day, threshold), digits = 2)
  result <- data.frame(cbind(dataset$year, dataset$month, dataset$day, tmax$low, round(dataset$tmax, digits = 2), tmax$up, tmin$low, round(dataset$tmin, digits = 2), tmin$up, dtr$low, round(dataset$tmax-dataset$tmin, digits = 2), dtr$up))
  names(result) <- c("year", "month", "day", "tmaxlow", "tmax", "tmaxup", "tminlow", "tmin", "tminup", "dtrlow", "dtr", "dtrup")
  result <- rbind(result[result$tmax > result$tmaxup, ], result[result$tmax < result$tmaxlow, ], result[result$tmin > result$tminup, ], result[result$tmin < result$tminlow, ], result[result$dtr > result$dtrup, ], result[result$dtr < result$dtrlow, ])
  result <- result[!duplicated(result), ]
  result <- result[order(result$year, result$month, result$day), ]
  label <- c("year,month,day,tmaxlow,tmax,tmaxup,tminlow,tmin,tminup,dtrlow,dtr,dtrup")
  WriteTable(result, label, outpath)
  if(nrow(result) != 0) {
    error <- paste("-Caution-", "There are outliers temperature values, please review", sep = "\n")
    class(error) <- "error"
    AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  }
}

## Write table for detail stats
MakeDetailStat <- function(dataset, name, original, outpath) {
  result <- original[is.na(dataset[name]), ]
  #result <- cbind(result$year, result$month, result$day, result$prcp, result$tmax, result$tmin)
  label <- c("year,month,day,prcp,tmax,tmin")
  WriteTable(result, label, outpath)
}

## Write table for NA stats
MakeNAStat <- function(dataset, outpath) {
  result <- data.frame(cbind(paste(dataset$year, dataset$month, sep = ""), as.numeric(is.na(dataset$tmax)), as.numeric(is.na(dataset$tmin)), as.numeric(is.na(dataset$prcp))))
  names(result) <- c("date", "tmax", "tmin", "prcp")
  tmax <- MonthlyStat(as.numeric(result$tmax)-1, result$date, dataset$year, "sum")
  tmin <- MonthlyStat(as.numeric(result$tmin)-1, result$date, dataset$year, "sum")
  prcp <- MonthlyStat(as.numeric(result$prcp)-1, result$date, dataset$year, "sum")
  stat <- rbind(prcp, tmax, tmin)
  title <- c(rep("prcp", nrow(prcp)), rep("tmax", nrow(tmax)), rep("tmin", nrow(tmin)))
  stat <- cbind(title, stat)
  stat <- stat[order(stat$year), ]
  label <- c("title,year,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,ann")
  WriteTable(stat, label, outpath)
}

## Check if dataset has right number of column
## Assume dataset is year, month, day, prcp, tmax, tmin
DataCheck <- function() {
  if(ncol(rclimdex.env$data) != 6) {
    error <- paste("-Error-", "Dataset formatting is not correct\nIt should be 6 column: 'year month day prcp tmax tmin'", sep = "\n")
    class(error) <- "try-error"
    return(error)
  }
  #dataset <- data.frame(sapply(rclimdex.env$data, AllNumeric))
  #names(rclimdex.env$data) <- AssignName(dataset)
  names(rclimdex.env$data) <- c("year", "month", "day", "prcp", "tmax", "tmin")
  return("")
}

## Assign name automatically
## Work in progress
## Not in use in current version
AssignName <- function(dataset) {
  names(dataset) <- rep("data", 6)
  dataset <- AssignCol(dataset, "month", "sum", rule = "dataset >= 1 && dataset <= 12 && dataset%%1 == 0")
  dataset <- AssignCol(dataset, "day", "sum", rule = "dataset >= 1 && dataset <= 31 && dataset%%1 == 0")
  dataset <- AssignCol(dataset, "year", "sum", rule = "dataset%%1 == 0")
  dataset <- AssignCol(dataset, "prcp", "sum", rule = "dataset == 0")
  dataset <- AssignCol(dataset, "tmax", "max")
  dataset <- AssignCol(dataset, "tmin", "min")
  if(any(names(dataset) == "data")) names(dataset) <- c("year", "month", "day", "prcp", "tmax", "tmin")
  return(names(dataset))
}

## Open batch input file
## Work in progress
## Not in use in current version
OpenBatch <- function() {
  name <- tk_choose.files(caption = "Open", filters = matrix(c("All Readable Files", ".txt .csv", "All Files", "*"), ncol = 2, byrow = TRUE))
  if(length(name) != 1) {
    error <- paste("-Error-", "Please select one file only", sep = "\n")
    class(error) <- "try-error"
    return(error)
  }
  data <- ReadTable(name)
  if(class(data) == "try-error") {
    error <- paste("-Error-", "Unable to open file:", name, sep = "\n")
    class(error) <- "try-error"
    return(error)
  }
  error <- ParameterCheck(data, name)
  if(class(data) == "try-error") return(error)
  return("")
}

## Prepare for batch process
## Work in progress
## Not in use in current version
PrepBatch <- function() {
  error <- OpenBatch()
  if(MakeAlert(error)) return()
  error <- ParseQCParameter()
  if(MakeAlert(error)) return()
  GetQCParameter()
  dataset <- ReadTable(rclimdex.env$parameter$file.in)
  if(class(dataset) == "try-error") {
    error <- paste("-Error-", "Unable to open file:", rclimdex.env$parameter$file.in, sep = "\n")
    MakeAlert(error)
    return()
  }
  assign("data", dataset, envir = rclimdex.env)
  SetOutputFolders(rclimdex.env$parameter$file.loc)
  error <- DataCheck()
  if(MakeAlert(error)) return()
  error <- QualityControl()
  if(MakeAlert(error)) return()
  tkmessageBox(message = paste("Quality control complete", "Click OK to continue", sep = "\n"))
  error <- ParseICParameter(rclimdex.env$data)
  if(MakeAlert(error)) return()
  error <- ParseICSelection()
  if(MakeAlert(error)) return()
  GetICParameter()
  GetICSelection()
  climdexinput <- ClimdexInput()
  IndicesCalculation(climdexinput)
  tkmessageBox(message = paste("Indices Calculation complete", "Click OK to continue", sep = "\n"))
  return()
}

## Check batch input file
## Work in progress
## Not in use in current version
ParameterCheck <- function(dataset, name) {
  if(length(dataset) != length(rclimdex.env$parameter)) {
    error <- paste("-Error-", "Unable to open file:", name, sep = "\n")
    class(error) <- "try-error"
    return(error)
  }
  for(i in 1:length(rclimdex.env$parameter)) {
    assign(paste("entries.", names(rclimdex.env$parameter[i]), sep = ""), tclVar(as.character(unlist(dataset[i]))), envir = rclimdex.env)
  }
  return("")
}

## Open and read dataset with open file interface
## Supports opening multiple files
OpenFile <- function(suffix) {
  fontstyle <- DefaultFont("")
  name <- tk_choose.files(caption = "Open", filters = matrix(c("All Readable Files", suffix, "All Files", "*"), ncol = 2, byrow = TRUE))
  ## Pop-up and go back if no file open
  if(length(name) < 1) {
    error <- paste("-Error-", "No file is selected", sep = "\n")
    class(error) <- "try-error"
    return(error)
  ## Multiple file selected, store list, and open first dataset only
  } else if(length(name) > 1) {
    assign("filelist", name, envir = rclimdex.env)
    name <- name[1]
  ## Only one file selected
  } else {
    assign("filelist", name, envir = rclimdex.env)
  }
  data <- ReadTable(name)
  ## Error when trying to read file
  if(class(data) == "try-error") {
    error <- paste("-Error-", "Unable to open file:", name, sep = "\n")
    class(error) <- "try-error"
    return(error)
  }
  assign("data", data, envir = rclimdex.env)
  rclimdex.env$parameter$file.in <- name
  assign("entries.file.in", tclVar(as.character(name)), envir = rclimdex.env)
  return("")
}

## Create output folders
## The 4 folders are indices, log, plots and trend
SetOutputFolders <- function(path) {
  if(substr(path,nchar(path),nchar(path)) != "/") {
    path <- paste(path, "/", sep = "")
  }
  MakeFolder(path, "indices")
  MakeFolder(path, "log")
#  MakeFolder(path, "plots")
#  MakeFolder(path, "trend")
  if(is.na(rclimdex.env$logfile)) {
    assign("logfile", paste(path, paste("log.", gsub("-", "", gsub(":", "", gsub("[[:space:]]", ".", Sys.time()))), ".txt", sep = ""), sep = ""), envir = rclimdex.env)
  }
}

## Initial values for input parameters
SetParameter <- function() {
  parameter <- list("", "", "", 3, 200, 1, -99.9, 1961, 1990, 0, 1, 25, 0, 20, 0, 25, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  names(parameter) <- list("file.in", "file.loc", "station", "std.tmp", "prcp.limit", "cal.type", "missing.mark", "bp.first", "bp.last", "lon", "lat", "tmax.upper", "tmax.lower", "tmin.upper", "tmin.lower", "prcp.threshold", "summ", "idmm", "trmm", "fdmm", "su", "id", "tr", "fd", "gsl", "txx", "txn", "tnx", "tnn", "tx10p", "tx90p", "tn10p", "tn90p", "wsdi", "csdi", "dtr", "rx1day", "rx5day", "sdii", "r10mm", "r20mm", "rnnmm", "cdd", "cwd", "r95p", "r99p", "prcptot")
  assign("parameter", parameter, envir = rclimdex.env)
}

## Get quality control parameters from user input
GetQCParameter <- function() {
  rclimdex.env$parameter$file.in <- as.character(tclvalue(rclimdex.env$entries.file.in))
  rclimdex.env$parameter$file.loc <- as.character(tclvalue(rclimdex.env$entries.file.loc))
  rclimdex.env$parameter$station <- as.character(tclvalue(rclimdex.env$entries.station))
  rclimdex.env$parameter$std.tmp <- as.numeric(tclvalue(rclimdex.env$entries.std.tmp))
#  rclimdex.env$parameter$prcp.limit <- as.numeric(tclvalue(rclimdex.env$entries.prcp.limit))
  rclimdex.env$parameter$cal.type <- as.numeric(tclvalue(rclimdex.env$entries.cal.type))
  rclimdex.env$parameter$missing.mark <- as.character(tclvalue(rclimdex.env$entries.missing.mark))
}

## Get indices calulation parameters from user input
GetICParameter <- function() {
  rclimdex.env$parameter$file.in <- as.character(tclvalue(rclimdex.env$entries.file.in))
  rclimdex.env$parameter$file.loc <- as.character(tclvalue(rclimdex.env$entries.file.loc))
  rclimdex.env$parameter$missing.mark <- as.character(tclvalue(rclimdex.env$entries.missing.mark))
  rclimdex.env$parameter$station <- as.character(tclvalue(rclimdex.env$entries.station))
  rclimdex.env$parameter$bp.first <- as.numeric(tclvalue(rclimdex.env$entries.bp.first))
  rclimdex.env$parameter$bp.last <- as.numeric(tclvalue(rclimdex.env$entries.bp.last))
#  rclimdex.env$parameter$lon <- as.numeric(tclvalue(rclimdex.env$entries.lon))
  rclimdex.env$parameter$lat <- as.numeric(tclvalue(rclimdex.env$entries.lat))
  rclimdex.env$parameter$tmax.upper <- as.numeric(tclvalue(rclimdex.env$entries.tmax.upper))
  rclimdex.env$parameter$tmax.lower <- as.numeric(tclvalue(rclimdex.env$entries.tmax.lower))
  rclimdex.env$parameter$tmin.upper <- as.numeric(tclvalue(rclimdex.env$entries.tmin.upper))
  rclimdex.env$parameter$tmin.lower <- as.numeric(tclvalue(rclimdex.env$entries.tmin.lower))
  rclimdex.env$parameter$prcp.threshold <- as.numeric(tclvalue(rclimdex.env$entries.prcp.threshold))
}

## Get indices calulation selections from user input
GetICSelection <- function() {
  rclimdex.env$parameter$txx <- as.numeric(tclvalue(rclimdex.env$entries.txx))
  rclimdex.env$parameter$txn <- as.numeric(tclvalue(rclimdex.env$entries.txn))
  rclimdex.env$parameter$tnx <- as.numeric(tclvalue(rclimdex.env$entries.tnx))
  rclimdex.env$parameter$tnn <- as.numeric(tclvalue(rclimdex.env$entries.tnn))
  rclimdex.env$parameter$tx10p <- as.numeric(tclvalue(rclimdex.env$entries.tx10p))
  rclimdex.env$parameter$tx90p <- as.numeric(tclvalue(rclimdex.env$entries.tx90p))
  rclimdex.env$parameter$tn10p <- as.numeric(tclvalue(rclimdex.env$entries.tn10p))
  rclimdex.env$parameter$tn90p <- as.numeric(tclvalue(rclimdex.env$entries.tn90p))
  rclimdex.env$parameter$rx1day <- as.numeric(tclvalue(rclimdex.env$entries.rx1day))
  rclimdex.env$parameter$rx5day <- as.numeric(tclvalue(rclimdex.env$entries.rx5day))
  rclimdex.env$parameter$dtr <- as.numeric(tclvalue(rclimdex.env$entries.dtr))
  rclimdex.env$parameter$prcptot <- as.numeric(tclvalue(rclimdex.env$entries.prcptot))
  rclimdex.env$parameter$r10mm <- as.numeric(tclvalue(rclimdex.env$entries.r10mm))
  rclimdex.env$parameter$r20mm <- as.numeric(tclvalue(rclimdex.env$entries.r20mm))
  rclimdex.env$parameter$wsdi <- as.numeric(tclvalue(rclimdex.env$entries.wsdi))
  rclimdex.env$parameter$csdi <- as.numeric(tclvalue(rclimdex.env$entries.csdi))
  rclimdex.env$parameter$su <- as.numeric(tclvalue(rclimdex.env$entries.su))
  rclimdex.env$parameter$fd <- as.numeric(tclvalue(rclimdex.env$entries.fd))
  rclimdex.env$parameter$tr <- as.numeric(tclvalue(rclimdex.env$entries.tr))
  rclimdex.env$parameter$id <- as.numeric(tclvalue(rclimdex.env$entries.id))
  rclimdex.env$parameter$cdd <- as.numeric(tclvalue(rclimdex.env$entries.cdd))
  rclimdex.env$parameter$cwd <- as.numeric(tclvalue(rclimdex.env$entries.cwd))
  rclimdex.env$parameter$gsl <- as.numeric(tclvalue(rclimdex.env$entries.gsl))
  rclimdex.env$parameter$sdii <- as.numeric(tclvalue(rclimdex.env$entries.sdii))
  rclimdex.env$parameter$r95p <- as.numeric(tclvalue(rclimdex.env$entries.r95p))
  rclimdex.env$parameter$r99p <- as.numeric(tclvalue(rclimdex.env$entries.r99p))
  rclimdex.env$parameter$summ <- as.numeric(tclvalue(rclimdex.env$entries.summ))
  rclimdex.env$parameter$fdmm <- as.numeric(tclvalue(rclimdex.env$entries.fdmm))
  rclimdex.env$parameter$trmm <- as.numeric(tclvalue(rclimdex.env$entries.trmm))
  rclimdex.env$parameter$idmm <- as.numeric(tclvalue(rclimdex.env$entries.idmm))
  rclimdex.env$parameter$rnnmm <- as.numeric(tclvalue(rclimdex.env$entries.rnnmm))
}

## Skip some indices calulations if dataset does not allow it
FilterICSelection <- function(dataset) {
  bp <- dataset[which(dataset$year >= rclimdex.env$parameter$bp.first & dataset$year <= rclimdex.env$parameter$bp.last), ]
  threshold <- (rclimdex.env$parameter$bp.last - rclimdex.env$parameter$bp.first + 1) * 365 * (1 - 0.25)
  if(sum(!is.na(dataset$prcp)) == 0) {
    rclimdex.env$parameter$rx1day <- 0
    rclimdex.env$parameter$rx5day <- 0
    rclimdex.env$parameter$prcptot <- 0
    rclimdex.env$parameter$r10mm <- 0
    rclimdex.env$parameter$r20mm <- 0
    rclimdex.env$parameter$cdd <- 0
    rclimdex.env$parameter$cwd <- 0
    rclimdex.env$parameter$sdii <- 0
    rclimdex.env$parameter$r95p <- 0
    rclimdex.env$parameter$r99p <- 0
    rclimdex.env$parameter$rnnmm <- 0
    AddLog("All precipitation values are NA, skips indices calulations relating to precipitation", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
#    return(1)
  }
  if(sum(!is.na(dataset$tmax)) == 0) {
    rclimdex.env$parameter$txx <- 0
    rclimdex.env$parameter$txn <- 0
    rclimdex.env$parameter$tx10p <- 0
    rclimdex.env$parameter$tx90p <- 0
    rclimdex.env$parameter$dtr <- 0
    rclimdex.env$parameter$wsdi <- 0
    rclimdex.env$parameter$csdi <- 0
    rclimdex.env$parameter$su <- 0
    rclimdex.env$parameter$id <- 0
    rclimdex.env$parameter$gsl <- 0
    rclimdex.env$parameter$summ <- 0
    rclimdex.env$parameter$idmm <- 0
    AddLog("All maximum temperature values are NA, skips indices calulations relating to maximum temperature", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
#    return(1)
  } 
  if(sum(!is.na(dataset$tmin)) == 0) {
    rclimdex.env$parameter$tnx <- 0
    rclimdex.env$parameter$tnn <- 0
    rclimdex.env$parameter$tn10p <- 0
    rclimdex.env$parameter$tn90p <- 0
    rclimdex.env$parameter$dtr <- 0
    rclimdex.env$parameter$wsdi <- 0
    rclimdex.env$parameter$csdi <- 0
    rclimdex.env$parameter$fd <- 0
    rclimdex.env$parameter$tr <- 0
    rclimdex.env$parameter$gsl <- 0
    rclimdex.env$parameter$fdmm <- 0
    rclimdex.env$parameter$trmm <- 0
    AddLog("All minimum temperature values are NA, skips indices calulations relating to minimum temperature", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
#    return(1)
  }
  if(sum(!is.na(bp$tmin)) <= threshold | sum(!is.na(bp$tmax)) <= threshold) {
    rclimdex.env$parameter$tx10p <- 0
    rclimdex.env$parameter$tx90p <- 0
    rclimdex.env$parameter$tn10p <- 0
    rclimdex.env$parameter$tn90p <- 0
    rclimdex.env$parameter$wsdi <- 0
    rclimdex.env$parameter$csdi <- 0
    AddLog("NA in base period is more than 25%, skips indices calulations for exceedance rate, cold spell duration and warm spell duration", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
#    return(1)
  }
#  return(0)
}

## Save indices calulation parameters from user input
SaveICSelection <- function() {
  result <- c(rclimdex.env$parameter$txx,rclimdex.env$parameter$txn,rclimdex.env$parameter$tnx,rclimdex.env$parameter$tnn,rclimdex.env$parameter$tx10p,rclimdex.env$parameter$tx90p,rclimdex.env$parameter$tn10p,rclimdex.env$parameter$tn90p,rclimdex.env$parameter$rx1day,rclimdex.env$parameter$rx5day,rclimdex.env$parameter$dtr,rclimdex.env$parameter$prcptot,rclimdex.env$parameter$r10mm,rclimdex.env$parameter$r20mm,rclimdex.env$parameter$wsdi,rclimdex.env$parameter$csdi,rclimdex.env$parameter$su,rclimdex.env$parameter$fd,rclimdex.env$parameter$tr,rclimdex.env$parameter$id,rclimdex.env$parameter$cdd,rclimdex.env$parameter$cwd,rclimdex.env$parameter$gsl,rclimdex.env$parameter$sdii,rclimdex.env$parameter$r95p,rclimdex.env$parameter$r99p,rclimdex.env$parameter$summ,rclimdex.env$parameter$fdmm,rclimdex.env$parameter$trmm,rclimdex.env$parameter$idmm,rclimdex.env$parameter$rnnmm)
  return(result)
}

## Reload indices calulation parameters from user input
LoadICSelection <- function(para) {
  rclimdex.env$parameter$txx <- para[1]
  rclimdex.env$parameter$txn <- para[2]
  rclimdex.env$parameter$tnx <- para[3]
  rclimdex.env$parameter$tnn <- para[4]
  rclimdex.env$parameter$tx10p <- para[5]
  rclimdex.env$parameter$tx90p <- para[6]
  rclimdex.env$parameter$tn10p <- para[7]
  rclimdex.env$parameter$tn90p <- para[8]
  rclimdex.env$parameter$rx1day <- para[9]
  rclimdex.env$parameter$rx5day <- para[10]
  rclimdex.env$parameter$dtr <- para[11]
  rclimdex.env$parameter$prcptot <- para[12]
  rclimdex.env$parameter$r10mm <- para[13]
  rclimdex.env$parameter$r20mm <- para[14]
  rclimdex.env$parameter$wsdi <- para[15]
  rclimdex.env$parameter$csdi <- para[16]
  rclimdex.env$parameter$su <- para[17]
  rclimdex.env$parameter$fd <- para[18]
  rclimdex.env$parameter$tr <- para[19]
  rclimdex.env$parameter$id <- para[20]
  rclimdex.env$parameter$cdd <- para[21]
  rclimdex.env$parameter$cwd <- para[22]
  rclimdex.env$parameter$gsl <- para[23]
  rclimdex.env$parameter$sdii <- para[24]
  rclimdex.env$parameter$r95p <- para[25]
  rclimdex.env$parameter$r99p <- para[26]
  rclimdex.env$parameter$summ <- para[27]
  rclimdex.env$parameter$fdmm <- para[28]
  rclimdex.env$parameter$trmm <- para[29]
  rclimdex.env$parameter$idmm <- para[30]
  rclimdex.env$parameter$rnnmm <- para[31]
}

## Parse user inputted quality control parameters
ParseQCParameter <- function() {
  ## No special characters
  error <- ParseError("[[:alnum:]_.:/\b\\-]*", as.character(tclvalue(rclimdex.env$entries.file.in)), "Input data path must not contain any of these character\n! \" # $ % & \' ( ) * + , ; < = > ? @ [ ] ^ ` { | } ~")
  ## Check if file path is valid
  if(!file.exists(as.character(tclvalue(rclimdex.env$entries.file.in)))) {
    error <- paste("-Error-", "Input data does not exist", "Please check data location", sep = "\n")
    class(error) <- "try-error"
  }
  if(class(error) == "try-error") return(error)

  ## Special character then file path valid
  error <- ParseError("[[:alnum:]_.:/\b\\-]*", as.character(tclvalue(rclimdex.env$entries.file.loc)), "Output directory must not contain any of these character\n! \" # $ % & \' ( ) * + , ; < = > ? @ [ ] ^ ` { | } ~")
  if(class(error) == "try-error") return(error)
  filepath <- as.character(tclvalue(rclimdex.env$entries.file.loc))
  filepath <- substr(filepath, 1, nchar(filepath) - 1)
  if(!file.exists(filepath)) {
    error <- paste("-Error-", "Output directory does not exist", "Please create output folder first", sep = "\n")
    class(error) <- "try-error"
  }
  if(substr(as.character(tclvalue(rclimdex.env$entries.file.loc)), nchar(as.character(tclvalue(rclimdex.env$entries.file.loc))), nchar(as.character(tclvalue(rclimdex.env$entries.file.loc)))) != "/") rclimdex.env$entries.file.loc <- tclVar(paste(as.character(tclvalue(rclimdex.env$entries.file.loc)), "/", sep = ""))
  if(class(error) == "try-error") return(error)

  ## Alphanumericals, dot and hyphen only
  error <- ParseError("[[:alnum:]_.\\-]*", as.character(tclvalue(rclimdex.env$entries.station)), "Station name must not contain any of these character\n! \" # $ % & \' ( ) * + , / : ; < = > ? @ [ \\ ] ^ ` { | } ~")
  if(class(error) == "try-error") return(error)

  ## Real numbers
  error <- ParseError("[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.std.tmp)), "Standard deviation for temperature must be a positive number")
  if(class(error) == "try-error") return(error)
#  error <- ParseError("[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.prcp.limit)), "Upper limit for precipitation must be a positive number")
#  if(class(error) == "try-error") return(error)

  ## 1, 2 or 3
  error <- ParseError("[1-3]{1}", as.character(tclvalue(rclimdex.env$entries.cal.type)), "Calender type must be 1, 2 or 3\nFor Gregorian, 365-day or 360-day respectively")
  if(class(error) == "try-error") return(error)

  ## No special character, no spaces
  error <- ParseError("[[:graph:]]*", as.character(tclvalue(rclimdex.env$entries.missing.mark)), "No spaces for missing marker")
  if(class(error) == "try-error") return(error)
}

## Parse user inputted indices calulation parameters
ParseICParameter <- function(dataset) {
  firstyear <- min(dataset["year"], na.rm = TRUE)
  lastyear <- max(dataset["year"], na.rm = TRUE)

  ## Special character then file path valid
  error <- ParseError("[[:alnum:]_.:/\b\\-]*", as.character(tclvalue(rclimdex.env$entries.file.in)), "Input data path must not contain any of these character\n! \" # $ % & \' ( ) * + , ; < = > ? @ [ ] ^ ` { | } ~")
  if(!file.exists(as.character(tclvalue(rclimdex.env$entries.file.in)))) {
    error <- paste("-Error-", "Input data does not exist", "Please check data location", sep = "\n")
    class(error) <- "try-error"
  }
  if(class(error) == "try-error") return(error)
  error <- ParseError("[[:alnum:]_.:/\b\\-]*", as.character(tclvalue(rclimdex.env$entries.file.loc)), "Output directory must not contain any of these character\n! \" # $ % & \' ( ) * + , ; < = > ? @ [ ] ^ ` { | } ~")
  if(class(error) == "try-error") return(error)
  filepath <- as.character(tclvalue(rclimdex.env$entries.file.loc))
  filepath <- substr(filepath, 1, nchar(filepath) - 1)
  if(!file.exists(filepath)) {
    error <- paste("-Error-", "Output directory does not exist", "Please create output folder first", sep = "\n")
    class(error) <- "try-error"
  }
  if(substr(as.character(tclvalue(rclimdex.env$entries.file.loc)), nchar(as.character(tclvalue(rclimdex.env$entries.file.loc))), nchar(as.character(tclvalue(rclimdex.env$entries.file.loc)))) != "/") rclimdex.env$entries.file.loc <- tclVar(paste(as.character(tclvalue(rclimdex.env$entries.file.loc)), "/", sep = ""))
  if(class(error) == "try-error") return(error)

  ## Alphanumericals, dot and hyphen only
  error <- ParseError("[[:alnum:]_.\\-]*", as.character(tclvalue(rclimdex.env$entries.station)), "Station name must not contain any of these character\n! \" # $ % & \' ( ) * + , / : ; < = > ? @ [ \\ ] ^ ` { | } ~")
  if(class(error) == "try-error") return(error)

  ## No special character, no spaces
  error <- ParseError("[[:graph:]]*", as.character(tclvalue(rclimdex.env$entries.missing.mark)), "No spaces for missing marker")
  if(class(error) == "try-error") return(error)

  ## Integers
  error <- ParseError("[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.bp.first)), "First year of base period must be a positive number")
  if(class(error) == "try-error") return(error)
  error <- ParseRange(as.numeric(tclvalue(rclimdex.env$entries.bp.first)), firstyear, lastyear, paste("Base Period must be between ", firstyear, " and ", lastyear, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.bp.last)), "Last year of base period must be a positive number")
  if(class(error) == "try-error") return(error)
  error <- ParseReverse(as.numeric(tclvalue(rclimdex.env$entries.bp.last)), as.numeric(tclvalue(rclimdex.env$entries.bp.first)), "Last year must be after first year of base period")
  if(class(error) == "try-error") return(error)
  error <- ParseRange(as.numeric(tclvalue(rclimdex.env$entries.bp.last)), firstyear, lastyear, paste("Base Period must be between ", firstyear, " and ", lastyear, sep = ""))
  if(class(error) == "try-error") return(error)

  ## Real numbers and threshold checks
#  error <- ParseError("-?[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.lon)), "Longitude must be a number")
#  if(class(error) == "try-error") return(error)
#  error <- ParseRange(as.numeric(tclvalue(rclimdex.env$entries.lon)), 0, 360, "Longitude must be between 0 and 360")
#  if(class(error) == "try-error") return(error)
#  error <- ParseError("-?[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.lat)), "Latitude must be a number")
#  if(class(error) == "try-error") return(error)
#  error <- ParseRange(as.numeric(tclvalue(rclimdex.env$entries.lat)), -90, 90, "Latitude must be between -90 and 90")
#  if(class(error) == "try-error") return(error)
  error <- ParseError("[1-2]{1}", as.character(tclvalue(rclimdex.env$entries.lat)), "Latitude must be 1 (NH) or 2 (SH)")
  if(class(error) == "try-error") return(error)
  error <- ParseError("-?[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.tmax.upper)), "Upper threshold of daily maximum temperature must be a number")
  if(class(error) == "try-error") return(error)
  error <- ParseError("-?[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.tmax.lower)), "Lower threshold of daily maximum temperature must be a number")
  if(class(error) == "try-error") return(error)
  error <- ParseReverse(as.numeric(tclvalue(rclimdex.env$entries.tmax.upper)), as.numeric(tclvalue(rclimdex.env$entries.tmax.lower)), "Upper threshold limit must be larger than lower threshold limit for maximum temperature")
  if(class(error) == "try-error") return(error)
  error <- ParseError("-?[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.tmin.upper)), "Upper threshold of daily minimum temperature must be a number")
  if(class(error) == "try-error") return(error)
  error <- ParseError("-?[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.tmin.lower)), "Lower threshold of daily mainmum temperature must be a number")
  if(class(error) == "try-error") return(error)
  error <- ParseReverse(as.numeric(tclvalue(rclimdex.env$entries.tmin.upper)), as.numeric(tclvalue(rclimdex.env$entries.tmin.lower)), "Upper threshold limit must be larger than lower threshold limit for minimum temperature")
  if(class(error) == "try-error") return(error)
  error <- ParseReverse(as.numeric(tclvalue(rclimdex.env$entries.tmax.upper)), as.numeric(tclvalue(rclimdex.env$entries.tmin.upper)), "Maximum temperature threshold must be higher than minimum temperature threshold")
if(class(error) == "try-error") return(error)
  error <- ParseReverse(as.numeric(tclvalue(rclimdex.env$entries.tmax.lower)), as.numeric(tclvalue(rclimdex.env$entries.tmin.lower)), "Maximum temperature threshold must be higher than minimum temperature threshold")
  if(class(error) == "try-error") return(error)
  error <- ParseError("[[:digit:]]*\\.?[[:digit:]]*", as.character(tclvalue(rclimdex.env$entries.prcp.threshold)), "Threshold of precipitation must be a positive number")
  if(class(error) == "try-error") return(error)
}

## Parse user inputted indices calulation selections
ParseICSelection <- function() {

  ## 1 or 0 whereas 0 is no and 1 is yes
  text <- " must be 0 (no) or 1 (yes)"
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.txx)), paste("txx", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.txn)), paste("txn", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.tnx)), paste("tnx", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.tnn)), paste("tnn", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.tx10p)), paste("tx10p", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.tx90p)), paste("tx90p", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.tn10p)), paste("tn10p", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.tn90p)), paste("tn90p", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.rx1day)), paste("rx1day", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.rx5day)), paste("rx5day", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.dtr)), paste("dtr", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.prcptot)), paste("prcptot", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.r10mm)), paste("r10mm", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.r20mm)), paste("r20mm", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.wsdi)), paste("wsdi", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.csdi)), paste("csdi", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.su)), paste("su", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.fd)), paste("fd", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.tr)), paste("tr", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.id)), paste("id", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.cdd)), paste("cdd", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.cwd)), paste("cwd", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.gsl)), paste("gsl", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.sdii)), paste("sdii", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.r95p)), paste("r95p", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.r99p)), paste("r99p", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.summ)), paste("summ", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.fdmm)), paste("fdmm", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.trmm)), paste("trmm", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.idmm)), paste("idmm", text, sep = ""))
  if(class(error) == "try-error") return(error)
  error <- ParseError("[0-1]{1}", as.character(tclvalue(rclimdex.env$entries.rnnmm)), paste("rnnmm", text, sep = ""))
  if(class(error) == "try-error") return(error)
}

## Create user interface for title screen
StartTitle <- function() {
  font <- GetFont()
  main <- tktoplevel()
  assign("main", main, envir = rclimdex.env)
  tkwm.title(main, "RClimDex")
  MakeLabel(main, "Start RClimDex", fontstyle = font$heading, padx = 170, pady = 75)
  MakeButton(main, "Run Quality Control", PrepQC, width = 30)
  MakeButton(main, "Run Indices Calulation", PrepIC, width = 30)
  MakeButton(main, "Exit", function()Quit("main", rclimdex.env), width = 30)
  MakeLabel(main, "", fontstyle = font$heading)
  assign("logfile", NA, envir = rclimdex.env)
}

## Create user interface for quality control
StartQC <- function() {
  font <- GetFont()
  main <- tktoplevel()
  assign("main", main, envir = rclimdex.env)

  ## Title
  tkwm.title(main, "RClimDex")
  MakeLabel(main, "Set Parameters for Quality Control", fontstyle = font$heading1, sticky = "w")
  MakeLabel(main, "", fontstyle = font$normal)

  ## User inputs
  top <- tkframe(main)
  tkgrid(top)
  MakeLabel(top, "Output files location : ", fontstyle = font$normal, sticky = "e")
  MakeEntry(top, rclimdex.env$parameter$file.loc, "entries.file.loc", row = 0, column = 1, sticky = "w", env = rclimdex.env)
  if(length(rclimdex.env$filelist) == 1) {
    MakeLabel(top, "Station name or code : ", fontstyle = font$normal, sticky = "e")
    MakeEntry(top, rclimdex.env$parameter$station, "entries.station", row = 1, column = 1, sticky = "w", env = rclimdex.env)
  }
  MakeLabel(top, "Number of standard deviation for temperature : ", fontstyle = font$normal, row = 2, column = 0, sticky = "e")
  MakeEntry(top, rclimdex.env$parameter$std.tmp, "entries.std.tmp", row = 2, column = 1, sticky = "w", env = rclimdex.env)
#  MakeLabel(top, "Upper limit for precipitation (mm) : ", fontstyle = font$normal, sticky = "e")
#  MakeEntry(top, rclimdex.env$parameter$prcp.limit, "entries.prcp.limit", row = 3, column = 1, sticky = "w", env = rclimdex.env)
#  MakeLabel(top, "Calender type : ", fontstyle = font$normal, sticky = "e")
#  MakeRadio(top, rclimdex.env$parameter$cal.type, list("Gregorian", "365-day", "360-day"), "entries.cal.type", fontstyle = font$normal, row = 4, column = 1, sticky = "w", env = rclimdex.env)
  assign("entries.cal.type", tclVar(as.character(rclimdex.env$parameter$cal.type)), envir = rclimdex.env)
  MakeLabel(top, "Missing marker : ", fontstyle = font$normal, sticky = "e")
  MakeEntry(top, rclimdex.env$parameter$missing.mark, "entries.missing.mark", row = 3, column = 1, sticky = "w", env = rclimdex.env)
  MakeLabel(main, "", fontstyle = font$normal)

  ## Action buttons, OK or Quit
  bottom <- tkframe(main)
  tkgrid(bottom)
  MakeButton(bottom, "Quit", PrepMain, sticky = "w")
  MakeButton(bottom, "Perform Quality Control", PerformQC, width = 30, row = 0, column = 1, sticky = "e")
  MakeLabel(main, "", fontstyle = font$normal)

  ## Log window
  log <- tkframe(main)
  tkgrid(log)
  MakeLog(log, "alertlog", env = rclimdex.env)
  AddLog(paste("Loaded ", length(rclimdex.env$filelist)," file(s)", sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  for(i in 1:length(rclimdex.env$filelist)) {
    AddLog(paste(i, " - ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  }
}

## Create user interface for indices calulation
StartIC <- function() {
  font <- GetFont()
  main <- tktoplevel()
  assign("main", main, envir = rclimdex.env)

  ## Title
  tkwm.title(main, "RClimDex")
  MakeLabel(main, "Set Parameters for Indices Calculation", fontstyle = font$heading1, sticky = "w")
  MakeLabel(main, "", fontstyle = font$normal)

  ## User input
  top <- tkframe(main)
  tkgrid(top)
  MakeLabel(top, "Output files location : ", fontstyle = font$normal, sticky = "e")
  MakeEntry(top, rclimdex.env$parameter$file.loc, "entries.file.loc", row = 0, column = 1, sticky = "w", env = rclimdex.env)
  if(length(rclimdex.env$filelist) == 1) {
    MakeLabel(top, "Station name or code : ", fontstyle = font$normal, sticky = "e")
    MakeEntry(top, rclimdex.env$parameter$station, "entries.station", row = 1, column = 1, sticky = "w", env = rclimdex.env)
  }
  MakeLabel(top, "Missing marker : ", fontstyle = font$normal, row = 2, column = 0, sticky = "e")
  MakeEntry(top, rclimdex.env$parameter$missing.mark, "entries.missing.mark", row = 2, column = 1, sticky = "w", width = 10, env = rclimdex.env)
  MakeLabel(top, "Base period : ", fontstyle = font$normal, sticky = "e")
  MakeICEntry(top, list("First year", "Last year"), list("bp.first", "bp.last"), fontstyle = font$normal, row = 3, column = 1)
  MakeLabel(top, "Station location : ", fontstyle = font$normal, sticky = "e")
  MakeRadio(top, rclimdex.env$parameter$lat, list("Northern Hemisphere", "Southern Hemisphere"), "entries.lat", fontstyle = font$normal, row = 4, column = 1, sticky = "w", env = rclimdex.env)
  MakeLabel(top, "Threshold of daily maximum temperature : ", fontstyle = font$normal, sticky = "e")
  MakeICEntry(top, list("Upper", "Lower"), list("tmax.upper", "tmax.lower"), fontstyle = font$normal, row = 5, column = 1)
  MakeLabel(top, "Threshold of daily minimum temperature : ", fontstyle = font$normal, sticky = "e")
  MakeICEntry(top, list("Upper", "Lower"), list("tmin.upper", "tmin.lower"), fontstyle = font$normal, row = 6, column = 1)
  MakeLabel(top, "Threshold of daily precipitation (mm) : ", fontstyle = font$normal, sticky = "e")
  MakeEntry(top, rclimdex.env$parameter$prcp.threshold, "entries.prcp.threshold", row = 7, column = 1, sticky = "w", width = 5, env = rclimdex.env)

  ## Indices selections
  MakeLabel(main, "", fontstyle = font$normal)
  indices <- rclimdex.env$parameter[c("summ", "idmm", "trmm", "fdmm", "su", "id", "tr", "fd", "txx", "txn", "tnx", "tnn", "tx10p", "tx90p", "tn10p", "tn90p", "gsl", "wsdi", "csdi", "dtr", "rx1day", "rx5day", "r95p", "r99p", "r10mm", "r20mm", "rnnmm", "prcptot", "sdii", "cdd", "cwd")]
  MakeCheckList(main, indices, as.list(names(indices)), lapply("entries.", paste0, names(indices)), 4, fontstyle = font$normal, env = rclimdex.env)
  MakeLabel(main, "", fontstyle = font$normal)

  ## Action buttons, OK or Quit
  bottom <- tkframe(main)
  tkgrid(bottom)
  MakeButton(bottom, "Quit", PrepMain, sticky = "w")
  MakeButton(bottom, "Perform Indices Calculation", PerformIC, row = 0, column = 1, width = 30, sticky = "e")
  MakeLabel(main, "", fontstyle = font$normal)

  ## Log window
  log <- tkframe(main)
  tkgrid(log)
  MakeLog(log, "alertlog", env = rclimdex.env)
  AddLog(paste("Loaded ", length(rclimdex.env$filelist)," file(s)", sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  for(i in 1:length(rclimdex.env$filelist)) {
    AddLog(paste(i, " - ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  }
}

## Prepare for quality control user interface
## Load datasets
PrepQC <- function() {
  error <- PrepData(suffix = ".txt .csv")
  if(MakeAlert(error)) return()
  rclimdex.env$parameter$file.loc <- GetFileLoc(rclimdex.env$parameter$file.in)
  tkmessageBox(message = paste("Load data file: ", rclimdex.env$parameter$file.in, "Click OK to continue", sep = "\n"))
  Quit("main", rclimdex.env)
  StartQC()
}

## Prepare for indices calulation user interface
## Load datasets
## Assume datasets passed quality control
PrepIC <- function() {
  error <- PrepData(suffix = ".indcal.csv")
  if(MakeAlert(error)) return()
  rclimdex.env$parameter$file.loc <- GetFileLoc(rclimdex.env$parameter$file.in)
  tkmessageBox(message = paste("Load data file: ", rclimdex.env$parameter$file.in, "Click OK to continue", sep = "\n"))
  rclimdex.env$data <- SetMissing(rclimdex.env$data)
  rclimdex.env$parameter$station <- RemoveEnd(rclimdex.env$parameter$station, ".indcal")
  Quit("main", rclimdex.env)
  StartIC()
}

## Read and prepare datasets
PrepData <- function(name = NULL, suffix = ".csv") {
  if(is.null(name)) {
    ## Open file select interface
    error <- OpenFile(suffix)
    if(class(error) == "try-error") {
    return(error)
    }
  } else {
    ## Read dataset
    dataset <- ReadTable(name)
    if(class(dataset) == "try-error") {
      error <- paste("-Error-", "Unable to open file:", name, sep = "\n")
      class(error) <- "try-error"
      return(error)
    }
    ## Get dataset, file name and file path
    rclimdex.env$data <- dataset
    rclimdex.env$parameter$file.in <- name
    rclimdex.env$entries.file.in <- tclVar(as.character(name))
  }
  error <- DataCheck()
  if(class(error) == "try-error") {
    return(error)
  }
  ## Get output file name
  rclimdex.env$parameter$station <- GetFileName(rclimdex.env$parameter$file.in)
  rclimdex.env$entries.station <- tclVar(GetFileName(rclimdex.env$parameter$file.in)) 
  return("")
}

## Initiate quality control for all selected datasets
PerformQC <- function() {
  MakeProgress("bar", title = "Processing Quality Control", message = "Initializing quality control", env = rclimdex.env)
  ## For all selected files
  good <- 0
  for(i in 1:length(rclimdex.env$filelist)) {
    ## Get parameters
    GetQCParameter()
    ## Setup output folders
    SetOutputFolders(rclimdex.env$parameter$file.loc)
    UpdateProgress(rclimdex.env$bar, i/length(rclimdex.env$filelist), paste("Processing file ", i, "/", length(rclimdex.env$filelist), ": ", rclimdex.env$filelist[i], sep = ""))
    eflag <- 0
    if(!(rclimdex.env$parameter$file.in == rclimdex.env$filelist[1] & i == 1)) {  
      error <- PrepData(rclimdex.env$filelist[i])
      if(AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)) eflag <- 1
    }
    if(as.logical(eflag)) {
      AddLog("FAILED", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      AddLog(paste(i , " out of ", length(rclimdex.env$filelist), " file(s) FAILED: ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      next()
    }
    ## Parse parameters
    error <- ParseQCParameter()
    if(AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)) {
      AddLog("FAILED", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      AddLog(paste(i , " out of ", length(rclimdex.env$filelist), " file(s) FAILED: ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      next()
    }
    ## Perform quality control
    error <- QualityControl()
    if(AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)) {
      AddLog("FAILED", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      AddLog(paste(i , " out of ", length(rclimdex.env$filelist), " file(s) FAILED: ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      next()
    }
    AddLog(paste(i , " out of ", length(rclimdex.env$filelist), " file(s) done: ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
    good <- good + 1
  }
  AddLog(paste("Successful in ", good, "/", length(rclimdex.env$filelist), " files", sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  AddLog("Quality control complete", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  AddLog("Press Quit to exit", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  CloseProgress(rclimdex.env$bar)
}

## Initiate indices calulations for all selected datasets
PerformIC <- function() {
  MakeProgress("bar", title = "Processing Indices Calulations", message = "Initializing indices calculations", env = rclimdex.env)
  good <- 0
  ## For all selected files
  for(i in 1:length(rclimdex.env$filelist)) {
    ## Get parameters
    GetICParameter()
    GetICSelection()
    ## Setup output folders
    SetOutputFolders(rclimdex.env$parameter$file.loc)
    UpdateProgress(rclimdex.env$bar, i/length(rclimdex.env$filelist), paste("Processing file ", i, "/", length(rclimdex.env$filelist), ": ", rclimdex.env$filelist[i], sep = ""))
    eflag <- 0
    if(!(rclimdex.env$parameter$file.in == rclimdex.env$filelist[1] & i == 1)) {
      error <- PrepData(rclimdex.env$filelist[i])
      if(AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)) {
        eflag <- 1
      } else {
        rclimdex.env$data <- SetMissing(rclimdex.env$data)
      }
    }
    if(i > 1) {
      LoadICSelection(icpara)
    } else {
      icpara <- SaveICSelection()
    }
    rclimdex.env$parameter$station <- RemoveEnd(rclimdex.env$parameter$station, ".indcal")
    if(length(rclimdex.env$data) != 6) eflag <- 1
    #eflag <- FilterICSelection(rclimdex.env$data)
    FilterICSelection(rclimdex.env$data)
    if(as.logical(eflag)) {
      AddLog("FAILED", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      AddLog(paste(i , " out of ", length(rclimdex.env$filelist), " file(s) FAILED: ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      next()
    }
    ## Parse parameters
    error <- ParseICParameter(rclimdex.env$data)
    if(AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)) {
      AddLog("FAILED", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      AddLog(paste(i , " out of ", length(rclimdex.env$filelist), " file(s) FAILED: ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      next()
    }
    error <- ParseICSelection()
    if(AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)) {
      AddLog("FAILED", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      AddLog(paste(i , " out of ", length(rclimdex.env$filelist), " file(s) FAILED: ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
      next()
    }
    ## Perform indices calulations
    climdexinput <- ClimdexInput()
    IndicesCalculation(climdexinput)
    AddLog(paste(i , " out of ", length(rclimdex.env$filelist), " file(s) done: ", rclimdex.env$filelist[i], sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
    good <- good + 1
  }
  AddLog(paste("Successful in ", good, "/", length(rclimdex.env$filelist), " files", sep = ""), rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  AddLog("Indices Calculation complete", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  AddLog("Press Quit to exit", rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  CloseProgress(rclimdex.env$bar)
}

## Prepare for main title
## Close previous window
PrepMain <- function() {
  Quit("main", rclimdex.env)
  StartTitle()
}

## Shortcut to make compound label and entry box in indices calulation user interface
MakeICEntry <- function(frame, message, initial, fontstyle = NULL, row = NULL, column = -1, padx = 0, pady = 0, width = 10) {
  top <- tkframe(frame)
  fontstyle <- DefaultFont(fontstyle)
  counter <- -1
  num <- 1:length(initial)
  for(i in num) {
    counter <- counter+1
    MakeLabel(top, as.character(message[i]), fontstyle = fontstyle, sticky = "e", row = 0, column = counter, width = 10)
    counter <- counter+1
    name <- paste("entries.", initial[i], sep = "")
    MakeEntry(top, as.character(rclimdex.env$parameter[match(initial[i], names(rclimdex.env$parameter))]), name, row = 0, column = counter, sticky = "w", width = width, env = rclimdex.env)
  }
  DisplayObject(top, row, column, padx, pady, "")
}

## Start RClimDex
rclimdex.start <- function() {
  options(warn = 2)
  SetFont()
  SetParameter()
  StartTitle()
}
