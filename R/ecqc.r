## ecqc.r
## Library of quality control functions for R
## To provide a easy, straight forward, standardized way to implement quality control for our projects
## The QC functions shall be self-contained and flexible

## Return a column from dataset with NAs filter out
## Parameters are dataset and variable (colname) to be filtered
## Options is target column if it is not the same as variable (colname) to be filtered
## For example, filter column date to dates that don't have NAs in tmax
FilterColumn <- function(dataset, var, column = NULL) {
  if(is.null(column)) column <- var
  return(dataset[!is.na(dataset[, var]), column])
}

## Check if dataset fits criteria given
## Parameters are dataset, evaluation statement, message if failed, and list of columns of the dataset to evaluate
## Evaulation statment for example: 'abs(dataset) > 70'
QualityCheck <- function(dataset, criteria, message, series) {
  if(any(eval(parse(text = criteria)), na.rm = TRUE)) {
    dataset[which(eval(parse(text = criteria))), unlist(series)] <- NA
    error <- paste("-Caution-", message, sep = "\n")
    class(error) <- "error"

    ## Write to Alert/Logbox if fail
    ## Currently calls both rclimdex.r and ecui.r which is not good, not self-contained code
    ## Will fix in update
    ## Comment out the MakeAlert and AddLog to use this function
    #MakeAlert(error)
    AddLog(error, rclimdex.env$alertlog, logfile = rclimdex.env$logfile)
  }
  return(dataset)
}

## Get outlier threshold for each day of the year in daily stat given the series of data, month, day and threhold to be outlier
## Default is 3 standard deviation
FindOutlier <- function(series, month, day, threshold = 3) {
  date <- month*100+day
  std <- DailyStat(series, date, "sd")
  average <- DailyStat(series, date, "mean")
  result <- data.frame(cbind(average-threshold*std, average+threshold*std))
  names(result) <- c("low", "up")
  return(result)
}

## Get monthly stat from daily dataset based on evaluation criteria (mean, max, etc.)
## Parameters are the series of data, month, day and evaluation criteria
## Evaulation criteria for example 'sum' for sum()
MonthlyStat <- function(series, month, year, fun) {
  monthly <- matrix(tapply(series, month, eval(parse(text = fun)), na.rm = TRUE), ncol = 12, byrow = TRUE)
  annual <- tapply(series, year, eval(parse(text = fun)), na.rm = TRUE)
  result <- data.frame(cbind(names(annual), monthly, annual))
  names(result) <- c("year", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec", "ann")
  return(result)
}

## Get daily stat from daily dataset based on evaluation criteria (mean, max, etc.)
## Parameters are the series of data, date in mm0dd format and evaluation criteria
## Evaulation criteria for example 'sd' for sd()
DailyStat <- function(series, date, fun) { 
  ## Leap days will repeat result of Feb 28 instead
  days <- rep("", 366)
  result <- rep("", length(series))
  annual <- tapply(series, date, eval(parse(text = fun)), na.rm = TRUE)
  days[as.numeric(names(annual))] <- as.numeric(annual)
  days[229] <- days[228]
  result <- as.numeric(days[date])
  return(result)
}

## Check if given series of data is numeric only
## Gives warning if not
AllNumeric <- function(series) {
  options(warn = -1)
  result <- as.numeric(as.character(series))
  options(warn = 2)
  return(result)
}

## Assign colname to the unassigned column that best fit the criteria evaluated
## Parameters are dataset, colname and evaluation criteria
## Evaulation criteria for example 'max' for max()
## Options are second evaluation statment, override column flag, unassigned column flag
AssignCol <- function(dataset, label, fun, rule = NULL, override = FALSE, unknown = "data") {
  if(fun != "sum") {
    result <- lapply(dataset, eval(parse(text = fun)), na.rm = TRUE)
  } else {
    ## Allow second evaulation statement if first evaulation criteria is the sum of something ('sum')
    result <- lapply(dataset, SumCondition, criteria)
  }
  name <- names(dataset)
  col <- which.max(result)
  for(i in 1:ncol(dataset)) {
    if(name[col] == unknown || override) {
       name[col] <- label
       break
    } else {
      result[col] <- -99.9
      col <- which.max(result)
    }
  }
  names(dataset) <- name
  return(dataset)
}

## Get number of values in given dataset the fits given evaulation statament
## For example, 'dataset < 0'
SumCondition <- function(dataset, criteria) {
  return(sum(eval(parse(text = criteria)), na.rm = TRUE))
}
