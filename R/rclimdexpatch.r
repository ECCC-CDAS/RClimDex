## rclimdexpatch.r
## Temporary patch in FDmm, SUmm, IDmmm, TRmm, Tmaxmean and Tminmean to climdex.pcic
## Retains coding style of climdex.pcic
## For example, climdex.function(climdexinput, options)

## Requires climdex.pcic
library(climdex.pcic)

## Returns a Tmaxmean series
## Default is both monthly and annual
climdex.tmaxmean <- function(ci, freq = c("monthly", "annual")) {
  if(freq[1] == "annual") {
    return(tapply(ci@data$tmax, ci@date.factors[freq[1]], mean, na.rm = TRUE) * ci@namasks$annual$tmax)
  } else {
    return(tapply(ci@data$tmax, ci@date.factors[freq[1]], mean, na.rm = TRUE) * ci@namasks$monthly$tmax)
  }
}

## Returns a Tminmean series
## Default is both monthly and annual
climdex.tminmean <- function(ci, freq = c("monthly", "annual")) {
  if(freq[1] == "annual") {
    return(tapply(ci@data$tmin, ci@date.factors[freq[1]], mean, na.rm = TRUE) * ci@namasks$annual$tmin)
  } else {
    return(tapply(ci@data$tmin, ci@date.factors[freq[1]], mean, na.rm = TRUE) * ci@namasks$monthly$tmin)
  }
}

## Expand the original function of climdex.fd with an option for a different threshold
## Function defaults to FD0 if threshold is not provided
climdex.fd <- function(ci, threshold = 0) {
  return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, threshold, "<") * ci@namasks$annual$tmin)
}

## Expand the original function of climdex.su with an option for a different threshold
## Function defaults to SU25 if threshold is not provided
climdex.su <- function(ci, threshold = 25) {
  return(number.days.op.threshold(ci@data$tmax, ci@date.factors$annual, threshold, ">") * ci@namasks$annual$tmax)
}

## Expand the original function of climdex.id with an option for a different threshold
## Function defaults to ID0 if threshold is not provided
climdex.id <- function(ci, threshold = 0) {
  return(number.days.op.threshold(ci@data$tmax, ci@date.factors$annual, threshold, "<") * ci@namasks$annual$tmax)
}

## Expand the original function of climdex.tr with an option for a different threshold
## Function defaults to TR20 if threshold is not provided
climdex.tr <- function(ci, threshold = 20) {
  return(number.days.op.threshold(ci@data$tmin, ci@date.factors$annual, threshold, ">") * ci@namasks$annual$tmin)
}

## Patch fix for all NA in ClimdexInput
## Return a column from dataset with NAs filter out except when all NA
FilterColumn <- function(dataset, var, column = NULL) {
  if(is.null(column)) column <- var
  result <- dataset[!is.na(dataset[, var]), column]
  if(length(result) > 0) {
    return(result)
  } else if(column == "year" | column == "month" | column == "day") {
    return(dataset[!is.na(dataset[, "month"]), column])
  } else {
    return(dataset[!is.na(dataset[, "month"]), "month"])
  }
  return(result)
}

## Lower overhead version of tapply
## From climdex.pcic
tapply.fast <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE) {
  FUN <- if (!is.null(FUN))
    match.fun(FUN)

  if(!is.factor(INDEX))
    stop("INDEX must be a factor.")

  if (length(INDEX) != length(X))
    stop("arguments must have same length")

  if (is.null(FUN))
    return(INDEX)

  namelist <- levels(INDEX)
  ans <- lapply(split(X, INDEX), FUN, ...)

  ans <- unlist(ans, recursive = FALSE)
  names(ans) <- levels(INDEX)
  return(ans)
}

## Get NA mask given threshold and split factor
## From climdex.pcic
get.na.mask <- function(x, f, threshold) {
  return(c(1, NA)[1 + as.numeric(tapply.fast(is.na(x), f, function(y) { return(sum(y) > threshold) } ))])
}
#lapply(ci@data, get.na.mask, ci@date.factors$annual, 15)
