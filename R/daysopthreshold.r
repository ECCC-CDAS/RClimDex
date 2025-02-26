## daysopthreshold.r
## Request from Xuebin Zheng for collaborations in China
## Retains coding style of climdex.pcic
## For example, climdex.function(climdexinput, options)

## Requires climdex.pcic
library(climdex.pcic)

## Change the original function of climdex.tn10p to be number of days op threshold instead of percent op threshold
climdex.tn10p <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); return(num.days.op.threshold(ci@data$tmin, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmin$outbase$q10, ci@quantiles$tmin$inbase$q10, ci@base.range, "<", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmin) }

## Change the original function of climdex.tx10p to be number of days op threshold instead of percent op threshold
climdex.tx10p <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(num.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q10, ci@quantiles$tmax$inbase$q10, ci@base.range, "<", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax) }

## Change the original function of climdex.tn90p to be number of days op threshold instead of percent op threshold
climdex.tn90p <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); return(num.days.op.threshold(ci@data$tmin, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmin$outbase$q90, ci@quantiles$tmin$inbase$q90, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmin) }

## Change the original function of climdex.tx90p to be number of days op threshold instead of percent op threshold
climdex.tx90p <- function(ci, freq=c("monthly", "annual")) { stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); return(num.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q90, ci@quantiles$tmax$inbase$q90, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax) }

## Change the original function of percent.days.op.threshold to be num.days.op.threshold
num.days.op.threshold <- function(temp, dates, jdays, date.factor, threshold.outside.base, base.thresholds, base.range, op='<', max.missing.days) {
  f <- match.fun(op)
  dat <- f(temp, threshold.outside.base[jdays])

  inset <- dates >= base.range[1] & dates <= base.range[2]
  ## Don't use in-base thresholds with data shorter than two years; no years to replace with.
  if(sum(inset) > 0 && length(dates) >= 360 * 2) {
    jdays.base <- jdays[inset]
    years.base <- get.years(dates[inset])

    ## Get number of base years, subset temp data to base period only.
    temp.base <- temp[inset]
    years.base.range <- range(years.base)
    byrs <- (years.base.range[2] - years.base.range[1] + 1)

    ## Linearize thresholds, then compare them to the temperatures
    bdim <- dim(base.thresholds)
    dim(base.thresholds) <- c(bdim[1] * bdim[2], bdim[3])
    yday.byr.indices <- jdays.base + (years.base - get.years(base.range)[1]) * bdim[1]
    f.result <- f(rep(temp.base, byrs - 1), base.thresholds[yday.byr.indices,])
    dim(f.result) <- c(length(yday.byr.indices), bdim[3])

    ## Chop up data along the 2nd dim into a list; sum elements of the list
    dat[inset] <- rowSums(f.result, na.rm=TRUE) / (byrs - 1)
  }
  dat[is.nan(dat)] <- NA
  if(missing(date.factor))
    return(dat)
  na.mask <- get.na.mask(dat, date.factor, max.missing.days)
  ## FIXME: Need to monthly-ize the NA mask calculation, which will be ugly.
  ret <- tapply.fast(dat, date.factor, sum, na.rm=TRUE) * na.mask
  ret[is.nan(ret)] <- NA
  return(ret)
}

## Get year
## From climdex.pcic
get.years <- function(dates) {
  return(as.POSIXlt(dates)$year + 1900)
}
