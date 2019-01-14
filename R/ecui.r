## ecui.r
## Library of redefined user-interface, input and output functions for R implemenation of tcl/tk graphical user interface
## To provide a easy, straight forward, standardized way to implement graphical user interface and I/O for our projects
## The GUI and I/O functions shall be self-contained and flexible

## Requires tcl/tk
library(tcltk)

## Write out a CSV formatted table given the dataset, header and output location
WriteTable <- function(dataset, label, outpath) {
  write.table(label, file = outpath, append = FALSE, quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  write.table(dataset, file = outpath, append = TRUE, quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
}

## Write out the trend of a dataset in CSV format given the dataset and output location
## ROW BY ROW, everycall to the function writes out one row and do trend analysis on one dataset only
## Options are for the rowname and colname
## When the colname (label=T) is given the file is created/overwritten and the input (dataset) are the colname
## Otherwise (default or label=F), a dataset is expected to calulate the trend and the file is appended
## The name of the trend analaysis of the given dataset is taken from the name parameter, it defaults to ""
WriteTrend <- function(dataset, outpath, name = "", label = FALSE) {
  if(!label) {
        ## Trend analysis
        ## Currently will fail when there are NAs in indices
        ## Temp solution of ignore warnings
        options(warn=-1)
        fit <- lsfit(dataset[, 1], dataset[, 2])
        options(warn=2)
        stat <- GetStat(fit)
    ## Append table with a single row: rowname, trend results
    write.table(cbind(name, dataset[1, 1], dataset[nrow(dataset), 1], stat$beta, stat$betaerror, stat$pvalue), file = outpath, append = TRUE, quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  } else {
    ## New file with colname in first row
    write.table(dataset, file = outpath, append = FALSE, quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  }
}

## Write out pre-defined plot in pdf format given the dataset and output location
## Options are for title, x-axis label and y-axis label
WritePlot <- function(dataset, outpath, main = "", xlab = "", ylab = "") {
#  jpeg(outpath, width = 1024, height = 768) Write out pre-defined plot in 1024x768 jpeg format given the dataset and output location
  pdf(file = outpath, width = 14.23, height = 10.67)
  original <- par(no.readonly = TRUE)
  par(cex = 2, mgp = c(2.5, 0.5, 0))
  plot(dataset, xlab = xlab, ylab = ylab, type = "b")
  ## Calculate R2, p-value, slope and slope-error
  ## Currently will fail when there are NA in the indices
  ## Temporary ignore warnings
  options(warn=-1)
  fit <- lsfit(dataset[, 1], dataset[, 2])
  options(warn=2)
  stat <- GetStat(fit)
  abline(fit, lwd = 3)
  naomit <- na.omit(dataset)
  lines(lowess(naomit[, 1], naomit[, 2]), lwd = 3, lty = 2)
  title(main = main)
  title(sub = bquote("R"^"2"*"="~.(stat$r2)~" p-value="~.(stat$pvalue)~" Slope estimate="~.(stat$beta)~" Slope error="~.(stat$betaerror)), cex = 0.5)
  par(original)
  dev.off()
}

## Write our pre-defined time-series plot in pdf format given the dataset and output location
## Options are type of plot (defaults to line plot), title, variable name, units, x-axis label, y-axis label, positive only flag and distribution plot flag along with dplot title
WriteTimeSeries <- function(dataset, outpath, type = "l", name = "", var = "", unit = NULL, xlab = "", ylab = "", yzero = FALSE, distribution = FALSE, dtitle = "") {
  pdf(file = outpath)
  original <- par(no.readonly = TRUE)
  ## Can draw distritbution plot on first page
  if(distribution) {
    DistributionTimeSeries(dataset, dtitle)
  }
  if(sum(is.na(dataset[, ncol(dataset)])) == nrow(dataset)) {
    ymax <- 100
    ymin <- -100
  } else {
    ymax <- min(max(dataset[, ncol(dataset)], na.rm = TRUE), 100)
    ymin <- max(min(dataset[, ncol(dataset)], na.rm = TRUE), -100)
  }
  if(yzero) ymin <- 0
  par(mfrow = c(4, 1))
  par(mar = c(3.1, 2.1, 2.1, 2.1), cex = 1)
  plotwidth <- -1
  for(i in seq(dataset[1, 1], dataset[nrow(dataset), 1], 10)) {
    rowend <- min(i+9, dataset[nrow(dataset), 1])
    rowdata <- dataset[dataset[, 1] >= i & dataset[, 1] <= rowend, ]
    rowyear <- which(!duplicated(rowdata[rowdata[, 1] >= i & rowdata[, 1] <= rowend, 1]))
    if(plotwidth < 0) plotwidth <- nrow(rowdata)
    plot(1:nrow(rowdata), rowdata[, ncol(rowdata)], type = type, col = "blue", xlab = "", ylab = "", xaxt = "n", xlim = c(1, plotwidth), ylim = c(ymin, ymax))
    abline(h = 0)
    rowna <- seq(1, nrow(rowdata))
    rowna <- rowna[is.na(rowdata[, ncol(rowdata)]) == TRUE]
    axis(side = 1, at = rowyear, labels = rowdata[rowyear, 1])
    ## Add yellow dividers for every 10 years
    for(k in 1:length(rowyear)) abline(v = rowyear[k], col = "yellow")
    axis(side = 1, at = nrow(rowdata)+1, labels = rowend+1)
    abline(v = nrow(rowdata)+1, col = "yellow")
    ## Add red circles for missing time step
    lines(rowna, rep(0, length(rowna)), type = "p", col = "red")
    if(!is.null(unit)) {
      title(bquote("Station: "~.(name)~", "~.(i)~"~"~.(rowend)~", "~.(var)~" ("~.(unit)~")"))
    } else {
      title(bquote("Station: "~.(name)~", "~.(i)~"~"~.(rowend)~", "~.(var)))
    }
  }
  par(original)
  dev.off()
}

## Draw pre-defined distribution plot given the dataset and output location
DistributionTimeSeries <- function(dataset, title = "") {
  histdata <- dataset[dataset[, ncol(dataset)] >= 1, ncol(dataset)]
  histdata <- histdata[is.na(histdata) == FALSE]
  if(length(histdata) > 30) {
    hist(histdata, main = title, breaks = c(seq(0, 20, 2), max(30, histdata)), xlab = "", col = "green", freq = FALSE)
    lines(density(histdata, bw = 0.2, from = 1), col = "red")
  }
}

## Get R2, p-value, beta and betaerror from lsfit() analysis results
GetStat <- function(bestfit) {
  out <- ls.print(bestfit, print.it = FALSE)
  r2 <- round(100*as.numeric(out$summary[1, 2]), 1)
  pvalue <- round(as.numeric(out$summary[1, 6]), 3)
  beta <- round(as.numeric(out$coef.table[[1]][2, 1]), 3)
  betaerror <- round(as.numeric(out$coef.table[[1]][2, 2]), 3)
  result <- list(r2, pvalue, beta, betaerror)
  names(result) <- c("r2", "pvalue", "beta", "betaerror")
  return(result)
}

## Open, read and return table given input file's path
## Get file format from file name, no header/colnames/rownames in file
## Can read from ASCII spaced delimited file (.txt) and CSV format table (.csv)
ReadTable <- function(name) {
  size <- nchar(name)
  type <- substr(name, size-3, size)
  if(type == ".txt") {
    data <- try(read.table(name, header = FALSE), silent = TRUE)
  } else if(type == ".csv") {
    data <- try(read.csv(name, header = FALSE), silent = TRUE)
  } else {
    data <- ""
    ## Gives error if unable to read file
    class(data) <- "try-error"
  }
  return(data)
}

## Return file location given file path
GetFileLoc <- function(name) {
  folders <- unlist(strsplit(name, "/"))
  out <- substr(name, 1, nchar(name)-nchar(folders[length(folders)]))
  return(out)
}

## Return file name given file path
GetFileName <- function(name) {
  folders <- unlist(strsplit(name, "/"))
  file <- folders[length(folders)]
  out <- substr(file, 1, nchar(file)-4)
  return(out)
}

## Make folders within given folder location with given folder name
MakeFolder <- function(path, name) {
  out <- paste(path, name, sep = "")
  if(!file.exists(out)) {
    dir.create(out)
  }
}

## Write to log file within given log file name and the message to log
WriteLogFile <- function(name, message) {
  adding <- TRUE
  if(!file.exists(name)) {
    adding <- FALSE
  }
  write(message, file = name, append = adding)
}

## Return file name without file format suffix given the file name and suffix
RemoveEnd <- function(name, match) {
  if(substr(name, nchar(name)-nchar(match)+1, nchar(name)) == match) {
    name <- substr(name, 1, nchar(name)-nchar(match))
  }
  return(name)
}

## Setup pre-defined fonts to be used in user interface
## The pre-defined fonts are Heading, Sub-Heading1, Sub-Heading2, Body and Fixed-width Body
## Can be accessed by heading, heading1, heading2, normal, fixedwidth
SetFont <- function() {
  names <- c("heading", "heading1", "heading2", "normal", "fixedwidth")
  fonts <- as.character(tkfont.names())
  if(any(is.na(match(names, fonts)))) {
    heading <- tkfont.create("heading", family = "times", size = 40, weight = "bold", slant = "italic")
    heading1 <- tkfont.create("heading1", family = "times", size = 20, weight = "bold")
    heading2 <- tkfont.create("heading2", family = "times", size = 14, weight = "bold")
    normal <- tkfont.create("normal", family = "times", size = 12)
    fixedwidth <- tkfont.create("fixedwidth", family = "courier", size = 12)
  }
}

## Return the pre-defined fonts to be used in user interface
GetFont <- function() {
  font <- c(tkfont.actual("heading"), tkfont.actual("heading1"), tkfont.actual("heading2"), tkfont.actual("normal"), tkfont.actual("fixedwidth"))
  names(font) <- c("heading", "heading1", "heading2", "normal", "fixedwidth")
  return(font)
}

## Check string if it containes only allowed characters
## Parameters are allowed characters (regular expressions), strings to be parsed error message if failed
## Please look up regex R help page for more about regular expression
ParseError <- function(pattern, string, message) {
  if(attr(regexec(pattern, string)[[1]], "match.length") !=  nchar(string) || nchar(string) == 0)  {
        error <- paste("-Error-", message, sep = "\n")
        class(error) <- "try-error"
    return(error)
  }
  return("")
}

## Chack if the number is within range
## Parameters are the number, upper bound, lower bound and error message if failed
## This is not parsing evernthough the function is called ParseRange
ParseRange <- function(num, upper, lower, message) {
  ## Corrects the upper and lower bound if it is reversed
  if(lower > upper) {
        tmp <- upper
        upper <- lower
        lower <- tmp
  }
  if(num > upper || num < lower) {
    error <- paste("-Error-", message, sep = "\n")
        class(error) <- "try-error"
    return(error)
  }
  return("")
}

## Chack if the lower threshold is not larger than the upper threshold
## Parameters are the upper threshold, lower threshold and error message if failed
## This is not parsing evernthough the function is called ParseReverse
ParseReverse <- function(upper, lower, message) {
  if(lower > upper) {
    error <- paste("-Error-", message, sep = "\n")
        class(error) <- "try-error"
    return(error)
  }
  return("")
}

## Create a label onto the user interface given the frame to display and message of label
## Options are fontstyle, row of frame, col of frame, x padding, y padding, position, width of label
## The position is controlled by sticky which means stick to which border within frame, north (n), south (s), east (e), west (w) or any combinations
## For example, sticky = "ewn" means the label sticks to north, east and west edge of frame, horizontally label will be stretched to satisfy the condition
MakeLabel <- function(frame, message, fontstyle = NULL, row = NULL, column = NULL, padx = 0, pady = 0, sticky = "", width = NULL) {
  fontstyle <- DefaultFont(fontstyle)
  if(is.null(width)) {
    label <- tklabel(frame, text = message, font = fontstyle)
  } else {
    label <- tklabel(frame, text = message, font = fontstyle, width = width)
  }
  DisplayObject(label, row, column, padx, pady, sticky)
}

## Create a button onto the user interface given the frame to display, message of button and action when button pressed
## Options are fontstyle, row of frame, col of frame, x padding, y padding, position, width of button and height of button
## Action is name of function to provoke when button is pressed
## See comments of MakeLabel for more information on postion controls (sticky)
MakeButton <- function(frame, message, action, fontstyle = NULL, row = NULL, column = NULL, padx = 0, pady = 0, sticky = "", width = 10, height = 1) {
  fontstyle <- DefaultFont(fontstyle)
  button <- tkbutton(frame, text = message, command = action, width = width, height = height, font = fontstyle)
  DisplayObject(button, row, column, padx, pady, sticky)
}

## Create a entrybox onto the user interface given the frame to display button, initial value of entrybox and name of entrybox
## Options are fontstyle, row of frame, col of frame, x padding, y padding, position, width of entry and environment to store the entrybox
## See comments of MakeLabel for more information on postion controls (sticky)
MakeEntry <- function(frame, initial, name, fontstyle = NULL, row = NULL, column = NULL, padx = 0, pady = 0, sticky = "", width = 30, env = .GlobalEnv) {
  var <- tclVar(initial)
  fontstyle <- DefaultFont(fontstyle)
  entry <- tkentry(frame, textvariable = var, width = width, font = fontstyle)
  DisplayObject(entry, row, column, padx, pady, sticky)
  assign(name, var, envir = env)
}

## Create a radio selections onto the user interface given the frame to display, initial selected radiobutton, list of selections and name of radio selections
## Options are fontstyle, row of frame, col of frame, x padding, y padding, position and environment to store the radio selections
## List of selections must be in a list
## Creates the selections in the same row
##
## For example, (x) selection1 ( ) selection2 ( ) selection3
##
## See comments of MakeLabel for more information on postion controls (sticky)
MakeRadio <- function(frame, initial, label, name, fontstyle = NULL, row = NULL, column = NULL, padx = 0, pady = 0, sticky = "", env = .GlobalEnv) {
  fontstyle <- DefaultFont(fontstyle)
  subframe <- tkframe(frame)
  var <- tclVar(as.character(initial))
  ## Get number of selections
  num <- 1:length(label)
  for (i in num) {
    radio <- tkradiobutton(subframe, variable = var, value = as.character(i))
    DisplayObject(radio, 0, (i-1)*2, 0, 0, "")
    MakeLabel(subframe, as.character(label[i]), fontstyle, 0, (i-1)*2+1)
    if(as.character(i) == initial) {
      tcl(radio, "select")
    }
  }
  DisplayObject(subframe, row, column, padx, pady, sticky)
  assign(name, var, envir = env)
}

## Create a table of checkbox selections (allows multiple selections) given the frame to display, list of initial selected box and list of selections
## Options are number of column in table of checkboxes, fontstyle, row of frame, col of frame, x padding, y padding, position and environment to store
##
## For example, [x] selection1 [ ] selection2 [x] selection3
##              [ ] selection4 [x] selection5 [x] selection6
##              [x] selection7 [ ] selection8 [x] selection9
##
## See comments of MakeLabel for more information on postion controls (sticky)
MakeCheckList <- function(frame, initial, label, name, ncol = 1, fontstyle = NULL, row = NULL, column = NULL, padx = 0, pady = 0, sticky = "", env = .GlobalEnv) {
  fontstyle <- DefaultFont(fontstyle)
  subframe <- tkframe(frame)
  num <- 1:length(initial)
  for(i in num) {
    x <- floor((i-1)/ncol)
    y <- (i-1)%%ncol*2
    MakeLabel(subframe, as.character(label[i]), fontstyle = fontstyle, sticky = "e", row = x, column = y)
    var <- tclVar(as.character(initial[i]))
    check <- tkcheckbutton(subframe, variable = var)
    DisplayObject(check, x, y+1, 0, 0, "")
    assign(name[[1]][i], var, envir = env)
  }
  DisplayObject(subframe, row, column, padx, pady, sticky)
}

## Create a textbox for logs to user given the frame to display and name of log
## Options are fontstyle, background color, row of frame, col of frame, x padding, y padding, position, width, height and environment to log box
## Log box will have vertical bar only, logs will be wrapped to the next line if necessary and read-only
MakeLog <- function(frame, name, fontstyle = NULL, bg = "gray", row = 0, column = 0, padx = 0, pady = 0, sticky = "", width = 10, height = 10, env = .GlobalEnv) {
  fontstyle <- DefaultFont(fontstyle)
  scroll <- tkscrollbar(frame, repeatinterval = 5, command = function(...)tkyview(log, ...))
  log <- tktext(frame, height = height, yscrollcommand = function(...)tkset(scroll, ...), bg = bg, state = "disabled", font = fontstyle, wrap = "word")
  DisplayObject(log, row, column, padx, pady, sticky)
  DisplayObject(scroll, row, column+1, padx, pady, "nsw")
  assign(name, log, envir = env)
}

## Insert log message in to log box given the message and log box
AddLog <- function(error, log, logfile = "log.txt") {
  if(!is.null(error)) {
    if(length(error) > 0) {
      tkconfigure(log, state = "normal")
      errortime <- paste("[", substring(Sys.time(),12,19), "] ", error, "\n", sep = "")
      tkinsert(log, "end", errortime)
      ## Debug + Temp Sol. for Prgress
#      print(paste("[", substring(Sys.time(),12,19), "] ", error, "\n", sep = ""))
      tksee(log, "end")
      tkconfigure(log, state = "disabled")
      if(!is.na(logfile)) {
        WriteLogFile(logfile, errortime)
      }
    }
  }
  ## eval() error also defaults to try-error class
  if(class(error) == "try-error") {
    ## Return error (stop/skip)
    return(TRUE)
  }
  ## Return warnings (countinue)
  return(FALSE)
}

## Make progress bar to display the current status given the name of the progress bar
## Options are title of window, message to display, width and environment to store the progress bar
MakeProgress <- function(name, title = "", message = "", width = 500, env = .GlobalEnv) {
  progress <- tkProgressBar(title = title, label = message, min = 0, max = 1, width = width)
  assign(name, progress, envir = env)
}

## Update the progress bar given the name of the progress bar, the completion percentage and message to display
UpdateProgress <- function(name, number, message) {
  setTkProgressBar(name, number, label=message)
}

## Close the progress bar given its name
CloseProgress <- function(name) {
  close(name)
}

## Pop-up and alert box with message
MakeAlert <- function(error) {
  ## eval() errors also defaults to try-error class
  if(class(error) == "try-error") {
    tkmessageBox(message = error, icon = "error")
    ## Return error (stop/skip)
    return(TRUE)
  }
  if(class(error) == "error") {
    tkmessageBox(message = error, icon = "warning")
  }
  ## Return warnings (countinue)
  return(FALSE)
}

## Return the default font
DefaultFont <- function(fontstyle) {
  if(is.null(fontstyle)) {
        return(tkfont.actual("normal"))
  }
  return(fontstyle)
}

## Put object on the user interface given object, row of frame, col of frame, x padding, y padding and position
## See comments of MakeLabel for more information on postion controls (sticky)
DisplayObject <- function(object, row, column, padx, pady, sticky) {
  if(!is.null(row) && !is.null(column)) {
    tkgrid(object, padx = padx, pady = pady, sticky = sticky, row = row, column = column)
  } else if(!is.null(row)) {
    tkgrid(object, padx = padx, pady = pady, sticky = sticky, row = row)
  } else if(!is.null(column)) {
    tkgrid(object, padx = padx, pady = pady, sticky = sticky, column = column)
  } else {
    tkgrid(object, padx = padx, pady = pady, sticky = sticky)
  }
}

## Close given frame/window from given environment
Quit <- function(frame, env = .GlobalEnv) {
  tkdestroy(get(frame, envir = env))
}
