
# method1 <- "Spline"
# n1 <- 100
# nPerc1 <- NULL
# p1 <- NULL
#
# method2 <- NULL
# n2 <- NULL
# nPerc2 <- NULL
# p2 <- NULL
#
# arMAX <- NULL
# makeAr <- NULL
# .height <- NULL
# biweightMean <- NULL
#
# DataBaseChoice <- NULL
# TempDataBase <- NULL
# fname <- NULL
# DataBaseChoice <- NULL
#
# makeSegPlot <- NULL
# remove.shorter.series <- NULL
# delete.shorter.series <- NULL
#
# makeFirstDetrending <- NULL
# makeSecondDetrending <- NULL
#
#
# save.detrend2 <- NULL
#
# first.year.common <- NULL
# last.year.common <- NULL
# winLength <- NULL
# stepWin <- NULL
# current_spline_n <- NULL
# current_spline_nPerc <- NULL
# current_spline_p <- NULL
#
#
# first_detrending_method <- NULL
# interactive.detrend <- NULL
# make.common.EPS <- NULL
# make.select.period.EPS <- NULL
# run.win.analysis <- NULL
# save.detrend1 <- NULL
# second_detrending_method <- NULL
# stc <- NULL

FirstYear <- function(rwl) {
  firstyear <- min(as.integer(rownames(rwl)))
  return(firstyear)
}

LastYear <- function(rwl) {
  lastyear <- max(as.integer(rownames(rwl)))
  return(lastyear)
}

as.logic <- function(x) {
  return(as.logical(as.integer(x)))
}

f.apply <- function(FUN = NULL, simplify = TRUE, ...) {
  # arrFiles<-as.matrix(sort(choose.files())) #tk_choose.files                     #select n files to be open
  arrFiles <- as.matrix(sort(tk_choose.files()))
  n_files <- dim(arrFiles)[1]
  if (n_files < 1) {
    return(
      invisible(tk_messageBox(
        type = "ok",
        "You should select at least one file!",
        caption = "Problems"
      )) # stop if no files were selected
    )
  }

  n.files <- sapply(
    X = arrFiles,
    FUN = FUN,
    simplify = simplify,
    ...
  )
  if (length(n.files) != n_files) {
    tk_messageBox(
      type = "ok",
      "There is some kind of problems!",
      caption = "Problems"
    )
    return(n.files)
  }
}

toNumber <- function(x) {
  as.fractions <- function(x) {
    as.integer((strsplit(x, "/")[[1]][1])) / as.integer((strsplit(x, "/")[[1]][2]))
  }
  if (length(grep("/", x) == 1)) {
    return(as.fractions(x))
  }
  return(as.numeric(x))
}

getFileName <- function(fullpath) {
  fullpath <- rmExt(fullpath)
  fullpath <- basename(fullpath)
  return(fullpath)
}
