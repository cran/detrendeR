detrendeRoptions <- function(Stc = c(5, 2, 1), ...) {
  #### 0. Head ####
  separator98 <- rep("=", 98)
  separator_98 <- rep("_", 98)
  cat(
    separator98,
    "\ndetrendeR",
    rep("\t", 8),
    "[",
    date(),
    "]\nfcampelo@ci.uc.pt\n",
    separator98,
    "\ndetrendeRoptions:\n",
    separator_98,
    "\n",
    sep = ""
  )
  #### 1. Tree mask ####
  cat("1. Tree mask: ",
    rep("S", Stc[1]),
    rep("T", Stc[2]),
    rep("c", Stc[3]),
    "\n",
    sep = ""
  )

  #### 2. Make segment plot ####
  makeSegPlot <- .get("makeSegPlot")
  mkSegPlot <- ifelse(makeSegPlot, "Yes", "No")

  cat("2. Make segment plot:", mkSegPlot, "\n")

  #### 3. Remove series shorter than ####
  remove.shorter.series <- .get("remove.shorter.series")
  delete.shorter.series <- .get("delete.shorter.series")
  if (remove.shorter.series) {
    cat(
      "3. Remove series shorter than",
      delete.shorter.series,
      "years: Yes\n"
    )
  } else {
    cat(
      "3. Remove series shorter than",
      delete.shorter.series,
      "years: No\n"
    )
  }

  #### 4. First detrend ####
  cat("4. First detrend: ")
  makeFirstDetrending <- .get("makeFirstDetrending")
  save.detrend1 <- .get("save.detrend1")
  method1 <- .get("method1")
  nPerc1 <- .get("nPerc1")
  n1 <- .get("n1")
  p1 <- .get("p1")
  if (makeFirstDetrending) {
    savePlot1Detrending <- ifelse(save.detrend1, "Yes", "No")
    cat(
      GetDetrendMethod(method1, n1, nPerc1, p1),
      "-> Save plot:",
      savePlot1Detrending,
      "\n"
    )
  } else {
    cat("No\n")
  }

  #### 5. Second detrend  ####
  cat("5. Second detrend: ")
  makeSecondDetrending <- .get("makeSecondDetrending")

  if (makeSecondDetrending) {
    save.detrend2 <- .get("save.detrend2")
    method2 <- .get("method2")
    n2 <- .get("n2")
    nPerc2 <- .get("nPerc2")
    p2 <- .get("p2")
    savePlot2Detrending <- ifelse(save.detrend2, "Yes", "No")
    cat(
      GetDetrendMethod(method2, n2, nPerc2, p2),
      "-> Save plot:",
      savePlot2Detrending,
      "\n"
    )
  } else {
    cat("No\n")
  }

  #### 6. Autoregressive model ####
  arMAX <- .get("arMAX")
  makeAr <- .get("makeAr")
  mkAr <- ifelse(makeAr, arMAX, "No")
  cat("6. Autoregressive model:", mkAr, "\n")

  #### 7. EPS analysis ####
  cat("7. EPS analysis: \n")
  run.win.analysis <- .get("run.win.analysis")
  make.common.EPS <- .get("make.common.EPS")
  make.select.period.EPS <- .get("make.select.period.EPS")
  if (any(
    run.win.analysis,
    make.common.EPS,
    make.select.period.EPS
  )) {
    first.year.common <- .get("first.year.common")
    last.year.common <- .get("last.year.common")
    mk.common <- ifelse(make.common.EPS, "Yes", "No")
    cat("    Common interval: ", mk.common, "\n", sep = "")
    span <- paste(first.year.common, last.year.common, sep = "-")
    mk.selected.perid <- ifelse(make.select.period.EPS, span, "No")
    cat("    Defined interval: ", mk.selected.perid, "\n", sep = "")

    if (run.win.analysis) {
      winLength <- .get("winLength")
      stepWin <- .get("stepWin")
      cat(paste0("    Running analysis: ", winLength, "-", stepWin, "\n"))
    }
  } else {
    cat("No\n")
  }
  cat(separator98, sep = "", "\n")
}
