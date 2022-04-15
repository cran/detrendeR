SaveDetrendJPG <- function(rwl,
                           detrend,
                           folderName = "Detrend",
                           work.dir = NULL,
                           detrend.method = "",
                           select.series = 1:(ncol(rwl))) {
  if (length(detrend.method) == 1) {
    detrend.method <- rep(detrend.method, ncol(rwl))
  }
  if (is.null(work.dir)) {
    work.dir <- getwd()
  }
  dir.create(folderName, showWarnings = FALSE)
  setwd(folderName)
  seriesnames <- colnames(rwl)
  yr.vec <- as.numeric(rownames(rwl))
  for (i in select.series) {
    jpeg(
      paste(seriesnames[i], ".jpg", sep = ""),
      width = 1200,
      height = 600,
      quality = 100
    )
    plot(
      yr.vec,
      rwl[, i],
      type = "l",
      xlab = "Years",
      ylab = "",
      main = seriesnames[i],
      las = 1,
      col = "blue"
    )
    mtext(
      paste(detrend.method[i], sep = ""),
      line = 0.5,
      side = 3,
      adj = 1,
      cex = 0.9,
      col = "blue",
      font = 1
    )
    mtext(
      "Detrender",
      line = 0.5,
      side = 3,
      adj = 0,
      cex = 0.9,
      col = "blue",
      font = 1
    )
    lines(yr.vec, detrend[, i], col = 2)
    dev.off()
  }
  setwd(work.dir)
}

ARSTAN <- function() {
  n.sink <- sink.number()
  while (n.sink > 0) {
    sink()
    n.sink <- n.sink - 1
  }
  f.apply(FUN = arstan)
}


arstan <- function(filename = NULL) {
  if (is.null(filename)) {
    filename <- tk_choose.files(multi = FALSE)
    if (length(filename) != 1) {
      return(
        tk_messageBox(
          type = "ok",
          "You should select at least one file!",
          caption = "Problems"
        )
      )
    }
  }

  file.name <- getFileName(filename)
  folder.work <- substr(filename, 1, nchar(filename) - nchar(basename(filename)))
  Output <- paste(folder.work, sep = "\\")
  outputFolder <- paste(file.name, sep = "\\")
  output <- paste(file.name, "out", sep = ".")
  setwd(folder.work)
  dir.create(file.name, showWarnings = FALSE)
  setwd(file.name)
  sink(output, append = FALSE)
  on.exit(sink())
  stc <- .get("stc")

  detrendeRoptions(Stc = stc)

  cat("Input: ",
    filename,
    "\nOutput: ",
    Output,
    outputFolder,
    "/",
    output,
    "\n",
    sep = ""
  )

  rwl <- readRwl(filename, n.header = NULL, info = FALSE) # read the file [i]

  # Give the number of trees and the no of cores per tree
  TreeIds(rwl, stc = stc)
  TrwLessThan(rwl, TRW = 0) # MISSING RINGS
  RwlInfo(rwl, print = TRUE) # Rwl information


  if (.get("makeSegPlot")) {
    # Make graph with series length
    jpeg(
      paste(file.name, "SegPlot.jpg", sep = "-"),
      width = 700,
      height = 700,
      quality = 85
    )
    op <- par(oma = c(0, 0, .5, 0))
    seg.plot(rwl)
    title(
      main = file.name,
      outer = TRUE,
      line = -0.5
    )
    par(op)
    dev.off()
  }

  # Delete series shorter than
  if (.get("remove.shorter.series")) {
    rwl <- DeleteSeriesShorterThan(rwl,
      filename = file.name,
      YEAR = .get("delete.shorter.series")
    )
  }

  # Plot RAW RWL
  jpeg(
    paste(file.name, "RAW.jpg", sep = "-"),
    width = 900,
    height = 700,
    quality = 85
  )
  plotRwl(rwl, file.name = file.name, save.csv = TRUE)
  dev.off()

  # DETRENDING  #########
  ## first detrending ####
  if (.get("makeFirstDetrending")) {
    first_detrending_method <- .get("first_detrending_method")
    interactive.detrend <- .get("interactive.detrend")
    save.detrend1 <- .get("save.detrend1")
    method1 <- .get("method1")
    n1 <- .get("n1")
    nPerc1 <- .get("nPerc1")
    p1 <- .get("p1")

    cat("\nDETRENDING\n\nFirst detrending [",
      first_detrending_method,
      "]\n",
      sep = ""
    )

    detrend1 <- apply(
      rwl,
      2,
      RemoveTrend,
      method = method1,
      BandwidthPerc = nPerc1,
      Bandwidth = n1,
      P = p1
    )
    #  if (save.detrend1)
    #      saveDetrendJPG(rwl, detrend1, folderName = "FirstDetrend",
    #         detrend.method = first_detrending_method)
    if (interactive.detrend) {
      InteractiveDetrending(
        rwl,
        detrend1,
        folderName = "FirstDetrend",
        method = method1,
        n = n1,
        nPerc = nPerc1,
        p = p1
      )

      if (.get("inter_detrend_changed")) {
        detrend1 <- .get("inter_detrend_output")

        first_detrending_method <-
          as.vector(.get("inter_detrend_output_changes")[-1, 3])
        first_detrending_method
      }
    }

    if (save.detrend1) {
      SaveDetrendJPG(rwl,
        detrend1,
        folderName = "FirstDetrend",
        detrend.method = first_detrending_method
      )
    }
    if (min(detrend1, na.rm = TRUE) < 0) {
      cat("\n")
      TrwLessThan(detrend1, TRW = 0.01)
    }
    rw1 <- rwl / detrend1
    write.rwl(
      as.data.frame(detrend1),
      fname = paste(file.name, "cv1", sep = "."),
      long.names = TRUE
    )
    write.rwl(rw1,
      fname = paste(file.name, "in1", sep = "."),
      long.names = TRUE
    )
    RwlInfo(rw1, print = TRUE)

    if (.get("makeSecondDetrending")) {
      ## second detrending ####
      second_detrending_method <- .get("second_detrending_method")
      save.detrend2 <- .get("save.detrend2")
      method2 <- .get("method2")
      n2 <- .get("n2")
      nPerc2 <- .get("nPerc2")
      p2 <- .get("p2")

      cat("\nSecond detrending [",
        second_detrending_method,
        "]\n",
        sep = ""
      )

      detrend2 <- apply(
        rw1,
        2,
        RemoveTrend,
        method = method2,
        BandwidthPerc = nPerc2,
        Bandwidth = n2,
        P = p2
      )
      #  if (save.detrend2)
      #      saveDetrendJPG(rw1, detrend2, folderName = "SecondDetrend",
      #        detrend.method = second_detrending_method)
      if (interactive.detrend) {
        InteractiveDetrending(
          rw1,
          detrend2,
          folderName = "SecondDetrend",
          method = method2,
          n = n2,
          nPerc = nPerc2,
          p = p1
        )
        if (.get("inter_detrend_changed")) {
          detrend2 <- .get("inter_detrend_output")
          second_detrending_method <- as.vector(.get("inter_detrend_output_changes")[-1, 3])
        }
      }
      if (save.detrend2) {
        SaveDetrendJPG(rw1,
          detrend2,
          folderName = "SecondDetrend",
          detrend.method = second_detrending_method
        )
      }
      if (min(detrend2, na.rm = TRUE) < 0) {
        cat("\n")
        TrwLessThan(detrend2, TRW = 0.01)
      }
      rw2 <- rw1 / detrend2
      write.rwl(
        as.data.frame(detrend2),
        fname = paste(file.name, "cv2",
          sep = "."
        ),
        long.names = TRUE
      )
      write.rwl(rw2,
        fname = paste(file.name, "in2", sep = "."),
        long.names = TRUE
      )
      RwlInfo(rw2, print = TRUE)
    } else {
      rw2 <- rw1
    }
  } else {
    rw2 <- rwl
  }

  #### Save std chronology ####
  crnStd <- mk_chrono(rw2,
    prefix = paste(file.name, "STD", sep = "-"),
    biweight = .get("biweightMean"),
    prewhiten = FALSE,
    stc = stc
  )

  PrintPlotChrono(crnStd,
    rwl = rw2,
    file.name = file.name,
    crono.type = "STD"
  )


  ## STD - EPS #####
  run.win.analysis <- .get("run.win.analysis")
  make.common.EPS <- .get("make.common.EPS")
  make.select.period.EPS <- .get("make.select.period.EPS")
  winLength <- .get("winLength")
  stepWin <- .get("stepWin")

  ##### window analysis ####
  if (run.win.analysis) {
    cat("\nRunning analysis of detrended series\n\n")
    runWinRw2 <- Run.Win(rw2,
      winLength = winLength,
      stc = stc,
      step = stepWin
    )
    for (i in c(0.75, 0.80, 0.85, 0.90)) {
      EPS.resume(runWinRw2, EPS = i)
    }
  }

  ##### common period ####
  if (make.common.EPS) {
    rw2_common <- common.interval(rw2, "years", FALSE)
    EPS.common.interval(rw2_common, stc = stc, out = FALSE)
  }
  ##### selected period ####
  if (make.select.period.EPS) {
    if (!(is.null(first.year.common) || is.null(last.year.common))) {
      EPS.common.interval(
        rw2,
        first.year.common = first.year.common,
        last.year.common = last.year.common,
        stc = stc,
        out = FALSE
      )
    }
  }

  # AR ######
  if (.get("makeAr")) {
    arMAX <- .get("arMAX")
    ArFunction(rw2, order.max = arMAX) # AUTOREGRESSIVE MODELING
    res <- apply(rw2, 2, ar.func, order.max = arMAX)
    res <- as.rwl(res)
    crnRes <- mk_chrono(
      res,
      prefix = paste(file.name, "RES", sep = "-"),
      biweight = .get("biweightMean"),
      prewhiten = FALSE,
      stc = stc
    )
    PrintPlotChrono(crnRes,
      rwl = res,
      file.name = file.name,
      crono.type = "RES"
    )
    cat("\nResidual series\n")
    RwlInfo(res, print = TRUE)
    write.rwl(res,
      fname = paste(file.name, "res", sep = "."),
      long.names = TRUE
    )


    ## RES - EPS #####
    ##### window analysis ####
    if (run.win.analysis) {
      # make.common.EPS <- .get("make.common.EPS")
      # make.select.period.EPS <- .get("make.select.period.EPS")
      # winLength <- .get("winLength")
      # stepWin <- .get("stepWin")

      cat("\nRunning analysis of residual series\n\n")
      runWinRes <- Run.Win(res,
        winLength = winLength,
        stc = stc,
        step = stepWin
      )
      for (i in c(0.75, 0.80, 0.85, 0.90)) {
        EPS.resume(runWinRes, EPS = i)
      }
    }
    ##### common period ####
    if (make.common.EPS) {
      res_common <- common.interval(res, "years", FALSE)
      EPS.common.interval(res_common, stc = stc, out = FALSE)
    }
    ##### selected period ####
    if (make.select.period.EPS) {
      first.year.common <- .get("first.year.common")
      last.year.common <- .get("last.year.common")
      if (!(is.null(first.year.common) || is.null(last.year.common))) {
        EPS.common.interval(
          res,
          first.year.common = first.year.common,
          last.year.common = last.year.common,
          stc = stc,
          out = FALSE
        )
      }
    }
  }
  cat("[", date(), "]", sep = "")
}
