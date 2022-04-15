ar.func <- function(y, order.max = 10) {
  y2 <- na.omit(y)
  ar1 <- ar(y2, order.max = order.max)
  y2 <- ar1$resid + ar1$x.mean
  y[!is.na(y)] <- y2
  return(y)
}

.as.logic <- function(x) {
  return(as.logical(as.integer(x)))
}

.RemoveTrend <- function(rw,
                         method = c("Spline", "Spline%", "ModNegExp", "Mean"),
                         BandwidthPerc = 2 / 3,
                         Bandwidth = 32,
                         P = 1 / 2) {
  switch(method,
    "Spline" = {
      curve <- .SPLINE(rw, bandwidth = Bandwidth, p = P)
      return(curve)
    },
    "Spline%" = {
      curve <- .SPLINE(rw,
        bandwidth = length(rw) * BandwidthPerc,
        p = P
      )
      return(curve)
    },
    "ModNegExp" = {
      curve <- .NegExp(rw)
      return(curve)
    },
    "Mean" = {
      series <- rw[!is.na(rw)]
      curve <- rep(mean(series), length(series))
      rw[!is.na(rw)] <- curve
      return(rw)
    }
  )
}

.SPLINE <- function(rw, bandwidth = 32, p = 0.5) {
  p <- as.numeric(p)
  series <- rw[!is.na(rw)]
  curve <-
    suppressWarnings(ffcsaps(series, nyrs = bandwidth, f = p))
  rw[!is.na(rw)] <- curve
  return(rw)
}

.NegExp <- function(Y, pos.slope = FALSE) {
  y <- Y[!is.na(Y)]
  y <- replace(y, y == 0, 0.001)
  fit.linear.model <- FALSE
  x <- 1:length(y)
  a <- mean(y[1:floor(length(y) * 0.05)])
  b <- -0.01
  k <- mean(y[floor(length(y) * 0.95):length(y)])
  fits <- try(nls(y ~ Const + A * exp(B * x),
    start = list(
      A = a,
      B = b, Const = k
    ),
    trace = FALSE
  ), silent = TRUE)
  if (inherits(x = fits, what = "try-error")) {
    fit.linear.model <- TRUE
  } else {
    fits <- predict(fits)
  }
  if (fits[1] < fits[length(fits)]) {
    fit.linear.model <- TRUE
  }
  if (fits[length(fits)] < 0) {
    fit.linear.model <- TRUE
  }
  if (fit.linear.model) {
    linear.model <- lm(y ~ x)
    fits <- predict(linear.model)
    if (coef(linear.model)[2] > 0 & !pos.slope) {
      fits <- rep(mean(y), length(x))
    }
  }
  Y[!is.na(Y)] <- fits
  return(Y)
}

.assign <- function(name, value) {
  setVariable(paste0("detrendeR_", name), value)
}

.get <- function(name) {
  getVariable(paste0("detrendeR_", name))
}

.optionsDefault <- function() {
  .assign("TempDataBase", "")

  .assign("current_spline_nPerc", 2 / 3)
  .assign("current_spline_n", 30)
  .assign("current_spline_p", 0.5)

  # Tree Mask
  .assign("stc", c(5, 2, 1))

  # Segment Plot
  .assign("makeSegPlot", FALSE)

  # Delete series shorter than
  .assign("remove.shorter.series", TRUE)
  .assign("delete.shorter.series", 100)

  # Correlation with the master
  .assign("lowCorr", 0.256)

  # Interactive Detrending
  .assign("interactive.detrend", TRUE)

  # FIRST DETRENDING
  .assign("makeFirstDetrending", TRUE)
  .assign("method1", "Spline")
  .assign("n1", 36)
  .assign("nPerc1", 2 / 3)
  .assign("p1", 0.5)
  .assign(
    "first_detrending_method",
    GetDetrendMethod("Spline", 36, 2 / 3, 0.5)
  )
  .assign("save.detrend1", TRUE)

  # SECOND DETRENDING
  .assign("makeSecondDetrending", TRUE)
  .assign("method2", "Mean")
  .assign("n2", 60)
  .assign("nPerc2", 2 / 3)
  .assign("p2", 0.5)
  .assign(
    "second_detrending_method",
    GetDetrendMethod("Mean", 60, 2 / 3, 0.5)
  )
  .assign("save.detrend2", TRUE)

  # AR
  .assign("makeAr", TRUE)
  .assign("arMAX", 3)

  # biweightMean
  .assign("biweightMean", TRUE)

  # Run windows analysis
  .assign("run.win.analysis", TRUE)
  .assign("winLength", 50)
  .assign("stepWin", 1)

  # EPS.common.interval
  .assign("make.common.EPS", TRUE)
  .assign("make.select.period.EPS", TRUE)
  .assign("first.year.common", 1900) # 1900)
  .assign("last.year.common", 2000) # 2000)

  # Save graph
  .assign("saveCronoJpg", TRUE)

  # window position
  .assign(".height", 0)
  .assign(".width", 0)
}
