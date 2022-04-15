NegExp <- function(x, pos.slope = FALSE) {
  y <- x[!is.na(out <- x)]
  y[y == 0] <- 0.001

  fit.linear.model <- FALSE

  x <- 1:length(y)
  a <- mean(y[1:floor(length(y) * 0.05)])
  b <- -0.01
  k <- mean(y[floor(length(y) * 0.95):length(y)])

  fits <- try(nls(y ~ Const + A * exp(B * x),
    start = list(A = a, B = b, Const = k),
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
  out[!is.na(out)] <- fits
  return(out)
}
