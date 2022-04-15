commonInterval <- function(rwl) {
  years <- range(as.numeric(rownames(rwl)))
  cat(paste("\nTime span: ", years[1], "-", years[2], ".", sep = ""))
  rwl.common <- common.interval(rwl = rwl, type = "years", make.plot = FALSE)
  if (nrow(rwl.common) > 0) {
    years <- range(as.numeric(rownames(rwl.common)))
    cat("\nCommon interval: ", years[1], "-", years[2], ".\n", sep = "")
  }
  if (!nrow(rwl.common) > 0) {
    cat("NO COMMON INTERVAL!!!!!\n")
  }
}
