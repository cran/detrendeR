plotRwl <- function(rwl,
                    file.name = "",
                    biweight = TRUE,
                    plot.mean = TRUE,
                    plot.spline = TRUE,
                    plot.y1 = FALSE,
                    band.width = 10,
                    Spar = 0.5,
                    save.csv = FALSE,
                    ...) {
  Year <- as.numeric(rownames(rwl))
  matplot(
    Year,
    rwl,
    ann = FALSE,
    type = "l",
    lty = 1,
    lwd = 1,
    las = 1,
    col = "grey20",
    ...
  )

  if (plot.mean) {
    if (!biweight) {
      mean.rwl <- apply(rwl, 1, mean, na.rm = TRUE)
    } else {
      mean.rwl <- apply(rwl, 1, tbrm, C = 9)
    }

    lines(Year, mean.rwl, col = "red", lwd = 3)
  }
  if (plot.spline) {
    crn.spline <-
      SPLINE(as.vector(mean.rwl), bandwidth = band.width, p = Spar)
    lines(Year, crn.spline, col = "blue", lwd = 2)
  }

  if (plot.y1) {
    abline(h = 1)
  }

  std.dev <- apply(rwl, 1, sd, na.rm = T)
  if (save.csv) {
    samp.depth <- apply(rwl, 1, function(y) {
      sum(!is.na(y))
    })
    crn.raw <-
      data.frame(Year, mean.rwl, crn.spline, std.dev, samp.depth)
    write.table(
      round(crn.raw, 3),
      paste(file.name, "RAW.csv", sep = ""),
      quote = FALSE,
      sep = ";",
      row.names = FALSE
    )
  }
}
