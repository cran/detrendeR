mk_chrono <- function(x,
                  prefix = "CRONO",
                  biweight = TRUE,
                  prewhiten = FALSE,
                  stc = c(5, 2, 1),
                  order.max.prewhiten = 3) {
  prefix <- as.character(prefix)

  if (order.max.prewhiten < 1) {
    prewhiten <- FALSE
  }

  SAMPLE_DEPTH <- sampleDepth(x, stc = stc)

  if (!biweight) {
    std <- apply(x, 1, mean, na.rm = TRUE)
  } else {
    std <- apply(x, 1, tbrm, C = 9)
  }


  out <- data.frame(std, SAMPLE_DEPTH)
  colnames(out) <- c(prefix, "NoTrees", "NoCores")

  if (prewhiten) {
    PREFIX <- paste(prefix, "prewhiten", sep = "-")


    x.ar <- apply(x, 2, ar.func, order.max = order.max.prewhiten)

    if (!biweight) {
      res <- apply(x.ar, 1, mean, na.rm = TRUE)
    } else {
      res <- apply(x.ar, 1, tbrm, C = 9)
    }
    res[is.nan(res)] <- NA
    out <- data.frame(std, res, SAMPLE_DEPTH)
    colnames(out) <- c(prefix, PREFIX, "NoTrees", "NoCores")
  }
  return(out)
}
