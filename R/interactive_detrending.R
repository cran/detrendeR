interactive_detrending <- function(input = NULL,
                                  output = NULL,
                                  TwoSteps = TRUE,
                                  ...) {
  method1 <- .get("method1")
  nPerc1 <- .get("nPerc1")
  n1 <- .get("n1")
  p1 <- .get("p1")

  rwl <- get(input)
  rwl_cv1 <-
    apply(
      rwl,
      2,
      RemoveTrend,
      method = method1,
      BandwidthPerc = nPerc1,
      Bandwidth = n1,
      P = p1
    )

  cat("\nDETRENDING [", input, "]\n\nFirst detrending\n", sep = "")
  if (.get("interactive.detrend")) {
    InteractiveDetrending(
      rwl,
      rwl_cv1,
      method = method1,
      n = n1,
      nPerc = nPerc1,
      p = p1
    )
    if (.get("inter_detrend_changed")) {
      message("changed")
      rwl_cv1 <- .get("inter_detrend_output")
      export2GlobalEnv(paste0(output, ".cv1"), rwl_cv1)
    }
  }
  rwl_in1 <- rwl / rwl_cv1
  export2GlobalEnv(paste0(output, ".in1"), rwl_in1)
  export2GlobalEnv(paste0(output, ".cv1"), rwl_cv1)
  RwlInfo(rwl_in1)
  if (.get("TwoSteps")) {
    method2 <- .get("method2")
    nPerc2 <- .get("nPerc2")
    n2 <- .get("n2")
    p2 <- .get("p2")

    cat("\nSecond detrending\n", sep = "")
    rwl_cv2 <-
      apply(
        rwl_in1,
        2,
        RemoveTrend,
        method = method2,
        BandwidthPerc = nPerc2,
        Bandwidth = n2,
        P = p2
      )
    if (.get("interactive.detrend")) {
      InteractiveDetrending(
        rwl_in1,
        rwl_cv2,
        method = method2,
        n = n2,
        nPerc = nPerc2,
        p = p2
      )
      if (.get("inter_detrend_changed")) {
        message("changed")
        rwl_cv2 <- .get("inter_detrend_output")
      }
    }
    export2GlobalEnv(paste0(output, ".cv2"), rwl_cv2)
    rwl_in2 <- rwl_in1 / rwl_cv2
    export2GlobalEnv(paste0(output, ".in2"), rwl_in2)
    RwlInfo(rwl_in2)
  }
}
