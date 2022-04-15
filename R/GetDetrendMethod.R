GetDetrendMethod <- function(method, n, nPerc, p) {
  switch(
    method,
    "Spline" = {
      return(paste("Spline n=", n, ", p=", p, sep = ""))
    },
    "Spline%" = {
      return(paste("Spline n%=", nPerc, ", p=", p, sep = ""))
    },
    "ModNegExp" = {
      return("Exponential")
    }
  )
  return(method)
}
