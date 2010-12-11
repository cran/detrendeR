ar.func <- function (y, order.max = 10) 
{
    y2 = na.omit(y)
    ar1 = ar(y2, order.max = order.max)
    y2 = ar1$resid + ar1$x.mean
    y[!is.na(y)] = y2
    return(y)
}

