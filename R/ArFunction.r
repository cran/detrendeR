ArFunction = function (rwl, order.max = 10) 
{
    ar.fun = function(rw, order.max = 10) {
        out = c(rep(NA, order.max + 1))
        y2 = na.omit(rw)
        ar1 = ar(y2, order.max = order.max)
        out[1] = ar1$order
        if (out[1] > 0) 
            for (i in 1:out[1]) out[i + 1] = ar1$ar[i]
        out
    }
    out = t(apply(rwl, 2, ar.fun, order.max = order.max))
    out <- round(out, 3)
    out[, 1] <- round(out[, 1], 0)
    colnames(out) <- c("AR", paste("   t", 1:order.max, sep = ""))
    rownames(out) <- format(rownames(out), width = 9)
    cat(rep("=", (21 + order.max * 8)), "\n\nAUTOREGRESSIVE MODELING\n", 
        rep("=", (21 + order.max * 8)), "\n", sep = "")
    WriteMatrix(out, ID.name = "Seq", row.name = "Series", col.width = 7)
    cat(rep("=", (21 + order.max * 8)), "\n", sep = "")
}

