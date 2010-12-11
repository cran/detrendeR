EPS.common.interval = function (rwl, first.year.common = NULL, last.year.common = NULL, 
    stc = c(5, 2, 1), out = TRUE) 
{
    win.stats = "No common interval"
    HEADER_STRING = "\nCommon interval analysis:\n"
    {
        if (is.null(first.year.common) || is.null(last.year.common)) {
            win <- na.omit(rwl)
        }
        else {
            HEADER_STRING = "\nInterval analysis:\n"
            winLength = last.year.common - first.year.common + 
                1
            if (length(winLength) != 0) 
                win <- RunWindow(rwl, start = first.year.common, 
                  winLength = winLength)
        }
    }
    {
        if (nrow(win) > 1) {
            win.stats <- EPS.value(win, stc = stc)
            colnames(win.stats) <- c("start", "end", "tree", 
                "core", "n.tot", "n.wt", "n.bt", " r.tot", " r.wt", 
                "  r.bt", " c.eff", " r.eff", "   eps")
            cat(rep("=", 94), HEADER_STRING, rep("=", 94), "\n", 
                sep = "")
            WriteMatrix(win.stats, ID = T, ID.name = "Seq", row.names = F, 
                na = "-")
        }
        else {
            cat(rep("=", 94), "\n", sep = "")
            cat("No common interval.\n")
        }
    }
    win.stats = as.matrix(win.stats)
    cat(rep("=", 94), "\n", sep = "")
    if (out) 
        return(as.vector(win.stats[1, ]))
}

