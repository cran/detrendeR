f.apply <- function (FUN = NULL, simplify = TRUE, ...) 
{
    arrFiles <- as.matrix(sort(tk_choose.files()))
    iLast <- dim(arrFiles)[1]
    if (iLast < 1) 
        return(invisible(tk_messageBox(type = "ok", "You should select at least one file!", 
            caption = "Problems")))
    n.files <- sapply(X = arrFiles, FUN = FUN, simplify = simplify, 
        ...)
    if (length(n.files) != iLast) {
        tk_messageBox(type = "ok", "There is some kind of problems!", 
            caption = "Problems")
        return(n.files)
    }
}

