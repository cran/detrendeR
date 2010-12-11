TreeIds =  
function (rwl, stc = c(5, 2, 1)) 
{
    treeMask <- readTreeMask(rwl, stc)
    commonInterval(rwl)
    cat(rep("=", 53), "\n", sep = "")
    flag = FALSE
    if (length(unique(treeMask[, 1])) > 1) 
        flag = TRUE
    WriteMatrix(treeMask, ID = T, ID.name = "Seq", row.names = T, 
        row.name = "Series   ", col.width = 7, sep = "|", na = "")
    if (flag) {
        cat(rep("=", 53), "\nWARNING:\nThere appears to be more than one site!!!\n", 
            rep("=", 53), sep = "")
    }
    else {
        cat(rep("=", 53), "\n", sep = "")
    }
}

