getFileName = function (fullpath) 
{
    fullpath = rmExt(fullpath)
    fullpath = basename(fullpath)
    return(fullpath)
}

