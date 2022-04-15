\name{detrendeR-package}
\alias{detrendeR-package}
\alias{detrendeR}
\docType{package}
\title{
detrendeR - A Graphical User Interface to visualize and analyze tree-ring data
}
\description{
A Graphical User Interface to perform some standard tree-ring analyses.
}
\details{
\tabular{ll}{
Package: \tab detrendeR\cr
Type: \tab Package\cr
Version: \tab 1.0.5\cr
License: \tab GPL\cr
}
}
\author{
Filipe Campelo <fcampelo@ci.uc.pt>

}
\references{
Bunn, A. 2008. A dendrochronology program library in R (dplR). Dendrochronologia 26:115-124.

Bunn, A. 2010. Statistical and visual crossdating in R using library. Dendrochronologia 28:251-258.

Cook, E.R., Kairiukstis, L.A. 1990. Methods of Dendrochronology: applications in the environmental sciences. Kluwer Academic Publishers.

Fox, J. 2005. The R Commander: A basic-statistics graphical user interface to R. Journal of Statistical Software 14:142.

Thioulouse, J., Dray, S. 2009. ade4TkGUI: ade4 Tcl/Tk Graphical User Interface. R package version 0.25. http://CRAN.R-project.org/package=ade4TkGUI.
}

\keyword{ package }

\examples{
\dontrun{
#detrender()

data(co021, package="dplR")
years <- as.integer(rownames(co021))
co021.subset <- subset(co021, subset = years >= 1900 & years <= 1950)
## remove the following series "645232","646107" and "646118"
co021.subset<-co021.subset[,!colnames(co021) %in% c("645232", "646107", "646118")]
EPS.value(co021.subset, stc=c(0,8,0))
}
\dontshow{
detrender()
Sys.sleep(1)
tkdestroy(detrendeR:::.get("GUI"))
}
}

