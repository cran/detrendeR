\name{EPS.value}
\alias{EPS.value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes general statistics on ring-width series. 
}
\description{
This function computes general statistics on a data.frame of ring-width series. This function is a changed version of the Andrew Bunn's rwi.stats function from the dplR package (Bunn, 2008)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EPS.value(rwl, stc = c(5,2,1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rwl }{a data.frame of ring widths  }
  \item{stc }{a vector of three integers giving the Site-Tree-Core mask. Defaults to c(5,2,1) and these three values should sum to eigth. The stc[1] gives the number of characters used to define the site code, stc[2] defines the tree mask and the core ids is given by the last element of the vector stc[3]. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{print }{Description of 'comp1'}
%%  \item{chrono }{Description of 'comp2'}
%% ...

A data.frame with the following values: 'start', 'end', 'tree', 'core', 'n.tot', 'n.wt', 'n.bt','r.tot','r.wt', 'r.bt', 'c.eff', 'r.eff', and 'eps'.

\item{start }{the first year.}
\item{end }{the last year.}
\item{tree }{the number of trees.}
\item{core }{the number of cores.}
\item{n.tot }{the number of unique combinations of the input series (i.e., 1/2*n*[n-1]).}
\item{n.wt }{the total number of unique combinations of the within-tree series.}
\item{n.bt }{the total number of unique combinations of the between-tree series.}
\item{r.tot }{the mean of all correlation between different
     cores.}
\item{r.wt }{the mean of correlations between series from the same tree over all trees.}
\item{r.bt }{the mean interseries correlation between all series from different trees.}
\item{c.eff }{the effective number of cores.}
\item{r.eff }{the effective signal calculated as [rbar.bt / (rbar.wt + (1-rbar.wt) / c.eff)].}
\item{r.eff }{the expressed population signal.}
}

\references{
Bunn, A. 2008. A dendrochronology program library in R (dplR). Dendrochronologia 26:115-124.

Cook, E.R., Kairiukstis, L.A. 1990. Methods of Dendrochronology: applications in the environmental sciences.  Kluwer Academic Publishers.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data(co021)

years<-as.integer(rownames(co021))
co021.subset<-(subset(co021, subset=years>=1900 & years<=1950))
co021.subset<-co021.subset[,-c(28,30,31)] ## to remove the following series "645232","646107" and "646118"

EPS.value(co021.subset, stc=c(0,8,0))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
