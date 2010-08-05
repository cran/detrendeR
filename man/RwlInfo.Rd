\name{RwlInfo}
\alias{RwlInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes some common statistics on individual ring-width series. 
}
\description{
This function computes some common descriptive statistics on individual series, such as the correlation with the master chronology, mean, median, standard deviation, mean sensitivity and first-order autocorrelation, and the output can be printed to the R editor window
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RwlInfo(rwl, print=TRUE, chrono=NULL)
}
\arguments{
  \item{rwl }{a data.frame with ring-width series as columns and years as rows.}
  \item{print }{logical flag. If TRUE the output is printed in the console.}
  \item{chrono }{If NULL the function computes a mean chronology using all series presented in the data.frame (rwl).}
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{print }{Description of 'comp1'}
%%  \item{chrono }{Description of 'comp2'}
%% ...
A data.frame with the descriptive statistics for each series.

}
\references{
%% ~put references to the literature/web site here ~
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

data(co021, package="dplR")
RwlInfo(co021)
RwlInfo(co021, print=FALSE)->co021.info
write(co021.info)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
