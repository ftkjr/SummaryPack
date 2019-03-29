\name{checkConfidence}
\alias{checkConfidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Check for Confidence Interval
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Checks to see if there is a confidence interval entered, if not assumes .05. Checks for validity of interval if entered.
}
\usage{
checkConfidence(confidenceInterval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{confidenceInterval}{The confidence interval, .05 if unentered
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a confidence interval for use in functions
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
My github -> https://github.com/ftkjr/SummaryPack
}
\author{
%%  ~~who you are~~
Frederick Kaesmann Jr
}
\note{
%%  ~~further notes~~
For use in MAT 342 functions where confidenceInterval is an optional argument.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Checking and saving a variable
confidenceInterval <- checkConfidence(confidenceInterval)

## Just Checking
checkConfidence(confidenceInterval)

## For use in a function
library(SummaryPack)

if (missing(confidenceInterval) || !missing(confidenceInterval)){
 confidenceInterval <- checkConfidence(confidenceInterval)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
