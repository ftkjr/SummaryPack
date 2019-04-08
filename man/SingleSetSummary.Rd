\name{SingleSetSummary}
\alias{SingleSetSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Run a set of summary statistics on a single set of observations
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SingleSetSummary(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{
%%     ~~Describe \code{x} here~~
A numeric vector of observations or responses
}
\item{confidenceInterval}{}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{SixNumberSummary}{Min, Q1, Median, Mean, Q3, Max}
\item{CheckNormality}{IQR, Standard Deviation, IQR/SD}
\item{t.test}{Single variable t-test}
\item{HypothesisTest}{}
\item{plots}{Histogram and Boxplot}
}
\references{
%% ~put references to the literature/web site here ~
My Github -> https://github.com/ftkjr/SummaryPack
}
\author{
%%  ~~who you are~~
Frederick T. Kaesmann Jr.
}
\note{
%%  ~~further notes~~
Designed for Ray Mugno's MAT 342 Spring 2019
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
