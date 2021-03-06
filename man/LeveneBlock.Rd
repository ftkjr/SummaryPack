\name{LeveneBlock}
\alias{LeveneBlock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Levene Test for Block Vector
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns a Levene Test, a statement of the Null and Alternative Hypotheses for a
Levene Test, then tests the p-value against the confidence interval
}
\usage{
LeveneBlock(someObservationVector, someTreamtentVector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{
%%     ~~Describe \code{x} here~~
A numeric vector
}
\item{block}{A character vector to group the observations by}
\item{confidenceInterval}{Assumed to be the normal 0.05
unless otherwise specified}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
For general use in other functions, not so much as a stand alone.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{Levene Test}{Returns Levene Test Test Statistic and p-value}
}
\references{
%% ~put references to the literature/web site here ~
My Github ->  https://github.com/ftkjr
}
\author{
%%  ~~who you are~~
Frederick T. Kaesmann Jr.
}
\note{
%%  ~~further notes~~
Designed for Professor Ray Mugno's MAT 342 class, Spring 2019
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data(CO2)

LeveneTest(CO2$uptake, CO2$Plant)

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
