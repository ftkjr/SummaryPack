\name{CheckNormality}
\alias{CheckNormality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check Normality of set(s) of observations
%%  ~~function to do ... ~~
}
\description{Checks normaility of a set of data which may or may not be broken out by treatments.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CheckNormality(observation, treatment)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%%     ~~Describe \code{x} here~~
  \item{observation}{vector of numeric values}
  \item{treatment}{optional value providing factors by which to separate observation values}
  \item{plots}{IF true prints associated plots, assumed FALSE}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Prints a series of summary statistics to quickly check how normal the data is for either a single set of observations or for observations broken down by factors.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{IQR}{Inter Quartile Range}
\item{Stand Dev}{Standard Deviation}
\item{IQR/Sigma}{Provides numeric value to check normality}
\item{Histogram}{}
\item{Boxplot}{}
}
  \references{My github -> https://github.com/ftkjr/SummaryPack
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Frederick T. Kaesmann Jr
}
\note{
%%  ~~further notes~~
Designed for MAT 342 Spring 2019
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{SummaryStats.Rd, DistMod.Rd
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## Single or no factor
data("CO2")

CheckNormality(CO2$uptake)

#####

## Multiple treatment factors
data("CO2")

CheckNormality(CO2$uptake,CO2$Plant)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
