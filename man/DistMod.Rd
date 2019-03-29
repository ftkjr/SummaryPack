\name{DistMod}
\alias{DistMod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distrobution Modifier
%%  ~~function to do ... ~~
}
\description{Examines three variations of the given data:  the original set, the square root of values in the set, and the log of the values in the set to determine which is most normal by IQR/SD. Also plots data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DistMod(observation, treatments)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%%     ~~Describe \code{x} here~~
\item{observation}{Set of numeric values to be evaluated}
  \item{treatments}{Set of factors by which to separate observation}
}
\details{This evaluates either a single vector or a vector separated by provided factors
%%  ~~ If necessary, more details than the description above ~~
}
\value{For less than two factors the function returs three values and an arrow to the most normal (whichever is closest to 1.34), for 2 or more factors, it returns the three values for each set of observations by factor.
\item{Original}{The IQR/SD for the original data}
\item{Square root}{The IQR/SD for the square root of the data}
\item{Log}{The IQR/SD for the log of the data}
\item{Boxplot}{}
\item{Histogram}{}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{My github -> https://github.com/ftkjr/SummaryPack
%% ~put references to the literature/web site here ~
}
\author{Frederick Kaesmann Jr
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
Written for MAT 342 Spring 2019
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## No treatment variables

data("CO2")

DistMod(CO2$uptake)

####

## treatment with multiple factors

HairEyeColor <- as.data.frame(data("HairEyeColor"))

DistMod(HairEyeColor$Freq, HairEyeColor$Eye)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
