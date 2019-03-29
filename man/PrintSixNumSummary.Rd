\name{PrintSixNumSummary}
\alias{PrintSixNumSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Print Six Number Summary and Length for set(s) of variables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Prints sizes and summary stats for observations, which is broken down by treatment and block if applicable
}
\usage{
PrintSixNumSummary(observations, treatments, block)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{
%%     ~~Describe \code{x} here~~
Observation variable
}
\item{treatments}{Factors by which to break down the observations, assumed NULL unless otherwise entered.}
\item{block}{Set of factors to be isolated, "Something you can't control and something you usually aren't interested in." - Prof Ray Mugno \\ Assumed NULL unless otherwise entered}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Inteneded for use in other summary functions.
}
\value{Returns (in order)
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{Observation Size}{Length of observations vector}
\item{Summary of observations}{Six number summary of observation vector}
\item{Treatment Sizes}{Size of Treatment vectors}
\item{Six Number Summary by Treatment}{Six number summary for each treatments vector}
\item{Block Sizes}{Size of Block vectors}
\item{Six Number Summary by Block}{Six number summary for each block vector}
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
Designed for MAT 342 spring 2019.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
####

data("CO2")
PrintSixNumSummary(CO2$uptake)

#########

data("CO2")
PrintSixNumSummary(CO2$uptake, CO2$Plant)

#########

data("CO2")
PrintSixNumSummary(CO2$uptake, CO2$Plant, CO2$Type)

######
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
