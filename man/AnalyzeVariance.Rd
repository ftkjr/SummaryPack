\name{AnalyzeVariance}
\alias{AnalyzeVariance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Examine variances and means across treatments and block groups
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Runs Levene tests examine Variance
}
\usage{
AnalyzeVariance(observations, treatments, block)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{
%%     ~~Describe \code{x} here~~
Numeric vector of response variables
}
\item{treatments}{Vector of factors to group observations}
\item{block}{An additional vector of factors, optional}
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
\item{Levene Test by Treatments}{
Levene test of variance for observations by treatments}
\item{Levene Test by Treatments P-value}{Returns P-value from Levene Test}
\item{Levene Test by Block}{Levene test of variance for observations by block}
}
\references{
%% ~put references to the literature/web site here ~
My GitHub -> https://github.com/ftkjr/SummaryPack
}
\author{
%%  ~~who you are~~
Frederick T. Kaesmann Jr.
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
#####

data("CO2")
AnalyzeVariance(CO2$uptake, CO2$Plant, CO2$Type)

#####

data("CO2")
AnalyzeVariance(CO2$uptake, CO2$Plant)

#####



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
