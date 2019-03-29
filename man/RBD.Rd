\name{RBD}
\alias{RBD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Randomized Block Design
%%  ~~function to do ... ~~
}
\description{Evaluates provided data through some summary statistics, tests, and plots.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RBD(observations, treatments, block, confidenceInterval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{
%%     ~~Describe \code{observations} here~~
A numeric vector
}
  \item{treatments}{
%%     ~~Describe \code{treatments} here~~
Set of factors which breaks down observations
}
  \item{block}{
%%     ~~Describe \code{block} here~~
Set of factors to be isolated, "Something you can't control and something you usually aren't interested in." - Prof Ray Mugno
}
  \item{confidenceInterval}{
%%     ~~Describe \code{confidenceInterval} here~~
Level of confidence to compare against p values. If unentered, .05 is assumed.
}
  \item{LeveneBlock}{
  Assumed FALSE unless provided as TRUE. If TRUE, runs Levene Test on observations by block.
  }
  \item{plot}{Assumed TRUE, if FALSE doesn't print any of the plots (including Tukey)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Starting from the Top
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{Confidence Interval}{Prints what confidence interval you're using}
\item{Observation Summary}{Prints 5 Number Summary for just the observations vector}
\item{Summary by Treatment}{5 Number Summary for observations by treatments}
\item{Levene Test by Treatment}{Levene Test of observations by treatments}
\item{Levene Test by Block}{If TRUE, runs Levene Test of observations by block. Assumed FALSE}
\item{ANOVA Table}{If Levene Test proves data is not heteroscedastic then runs ANOVA Table}
\item{Tukey Table}{If ANOVA rejects Null Hypothesis then we examine data with a Tukey Table and plots it}
\item{Boxplots}{Plots a boxplot of observations~treatments, as well as a ggboxplot of all three variables.}
}
\references{
%% ~put references to the literature/web site here ~
My github -> https://github.com/ftkjr/SummaryPack
}
\author{
%%  ~~who you are~~
Frederick Kaesmann Jr.
}
\note{
%%  ~~further notes~~
Created for Professor Ray Mugno's MAT 342 Spring 2019
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##

data("CO2")
RBD(CO2$uptake, CO2$Plant, CO2$Type, .04, LeveneBlock = TRUE)

###

data("CO2")
RBD(CO2$uptake, CO2$Plant, CO2$Type)

##
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
