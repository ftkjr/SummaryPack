\name{HypothesisTest}
\alias{HypothesisTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hypothesis Test
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function tests a given p-value by the provided confidence interval.
}
\usage{
HypothesisTest(pval, confidenceInterval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pval}{P-value returned by the various statistical tests}
  \item{confidenceInterval}{Provided confidence interval to test p-value}
%%     ~~Describe \code{x} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
Quick test of acceptance or rejection of null hypothesis.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a statement of whether we reject or accept our null hypothesis.
}
\references{My Github -> https://github.com/ftkjr/SummaryPack
%% ~put references to the literature/web site here ~
}
\author{Frederick Kaesmann Jr.
%%  ~~who you are~~
}
\note{Designed for MAT 342 Spring 2019
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
