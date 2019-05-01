\name{SummaryStats}
\alias{SummaryStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary Stats
%%  ~~function to do ... ~~
}
\description{Function runs some basic multi variable analysis as required for MAT 342 Project 2.
Runs a 5 Number Summary, Mean, Inter Quartile Range, Variance, Standard Deviation, then checks for normality by IQR/SD.
If there are one to two factors, it runs a t-test and plots it with a histogram and boxplot.
If there are more than two factors then it runs a Levene test to check for homoscedasticity (sameness) of variance, if variances are equal across factors, then it runs an ANOVA, where if the means of the factors aren't equal it runs a TukeyHSD table to see which means are different.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SummaryStats(observations, treatments, confidenceInterval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{Your numeric variable, the variable being measured
%%     ~~Describe \code{x} here~~
}
\item{treatments}{Your factors, the variables by which you are separating your observations}
\item{confidenceInterval}{Should be self explanatory. If this argument isn't entered it assumes alpha = .05 or a 95\% interval}
\item{post.hoc}{Accepts either "Wilcoxon" for pairwise Wilcoxon, or "Dunn" for Dunn's test}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a series of basic statistics about your sample. It runs a t-test or Levene -> ANOVA -> TukeyHSD depending on nlevels of treatment.
\item{5 number summary (with mean)}{Min, Quartiles, Median, Mean, and Max}
\item{Inter Quartile Range}{3rd Quartile - 1st Quartile}
\item{Variance}{}
\item{Standard Deviation}{}
\item{IQR/SD}{Check Normality}
\item{t-test}{If nlevels of treatment == 1}
\item{Welch test}{If nlevels of treatment == 2}
\item{Levene test}{If nlevels of treatment > 2}
\item{oneway test}{If Levene Test returns heteroscedasticity of treatments}
\item{Kruskal-Wallis}{If data is nonparametric, runs a Kruskal-Wallis Test}
\item{Pairwise Wilcox test}{If Kruskal-Wallis returns a significant p value, returns Pairwise Wilcox if post.hoc = NULL or "Wilcoxon"}
\item{Dunn's Test}{If Kruskal-Wallis returns a significant p value, returns Dunn's Test if post.hoc = "Dunn"}
\item{ANOVA test}{If applicable by Levene values}
\item{TukeyHSD}{If applicable by ANOVA values}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{My github -> https://github.com/ftkjr/SummaryPack
%% ~put references to the literature/web site here ~
}
\author{Frederick Kaesmann Jr.
%%  ~~who you are~~
}
\note{I hope you like it
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data("CO2")

SummaryStats(CO2$uptake,CO2$Plant)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
