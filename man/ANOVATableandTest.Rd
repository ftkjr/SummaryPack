\name{ANOVATableandTest}
\alias{ANOVATableandTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Print ANOVA tables and Tukey Table, test Hypotheses
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
ANOVA or oneway.test based on Homoscedasticity or Heteroscedasticity of observations by treatments. Also ANOVA table for observations by treatments and block. Prints Tukey Tables where applicable.
}
\usage{
ANOVATableandTest(someObservations, aTreatmentVector, blockingGroups)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{
%%     ~~Describe \code{x} here~~
A Numeric Vector}
\item{treatments}{A Vector of Factors to Group observations}
\item{block}{A Vector of Factors, Optional Argument}
\item{confidenceInterval}{Assumed to be 0.05 unless otherwise specified}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Designed for use in RBD()
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{ANOVA Table}{If block doesn't exist and the P-value from Levene's Test for observations by treatments is not significant, returns one way ANOVA}
\item{ANOVA P-value}{Returns P-value from ANOVA table for use in other functions.}
\item{Oneway.test Table}{If block doesn't exist, and the P-value from Levene's Test for observations by treatments is significant, returns oneway.test with var.equal = FALSE}
\item{ANOVA Table}{If block exists, runs multifactor ANOVA}
\item{Tukey Table}{If applicable for one or multifactor ANOVA, prints and plots Tukey Table}
}
\references{
%% ~put references to the literature/web site here ~
My Github -> https://github.com/ftkjr
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
# No block Arg
data(CO2)
ANOVATable(CO2$uptake, CO2$Plant)

########

# With block
data(CO2)
ANOVATable(CO2$uptake, CO2$Plant, CO2$Type)
}
