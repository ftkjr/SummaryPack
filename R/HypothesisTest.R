# HypothesisTest(pval, confidenceInterval)
# Frederick T. Kaesmann Jr. 3/22/2019

HypothesisTest <- function(pval, confidenceInterval) {
  # Test Null Hypothesis based on p value and confidence interval
  #
  # Arg: pval: value extracted from various stats test
  #
  # Returns: Statements of rejection or failed rejection of null hypothesis
  #

  # Null Hypothesis test
  if (pval < confidenceInterval) {
    cat("\n", sprintf("t-test pval (%f) < (%s) confidence interval", pval, confidenceInterval))
    cat("\n REJECT Null Hypothesis \n")
  } else if (pval > confidenceInterval) {
    cat("\n", sprintf("t-test pval (%f) > (%s) confidence interval", pval, confidenceInterval))
    cat("\n CANNOT REJECT Null Hypothesis")
  }
}
