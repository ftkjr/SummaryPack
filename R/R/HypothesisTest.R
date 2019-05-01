# HypothesisTest()
# Frederick T. Kaesmann Jr. 3/22/2019

HypothesisTest <- function(pvalue, confidenceInterval) {
  # Test Null Hypothesis based on p value and confidence interval
  #
  # Arg: pval: value extracted from various stats test
  #
  # Returns: Statements of rejection or failed rejection of null hypothesis
  #


  # If nothing entered, assume the usual 0.05
  if (missing(confidenceInterval)) {
    confidenceInterval <- 0.05
  }

  # Null Hypothesis test
  if (pvalue < confidenceInterval) {
    # If p-value is less than the confidence interval,
    # reject the null hypothesis in favor of alternative
    cat("\n",
        sprintf("Test p-value (%f) < (%s) confidence interval",
                pvalue, confidenceInterval))
    cat("\n REJECT Null Hypothesis in favor of Alternative \n")

  } else if (pvalue > confidenceInterval) {
    # If p-value is greater than the confidence interval
    # we cannot reject the null hypothesis in favor of the alternative
    cat("\n",
        sprintf("Test p-value (%f) > (%s) confidence interval",
                pvalue, confidenceInterval))
    cat("\n CANNOT REJECT Null Hypothesis")
  }
}
