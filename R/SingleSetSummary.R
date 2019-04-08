# SingleSetSummary(observations)
# Frederick T. Kaesmann Jr.

SingleSetSummary <- function(observations, confidenceInterval, plot){
  # Prints a basic set of statistic tests
  # for a single set of observations
  #
  # Args:
  #   observations: response variable (numeric)
  #
  # Returns:
  #   A series of summary statistics. (Designed for MAT 342


  library(SummaryPack)
  library(lawstat)
  library(MASS)
  library(ggpubr)

  observations <- as.numeric(observations)
  sheet <- as.data.frame(observations)

  # Condfidence interval check
  if (missing(confidenceInterval) || !missing(confidenceInterval)){
    confidenceInterval <- CheckConfidence(confidenceInterval)
  }
  cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, (100-confidenceInterval*100)))

  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # min, max, mean, sd, etc
  PrintSixNumSummary(observations)

  # Is it normal?
  CheckNormality(observations)

  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # Single Variable t.test
  ttest <- t.test(observations)
  pvalTtest <- ttest$p.value
  print(ttest)

  # Null Hypothesis test
  HypothesisTest(pvalTtest, confidenceInterval)

  # Plot your data
  if (missing(plot) || plot != FALSE){
  par(ask=TRUE)
  print(gghistogram(sheet, "observations", add = "mean", bins = 30,
                    title = "For Internal Use Only"))
  print(ggboxplot(sheet, y = "observations", add = "jitter",
                  title = "For Internal Use Only"))
  }
}
