# SingleSetSummary(observations)
# Frederick T. Kaesmann Jr.

SingleSetSummary <- function(observations,
                             confidenceInterval = 0.05, plot = TRUE){
  # Prints a basic set of statistic tests
  # for a single set of observations
  #
  # Args:
  #   observations: response variable (numeric vector)
  #
  # Returns:
  #   A series of summary statistics. (Designed for MAT 342


  if (!require("ggpubr")) install.packages("ggpubr")
  library(ggpubr)
  if (!require("magrittr")) install.packages("magrittr")
  library(magrittr)

  observations <- as.numeric(observations)
  sheet <- as.data.frame(observations)

  # Condfidence interval check
    confidenceInterval <- CheckConfidence(confidenceInterval)
  cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, (100-confidenceInterval*100)))

  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  observations %>%
    PrintSixNumSummary() %>% # min, max, mean, sd, etc
    CheckNormality() # Is it normal?

  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # Single Variable t.test
  t.test(observations)
  print(ttest)

  # Null Hypothesis test
  pvalTtest <- ttest$p.value
  HypothesisTest(pvalTtest, confidenceInterval)

  # Plot your data
  if (plot == TRUE){
    par(ask=TRUE)
    print(gghistogram(sheet, "observations", add = "mean", bins = 30,
                    fill = "red", title = "For Internal Use Only"))
    print(ggboxplot(sheet, y = "observations", add = "jitter",
                    color = "red", title = "For Internal Use Only"))
  }
}
