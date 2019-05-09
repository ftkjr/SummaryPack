# SingleSetSummary(observations)
# Frederick T. Kaesmann Jr.

SingleSetSummary <- function(observations,
                             confidenceInterval = 0.05,
                             plot = TRUE){
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
  if (!require("tibble")) install.packages("tibble")
  library(tibble)

  observations %<>% as.numeric()
  sheet <- tibble(observations)

  # Condfidence interval check
  confidenceInterval %<>% CheckConfidence()

  cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, (100-confidenceInterval*100)))

  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # Min, Q1, Median, Mean, Q3, Max
  observations %>% PrintSixNumSummary()

  # Normal Data?
  observations %>%  CheckNormality()

  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # Single Variable t.test
  ttest <- t.test(observations)
  ttest %>% print()

  # Null Hypothesis test
  ttest$p.value %>%  HypothesisTest(confidenceInterval)

  # Plot your data
  if (plot == TRUE){
    par(ask=TRUE)
    SummaryPlots(observations)
  }
}
