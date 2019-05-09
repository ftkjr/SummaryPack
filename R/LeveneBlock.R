#' @title LeveneBlock()
#'
#' @param observations - A numeric vector
#' @param block - A vector of factors to group observations
#' @param confidenceInterval - assumed to be the normal 0.05
#'
#' @return Levene Test
#' @return Hypothesis test, check Levene$p.value against confidenceInterval
#'
#'
#' @examples data(CO2)
#'           LeveneBlock(CO2$uptake, CO2$Plant)
LeveneBlock <- function(observations, block, confidenceInterval = 0.05){
  if (!require("lawstat")) install.packages("lawstat")
  library(lawstat)
  library(SummaryPack)

  observations %<>% as.numeric()
  block %<>% as.factor()
  confidenceInterval %<>% CheckConfidence()

  # Checking for Homogeniety of Block Variance with Levene Test
  cat("Levene Test for Observations by Block:")
  LeveneBlock <- observations %>%
    lawstat::levene.test(block)%>%
    print()

  cat("\n", "Null Hypothesis: Homoscedasticity of Variance.")
  cat("\n", "Alternative: Heteroscedasticity of Variance.\n ")

  LeveneBlock$p.value %>%
    HypothesisTest(confidenceInterval)

}
