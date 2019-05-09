#' @title LeveneTreatments()
#'
#' @param observations - A numeric vector
#' @param treatments - A vector of factors to group observations
#' @param confidenceInterval - assumed to be the normal 0.05
#'
#' @return Levene Test
#' @return Hypothesis test, check Levene$p.value against confidenceInterval
#'
#'
#' @examples data(CO2)
#'           LeveneTreatments(CO2$uptake, CO2$Plant)
LeveneTreatments <- function(observations, treatments,
                             confidenceInterval = 0.05){

  if (!require("lawstat")) install.packages("lawstat")
    library(lawstat)
  library(SummaryPack)

  observations %<>% as.numeric()
  treatments %<>% as.factor()
  confidenceInterval %<>% CheckConfidence()

  # Checking for Homogeniety of Treatment Variance with Levene Test
  cat("Levene Test for Observations by Treatments:",
      "\n")
  Levene <- observations %>%
    lawstat::levene.test(treatments) %>%
    print()

  cat("\n", "Null Hypothesis: Homoscedasticity of Variance.")
  cat("\n", "Alternative: Heteroscedasticity of Variance. \n")

  Levene$p.value %>%
    HypothesisTest(confidenceInterval)

  return(Levene$p.value)

}
