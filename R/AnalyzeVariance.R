
#' @title AnalyzeVariance()
#' @author Frederick T. Kaesmann Jr.
#'
#' @param observations Numeric Vector of Response Variables
#' @param treatments Vector of Factors to Group observations
#' @param block Additional Vector of Factors
#' @param confidenceInterval Assumed to be 0.05 unless otherwise specified
#'
#' @return Levene Test for observations by treatments
#' @return Levene Test for treatments P-value
#' @return Levene Test for observations by block
#' @export
#'
#' @examples AnalyzeVariance(someObservations, someTreatments, aBlock)
AnalyzeVariance <- function(observations, treatments,
                            block = NULL,
                            confidenceInterval = 0.05){

  if (!require("lawstat")) install.packages("lawstat")
  library(lawstat)
  if (!require("MASS")) install.packages("MASS")
  library(MASS)
  if (!require("dunn.test")) install.packages("dunn.test")
  library(dunn.test)
  if (!require("s20x")) install.packages("s20x")
  library(s20x)
  if (!require("magrittr")) install.packages("magrittr")
  library(magrittr)
  library(SummaryPack)

  observations %<>% as.numeric()
  treatments %<>% as.factor()
  confidenceInterval %<>% CheckConfidence()

  cat("\n \n %%%%%%%%%%%% Variance Analysis %%%%%%%%%%%% \n \n")

  # Checking for Homogeniety of Treatment Variance with Levene Test
  pvalueLevene <- LeveneTreatments(observations, treatments, confidenceInterval)
  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # Checking for Homogeniety of Block Variance with Levene Test
  if (!is.null(block)){
    LeveneBlock(observations, block, confidenceInterval)
    cat("\n",
        "-------------------------------------------------------------------",
        "\n \n")
  }
}
