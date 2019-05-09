#' @title RBD()
#' @author Frederick T. Kaesmann Jr.  3/20/19
#'
#' @param observations A numeric vector
#' @param treatments A vector of factors to group observations
#' @param block "Something you can't control and
#                  something you usually aren't interested in."
#                                             - Prof Ray Mugno
#' @param confidenceInterval Assumed to be 0.05 unless otherwise specified
#' @param plot If true returns the associated graphs from SummaryPlots()
#'
#' @return 6 number summary for the observations vector
#' @return 6 number summary for observations by treatments
#' @return 6 number summary for observations by block
#' @return IQR/SD for observations by treatments and observations by block
#' @return Levene test for observations by treatments and observations by block
#' @return ANOVA Tables from ANOVATableandTest()
#' @return Histograms and Boxplots from SummaryPlots()
#'
#'
#'
#' @examples RBD(someObservations, aTreatmentVector, blockingVector)
RBD <- function(observations, treatments, block,
                confidenceInterval = 0.05,
                plot = TRUE){

  # Add requisite libraries, install if necessary
  if (!require("lawstat")) install.packages("lawstat")
  library(lawstat)
  if (!require("MASS")) install.packages("MASS")
  library(MASS)
  if(!require("ggpubr")) install.packages("ggpubr")
  library(ggpubr)
  if (!require("tibble")) install.packages("tibble")
  library(tibble)
  library(SummaryPack)

  # Checking inputs
  observations %<>% as.numeric()
  treatments %<>% as.factor()
  block %<>% as.factor()

  cat("%%%%%%%%%%%%%%% Begin Randomized BLock Design Test %%%%%%%%%%%%%%%%%%%",
      "\n \n")


  # Condfidence interval check
  confidenceInterval %<>% CheckConfidence()

  cat("Your confidence interval is:",
      "\n",
      sprintf("%s, (%s percent)",
              confidenceInterval, (100-confidenceInterval*100)))
  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # Basic Number Summary
  PrintSixNumSummary(observations, treatments, block)

  # How normal is observations by treatment
  cat("\n Observations by Treatments \n")
  CheckNormality(observations, treatments)
  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # Check Normality of observations by block
  cat("Observations by Block \n")
  CheckNormality(observations, block)
  cat("\n",
      "-------------------------------------------------------------------",
        "\n")

  # Levene Test observations by treatments and observations by block
  AnalyzeVariance(observations, treatments, block,
                  confidenceInterval = confidenceInterval)

  # ANOVA Table
  ANOVATableandTest(observations, treatments, block, confidenceInterval)

  if (plot == TRUE){
    # Plot it
   SummaryPlots(observations, treatments, block)
  }
}#close function
# Fin
