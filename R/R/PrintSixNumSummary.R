# PrintSixNumSummary(observations, treatments)
# Fred Kaesmann Jr  3/25/2019

#' PrintSixNumSummary
#'
#' @param observations - a numeric vector of observations
#' @param treatments - a set of factors to group observations
#' @param block - additional grouping factors
#'
#' @return - Number of observations in the whole set,
#'  Six Number Summary of all observations (min, 1st Q, median, mean, 3rd Q, max)
#'           Number of observations per treatment
#'           Six Number Summary of observations by treatments
#'           Number of observations per block
#'           Six Number Summary of observations by block
#' @export
#'
#' @examples
#'           PrintSixNumSummary(SomeNumericVector, aTreatmentVector, aBlockingVector)
#'
#'
PrintSixNumSummary <- function(observations, treatments = NULL, block = NULL){

  SampleSize <- length(observations)
  ObservationsSummary <- summary(observations)
  cat("Observations size:", SampleSize, "\n \n")
  cat("\n",
      "-------------------------------------------------------------------", "\n")
  cat("\n", "Six Number Summary of Observations:", "\n \n")
  print(ObservationsSummary)
  cat("\n",
      "-------------------------------------------------------------------", "\n")

  if (!is.null(treatments)){
    SizeofTreatments <- tapply(observations, treatments, length)
    ObservationsbyTreatments <- tapply(observations, treatments, summary)
    cat("\n", "Treatment Sizes:", "\n \n")
    print(SizeofTreatments)
    cat("\n",
        "-------------------------------------------------------------------", "\n")
    cat("\n", "Six Number Summary by Treatment:", "\n \n")
    print(ObservationsbyTreatments)
    cat("\n",
        "-------------------------------------------------------------------", "\n")
  }

  if (!is.null(block)){
    SizeofBlock <- tapply(observations, block, length)
    ObservationsbyBlock <- tapply(observations, block, summary)
    cat("\n", "Block Sizes:", "\n \n")
    print(SizeofBlock)
    cat("\n",
        "-------------------------------------------------------------------", "\n")
    cat("\n", "Six Number Summary by Block:", "\n \n")
    print(ObservationsbyBlock)
    cat("\n",
        "-------------------------------------------------------------------", "\n")
  }
}
