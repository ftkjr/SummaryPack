# CheckConfidence(confidenceInterval)
# Frederick T. Kaesmann Jr.  3/21/2019

CheckConfidence <- function(confidenceInterval){
  # Condfidence interval check
  #
  # Arg: confidenceInterval: should be pretty self explanatory
  #
  # Returns: .05 confidence interval if none entered
  #          Checks validity of confidence interval if entered
  #

  # If nothing entered, assume the normal .05
  if (missing(confidenceInterval)){
      return(.05)
    } else if (0 < confidenceInterval && confidenceInterval < .2){
        return(confidenceInterval)
    } else if (confidenceInterval >= 90 && confidenceInterval < 100){
      # (if they input something like 95 instead of .05)
        return( 1-(confidenceInterval/100))
    } else if (confidenceInterval >= 100 || confidenceInterval <= 0) {
        stop("Confidence Interval Error")
    } else if (confidenceInterval >= .2 && confidenceInterval < 90) {
        stop("Confidence Interval Error")
  }
}
