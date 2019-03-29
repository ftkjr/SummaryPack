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

  if (missing(confidenceInterval)){
    return(.05)
  } else if (0 < confidenceInterval && confidenceInterval < 1){
    return(confidenceInterval)
    } else if (confidenceInterval > 1 && confidenceInterval < 100){
    # (if they input something like 95 instead of .05)
    return( 1-(confidenceInterval/100))
    } else if (confidenceInterval >= 100){ # Erroneous Confidence Interval
    stop("Confidence Interval Error")
      } else if (confidenceInterval <= 0) {
    stop("Confidence Interval Error")
      }
}
