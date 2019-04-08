# SummaryStats(Observations, treatments, confidenceInterval, ...)
# Fred Kaesmann Jr Date: 2019-03-04

SummaryStats <- function(observations, treatments, confidenceInterval,
                         is.nonparametric = FALSE, plot = TRUE){
  # Computes some basic summary statistics, runs a Levene test to determine
  # if ANOVA assumptions are met, if levene p > confidenceInterval then it runs an ANOVA test,
  # and a TukeyHSD test if ANOVA p value is less than the confidence interval.
  #
  # Args:
  #   observations: observation variable (numeric)
  #   treatments: Different indexes of observations.
  #   confidenceInterval: Measure of accuracy of statistics tests,
  #                          if no arg present, assumes .05 (95%).
  #                             (optional input)
  #
  # Returns:
  #   A series of summary statistics. (Designed for MAT 342 Project 2)


  # Add requisite libraries
  library(lawstat)
  library(MASS)
  library(ggpubr)
  library(SummaryPack)


  cat("%%%%%%%%%%%%%%%%% Begin Summary Stats %%%%%%%%%%%%%%%%%%%%", "\n \n")

  # Make sure observations is numeric and treatments are factors
  observations <- as.numeric(observations)
  sheet <- cbind.data.frame(observations)

  if (!missing(treatments)){
    treatments <- as.factor(treatments)
    sheet <-cbind.data.frame(observations,treatments)
  }

 # Condfidence interval check
  if (missing(confidenceInterval) || !missing(confidenceInterval)){
    confidenceInterval <- CheckConfidence(confidenceInterval)
  }

  # For a single set of observations run the SingleSetSummary function
  if (missing(treatments) || nlevels(as.factor(treatments)) == 1){
    if (plot == TRUE) SingleSetSummary(observations, confidenceInterval)
    if (plot == FALSE) SingleSetSummary(observations, confidenceInterval, plot = FALSE)


  } else {
    # For observations grouped by treatments

 cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, (100-confidenceInterval*100)))
  cat("\n",
      "-------------------------------------------------------------------", "\n \n")

  # Number Summaries
  PrintSixNumSummary(observations, treatments)
  CheckNormality(observations, treatments)

  # If there's only two treatments run two sample t.test
  if (nlevels(treatments) == 2){

    welch <- t.test(observations~treatments)
    pvalWelch <- welch$p.value
    print(welch)
    HypothesisTest(pvalWelch, confidenceInterval)

  } else {

    if (plot == FALSE){
      if (is.nonparametric == TRUE){
      AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval,
                      is.nonparametric = TRUE, plot = FALSE)
      } else if (is.nonparametric == FALSE){
        AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval,
                        plot = FALSE)
      }

    } else {
      if (is.nonparametric == TRUE){
        AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval,
                        is.nonparametric = TRUE)
      } else {
        AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval)
      }
    }
  }

  # Plot it to see everything
  if (plot == TRUE){
  par(ask = TRUE)
  print(ggboxplot(sheet, y = "observations", x = "treatments", add = "jitter",
                  color = "treatments", title = "For Internal Use Only"))
  print(gghistogram(sheet, "observations", add = "mean", bins = 30,
                    facet.by = "treatments", fill = "treatments",
                    title = "For Internal Use Only"))
  }
  }
}
