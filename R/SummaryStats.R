# SummaryStats()
# Fred Kaesmann Jr Date: 2019-03-04

SummaryStats <- function(observations, treatments,
                         confidenceInterval = 0.05,
                         post.hoc = NULL,
                         is.nonparametric = FALSE,
                         plot.ANOVA = TRUE,
                         plot = TRUE){
  # Computes some basic summary statistics, runs a Levene test to determine
  # if ANOVA assumptions are met,
  # if levene p > confidenceInterval then it runs an ANOVA test,
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


  # Add requisite libraries, install them if they're not already installed

  if (!require("lawstat")) install.packages("lawstat")
  library(lawstat)
  if (!require("MASS")) install.packages("MASS")
  library(MASS)
  if (!require("dunn.test")) install.packages("dunn.test")
  library(dunn.test)
  if (!require("s20x")) install.packages("s20x")
  library(s20x)
  if(!require("ggpubr")) install.packages("ggpubr")
  library(ggpubr)
  if (!require("magrittr")) install.packages("magrittr")
  library(magrittr)
  if (!require("tibble")) install.packages("tibble")
  library(tibble)
  library(SummaryPack)


  cat("%%%%%%%%%%%%%%%%% Begin Summary Stats %%%%%%%%%%%%%%%%%%%%", "\n \n")

  # Make sure observations is numeric and treatments are factors
  observations %<>% as.numeric()
  sheet <- tibble(observations)


  # If the treatment arg exists
  if (!missing(treatments)){
    treatments %<>% as.factor()
    sheet <- tibble(observations,treatments)
  }

 # Condfidence interval check
  confidenceInterval %<>% CheckConfidence()

  # For a single set of observations run the SingleSetSummary function
  if (missing(treatments) || nlevels(as.factor(treatments)) == 1){
    if (plot == TRUE) SingleSetSummary(observations, confidenceInterval)
    if (plot == FALSE) SingleSetSummary(observations, confidenceInterval,
                                        plot = FALSE)
  # For observations grouped by treatments
  } else {

    cat("Your confidence interval is:", "\n",
        sprintf("%s, (%s percent)",
                confidenceInterval, (100-confidenceInterval*100)))
    cat("\n",
        "-------------------------------------------------------------------",
        "\n \n")

  # Number Summaries
  PrintSixNumSummary(observations, treatments)
  CheckNormality(observations, treatments)

  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # If there's only two treatments run two sample t.test
  if (nlevels(treatments) == 2){
    if (is.nonparametric == TRUE){
      # Non-Parametric data
      wilcox <- wilcox.test(observations~treatments) %>%
        print()
      wilcox$p.value %>% HypothesisTest(confidenceInterval)

    } else {

    # Parametric data
    welch <- t.test(observations~treatments) %>%
      print()
    welch$p.value %>% HypothesisTest(confidenceInterval)
    }

  } else {
    # Run ANOVA or One Way depending on Levene P-value
    if (plot.ANOVA == FALSE || plot == FALSE){
        ANOVATableandTest(observations, treatments,
                          confidenceInterval = confidenceInterval,
                          plot = FALSE)
    } else {
      ANOVATableandTest(observations, treatments,
                        confidenceInterval = confidenceInterval)
    }

    # Run nonparametric Kruskal-Wallis test
    if (is.nonparametric == TRUE){
      cat("\n",
          "-------------------------------------------------------------------",
          "\n \n")

      if (plot == TRUE){

        # If post.hoc is null, assume Dunn
        if (is.null(post.hoc)){
          KruskalSummary(observations, treatments, confidenceInterval)

          # If the Wilcoxon post hoc is specified
        } else if (post.hoc == "wilcoxon" || post.hoc == "Wilcoxon"){
          KruskalSummary(observations, treatments, confidenceInterval,
                         post.hoc = "Wilcoxon")

          # If Dunn specified
        } else if (is.null(post.hoc)
                   || post.hoc == "dunn" || post.hoc == "Dunn"){
          KruskalSummary(observations, treatments, confidenceInterval,
                          post.hoc = "dunn")
        }

        # If plot is false
      } else if (plot == FALSE) {

        # No Plot and Wilcoxon
        if ( post.hoc == "wilcoxon" || post.hoc == "Wilcoxon"){

           KruskalSummary(observations, treatments, confidenceInterval,
                          post.hoc = "Wilcoxon")
          # No Plot and Dunn
        } else if (is.null(post.hoc)
                   ||post.hoc == "dunn" || post.hoc == "Dunn"){
          KruskalSummary(observations, treatments, confidenceInterval,
                          post.hoc = "Dunn")
        }
      }
    }

  # Plot it to see everything
  if (plot == TRUE){
    # If treament arg exists
  if (!missing(treatments)){
    SummaryPlots(observations, treatments)

    # If just a single observations vector is entered
  } else {
    SummaryPlots(observations)
  }
  }
  }
}
}
