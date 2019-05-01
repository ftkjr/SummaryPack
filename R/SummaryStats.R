# SummaryStats()
# Fred Kaesmann Jr Date: 2019-03-04

SummaryStats <- function(observations, treatments, confidenceInterval = 0.05,
                         post.hoc = NULL, is.nonparametric = FALSE,
                           plot = TRUE){
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
  library(SummaryPack)


  cat("%%%%%%%%%%%%%%%%% Begin Summary Stats %%%%%%%%%%%%%%%%%%%%", "\n \n")

  # Make sure observations is numeric and treatments are factors
  observations %>%
    as.numeric()
  sheet <- cbind.data.frame(observations)

  # If the treament
  if (!missing(treatments)){
    treatments <- as.factor(treatments)
    sheet <-cbind.data.frame(observations,treatments)
  }

 # Condfidence interval check
  confidenceInterval %<>%
    CheckConfidence()

  # For a single set of observations run the SingleSetSummary function
  if (missing(treatments) || nlevels(as.factor(treatments)) == 1){
    if (plot == TRUE) SingleSetSummary(observations, confidenceInterval)
    if (plot == FALSE) SingleSetSummary(observations, confidenceInterval,
                                        plot = FALSE)

  } else {
    # For observations grouped by treatments
 cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, (100-confidenceInterval*100)))
  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # Number Summaries
  PrintSixNumSummary(observations, treatments)
  CheckNormality(observations, treatments)

  # If there's only two treatments run two sample t.test
  if (nlevels(treatments) == 2){
    if (is.nonparametric == TRUE){

      # Non-Parametric data
      wilcox <- wilcox.test(observations~treatments)
      print(wilcox)
      pvalWilcox <- wilcox$p.value
      HypothesisTest(pvalWilcox, confidenceInterval)

    } else {

    # Parametric data
    welch <- t.test(observations~treatments)
    print(welch)
    pvalWelch <- welch$p.value
    HypothesisTest(pvalWelch, confidenceInterval)
    }

  } else {
    # Run ANOVA or One Way depending on Levene P-value
    if (plot == FALSE){
        AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval,
                        plot = FALSE)
    } else {
      AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval)
    }

    cat("\n",
        "-------------------------------------------------------------------",
        "\n \n")

    # Run nonparametric Kruskal-Wallis test
      if (plot == TRUE){
        # Plot and Wilcoxon
        if (is.null(post.hoc)){
          AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval,
                          is.nonparametric = TRUE, post.hoc = "dunn")
        } else if (post.hoc == "wilcoxon" || post.hoc == "Wilcoxon"){
          AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval,
                          is.nonparametric = TRUE, post.hoc = "wilcoxon")
          # Plot and Dunn
        } else if (post.hoc == "dunn" || post.hoc == "Dunn"){
          AnalyzeVariance(observations, treatments, block = NULL, confidenceInterval,
                          is.nonparametric = TRUE, post.hoc = "dunn")
        }
      } else if (plot == FALSE) {
        # No Plot and Wilcoxon
        if (is.null(post.hoc) || post.hoc == "wilcoxon" || post.hoc == "Wilcoxon"){
          AnalyzeVariance(observations, treatments, confidenceInterval,
                          block = NULL, #top = FALSE,
                          is.nonparametric = TRUE, post.hoc = "wilcoxon",
                          plot = FALSE)
          # No Plot and Dunn
        } else if (post.hoc == "dunn" || post.hoc == "Dunn"){
          AnalyzeVariance(observations, treatments, confidenceInterval,
                          block = NULL, #top = FALSE,
                          is.nonparametric = TRUE, post.hoc = "dunn",
                          plot = FALSE)
        }
      }
  }

  # Plot it to see everything
  if (plot == TRUE){
  par(ask = TRUE)
    print(gghistogram(sheet, "observations", add = "mean", bins = 30,
                      fill = "red"))
    print(gghistogram(sheet, "observations", add = "mean", bins = 30,
                    facet.by = "treatments", fill = "treatments",
                    palette = "npc"))
    print(ggboxplot(sheet, y = "observations", x = "treatments", add = "jitter",
                  color = "treatments"))

  }
  }
}
