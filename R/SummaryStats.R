# SummaryStats(Observations, treatments, confidenceInterval)
# Fred Kaesmann Jr Date: 2019-03-04

SummaryStats <- function(observations, treatments, confidenceInterval){
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

  # Make sure observations is numeric
  observations <- as.numeric(observations)
  sheet <- as.data.frame(observations)

 # Condfidence interval check
  if (missing(confidenceInterval) || !missing(confidenceInterval)){
    confidenceInterval <- CheckConfidence(confidenceInterval)
  }
  cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, (100-confidenceInterval*100)))
  cat("\n",
      "-------------------------------------------------------------------", "\n \n")


  # For a single set of observations
  if (missing(treatments) || nlevels(as.factor(treatments)) == 1){

    PrintSixNumSummary(observations)

    CheckNormality(observations)

    # Single Variable t.test
    ttest <- t.test(observations)
    pvalTtest <- ttest$p.value
    print(ttest)

    # Null Hypothesis test
    print(HypothesisTest(pvalTtest, confidenceInterval))

    # Plot your dataset hist -> wait -> boxplot
    print(gghistogram(sheet, "observations", add = "mean",
                      title = "For Internal Use Only"))
    par(ask=TRUE)
    print(ggboxplot(sheet, y = "observations", add = "jitter",
                    title = "For Internal Use Only"))

  } else {

  # Make sure treatments is characters
  treatments <- as.factor(treatments)
  sheet <- as.data.frame(cbind(observations,treatments))

  # Number Summaries
  PrintSixNumSummary(observations, treatments)
  CheckNormality(observations, treatments)

  # Plot it to see everything
  par(ask = TRUE)
  print(ggboxplot(sheet, y = "observations", x = "treatments", add = "jitter",
                  color = "treatments", title = "For Internal Use Only"))
  print(gghistogram(sheet, "observations", add = "mean", facet.by = "treatments",
                    color = "treatments", title = "For Internal Use Only"))

  # If there's only two treatments
  if (nlevels(treatments)== 2){
    welch <- t.test(observations~treatments)
    pvalWelch <- welch$p.value
    print(welch)

    # Null Hypothesis check
    print(HypothesisTest(pvalWelch, confidenceInterval))

  } else {

  # Checking for Homogeniety of Variance with Levene Test
  levy <- levene.test(observations, treatments)
  pvalLevy <- levy$p.value
  print("Levene Test:")
  print(levy)

  # If Levene test p value is less than confidence interval,
  # then assumptions from anova not met and therefor inapplicaple
  if (levy$p.value < confidenceInterval){
    cat(sprintf("Levene Test p-value (%f) < (%s) confidence interval",
                pvalLevy, confidenceInterval))
    cat("\n", "REJECT null hypothesis of homoscedasticity of variance")
    cat("\n", "Variances significantly different across treatments")
    cat("\n", "therefore assumptions for ANOVA not met")
    cat("\n", "-------------------------------------------------------------------", "\n")

  } else { # Assuming levene test doesnt disqualify our data, we proceed to ANOVA
    cat(sprintf("Levene Test p-value (%f) > (%s) confidence interval",
                pvalLevy, confidenceInterval))
    cat("\n", "CANNOT REJECT null hypothesis of homoscedasticity of variance")
    cat("\n", "-------------------------------------------------------------------", "\n")

    # Checking to see if means across treatments are the same
    anov <- aov(observations ~ treatments)
    AnovaTable <- anova(anov)
    pvalAnova <- AnovaTable$`Pr(>F)`[1]
    print(AnovaTable)

    # If ANOVA p value is less than confidence interval,
    if (pvalAnova < confidenceInterval){
      cat(sprintf("ANOVA p-value (%f) < (%s) confidence interval",
                  pvalAnova, confidenceInterval))
      cat("\n", "REJECT null hypothesis")
      cat("\n", "There is at least one set of different sample means")
      cat("\n", "-------------------------------------------------------------------", "\n")

      # run Tukey table to see which means are different
      tuke <- TukeyHSD(anov)
      print(tuke)
      par(ask=TRUE)
      plot(tuke)
    } else {
      cat(sprintf("ANOVA p-value (%f) > (%s) confidence interval",
                  pvalAnova, confidenceInterval))
      print("CANNOT REJECT null hypothesis")
      print("Means are not significantly different across treatments")
    }
   }
  }
 }
}
