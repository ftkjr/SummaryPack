# RBD(Observations, treatment, block, confidenceInterval)
# Frederick T. Kaesmann Jr.  3/20/19

RBD <- function(observations, treatments, block, confidenceInterval = 0.05, plot = TRUE){
  # Performs a Randomized Block Design
  #
  # Args: observations
  #       treatment
  #       block: "Something you can't control and
  #                  something you usually aren't interested in."
  #                                             - Prof Ray Mugno
  #       confidenceInterval: self explanatory, assumed to be .05 (95%) if unentered
  #
  # Returns:
  #         5 number summary for the set of observations
  #         5 number summary for observations by treatments
  #         Means by treatment
  #         Means by block
  #         Levene test for observations by treatments
  #         Two-Way ANOVA
  #         TukeyHSD if appropriate
  #         Boxplot of observations~treatments
  #         ggboxplot(dataset, x = "block", y = "observations", color = "treatments")
  #

  # Add requisite libraries, install if necessary
  if (!require("lawstat")) install.packages("lawstat")
  library(lawstat)
  if (!require("MASS")) install.packages("MASS")
  library(MASS)
  if(!require("ggpubr")) install.packages("ggpubr")
  library(ggpubr)
  library(SummaryPack)

  #Checking inputs
  observations <- as.numeric(observations)
  treatments <- as.factor(treatments)
  block <- as.factor(block)
  dataset <- cbind.data.frame(treatments,observations,block)

  cat("%%%%%%%%%%%%%%% Begin Randomized BLock Design Test %%%%%%%%%%%%%%%%%%%", "\n \n")


  # Condfidence interval check
  confidenceInterval <- CheckConfidence(confidenceInterval)

  cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, (100-confidenceInterval*100)))
  cat("\n",
      "-------------------------------------------------------------------", "\n \n")

  # Basic Number Summary
  PrintSixNumSummary(observations, treatments, block)

  # Check Normality of observations by treatment
  cat("\n Observations by Treatments \n")
  CheckNormality(observations, treatments)
  cat("\n",
      "-------------------------------------------------------------------", "\n \n")

  # Check Normality of observations by block
  cat("Observations by Block \n")
  CheckNormality(observations, block)
  cat("\n", "-------------------------------------------------------------------",
        "\n")

  AnalyzeVariance(observations, treatments, confidenceInterval, block = block,
                  ANOVA = FALSE, plot = FALSE)

  # ANOVA Table
  cat("\n", "Checking for interations between block and treatments \n")
  anov <- lm(observations ~ treatments * block)
  anovaTable <- anova(anov)
  print(anovaTable)
  pvalTreatmentbyBlock <- anovaTable$`Pr(>F)`[3]
  cat("Null: No effect between treatments and block", "\n")
  cat("Alternative: Block has an affect on treatments", "\n")
  HypothesisTest(pvalTreatmentbyBlock, confidenceInterval)
  cat("\n \n", "-------------------------------------------------------------------",
      "\n \n")

  # Insert something here to remove the Treatments:Block portion of ANOVA table if the T:B pvalue cannot reject null hypothesis
  if (anovaTable$`Pr(>F)`[3] > confidenceInterval){
    anov <- lm(observations ~ treatments + block)
    anovaTable <- anova(anov)
    pvalTreatments <- anovaTable$`Pr(>F)`[1]
    pvalBlock <- anovaTable$`Pr(>F)`[2]
    print(anovaTable)
    cat("Null: Homogeniety of means across treatments", "\n")
    cat("Alternative: Heterogeneity of means across treatments", "\n")
    HypothesisTest(pvalTreatments, confidenceInterval)


  # Tukey table if applicable
  if ((pvalTreatments < confidenceInterval) || (pvalBlock < confidenceInterval) ){
    if (pvalTreatments < confidenceInterval) {
      cat("P value of Treatments less than confidence interval", "\n")
      HypothesisTest(pvalTreatments, confidenceInterval)
    }
    if (pvalBlock < confidenceInterval) {
      cat("P value of Blocks less than confidence interval", "\n")
      HypothesisTest(pvalBlock, confidenceInterval)
    }
    cat("\n", "-------------------------------------------------------------------",
        "\n \n")
    cat("Tukey table provided to evaluate which means are different", "\n")
    twoway <- aov(observations ~ treatments + block)
    tukeTable <- TukeyHSD(twoway)
    print(tukeTable)

  if (plot == TRUE) {
    plot(tukeTable)
    }
  }
  } else if (anovaTable$`Pr(>F)`[3] < confidenceInterval){
    cat("\n Cannot make accurate predictions abouttreatment means \n",
       "if there is a statistically significant affect")
  }
  if (plot == TRUE){
    # Plot it
    par(ask=TRUE)
    # Single histogram of observation vector
    print(gghistogram(dataset, x = "observations", add = "mean",
                      color = "red", bins = 30, title = "For Internal Use Only"))
    # Histogram of observations by treatments
    print(gghistogram(dataset, x = "observations", add = "mean", bins = 30,
                      facet.by = "treatments", color = "treatments",
                      palette = "npc", title = "For Internal Use Only"))
    # Histogram of observations by block
    print(gghistogram(dataset, x = "observations", add = "mean", bins = 30,
                      facet.by = "block", color = "block",
                      title = "For Internal Use Only"))
    # Box Plots
    print(ggboxplot(dataset, x = "treatments", y = "observations", add = "jitter",
                    color = "treatments", palette = "npc",
                    xlab = "observations~treatments", title = "For Internal Use Only"))
    print(ggboxplot(dataset, x = "block", y = "observations", add = "jitter",
                    color = "block", palette = "npc",
                    title = "For Internal Use only"))
    print(ggboxplot(dataset, x = "block", y = "observations", add = "jitter",
                    color = "treatments", palette = "npc",
                    title = "For Internal Use only"))
    print(ggboxplot(dataset, x = "treatments", y = "observations", add = "jitter",
                    color = "block", palette = "npc",
                    title = "For Internal Use Only"))
  }
}#close function
# Fin
