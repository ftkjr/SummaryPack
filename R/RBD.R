# RBD(Observations, treatment, block, confidenceInterval)
# Frederick T. Kaesmann Jr.  3/20/19

RBD <- function(observations, treatments, block, confidenceInterval, LeveneBlock = FALSE, plot = TRUE){
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

  # Add requisite libraries
  library(ggpubr)
  library(lawstat)
  library(MASS)
  library(SummaryPack)

  #Checking inputs
  dataset <- as.data.frame(cbind(treatments,observations,block))
  dataset$observations <- as.numeric(observations)
  dataset$treatments <- as.factor(treatments)
  dataset$block <- as.factor(block)

  cat("%%%%%%%%%%%%%%% Begin Randomized BLock Design Test %%%%%%%%%%%%%%%%%%%", "\n \n")


  # Condfidence interval check
  if (missing(confidenceInterval) || !missing(confidenceInterval)){
    confidenceInterval <- CheckConfidence(confidenceInterval)
  }
  cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, (100-confidenceInterval*100)))
  cat("\n",
      "-------------------------------------------------------------------", "\n \n")

  # Basic Number Summary
  PrintSixNumSummary(observations, treatments, block)
  cat("\n",
      "-------------------------------------------------------------------", "\n \n")
  cat("Observations by Treatments")
  print(CheckNormality(observations, treatments))
  cat("\n",
      "-------------------------------------------------------------------", "\n \n")
  cat("Observations by Block")
  print(CheckNormality(observations, block))

  # If LeveneBlock==TRUE run levene test on observations by block
  if (LeveneBlock == TRUE){
    cat("\n", "-------------------------------------------------------------------",
        "\n")
    levyBlock <- levene.test(observations, block)
    pvalLevyBlock <- levyBlock$p.value
    cat("Levene Test by block because you asked so nicely", "\n")
    print(levyBlock)
    cat("Null: Homoscedasticity of Variance", "\n")
    cat("Alternative: Not Homoscedasticity of Variance, aka Heteroscedasticity", "\n")
    print(HypothesisTest(pvalLevyBlock, confidenceInterval))
    cat("\n",
        "-------------------------------------------------------------------", "\n \n")
  }

  # Levene test for homoscedasticity of variance OF TREATMENTS
  levyTreat <- levene.test(observations, treatments)
  pvalLevyTreat <- levyTreat$p.value
  print("Levene Test by Treatment:")
  print(levyTreat)
  cat("\n")
  cat("Null: Homoscedasticity of Variance", "\n")
  cat("Alternative: Not Homoscedasticity of Variance, aka Heteroscedasticity", "\n")
  print(HypothesisTest(pvalLevyTreat, confidenceInterval))

  # If Levene test p value is less than confidence interval,
  # then assumptions from anova not met and therefore inapplicaple
  if (levyTreat$p.value < confidenceInterval){
    cat("\n", "Variances significantly different across treatments")
    cat("\n", "therefore assumptions for ANOVA not met")
    cat("\n", "-------------------------------------------------------------------",
        "\n")

  } else { # Assuming levene test doesnt disqualify our data, we proceed to ANOVA
    cat("\n", "-------------------------------------------------------------------",
        "\n")

  # ANOVA Table
  anov <- lm(observations ~ treatments * block)
  anovaTable <- anova(anov)
  pvalTreatments <- anovaTable$`Pr(>F)`[1]
  pvalBlock <- anovaTable$`Pr(>F)`[2]
  print(anovaTable)
  cat("\n", "-------------------------------------------------------------------",
      "\n")
  cat("Null: Homogeniety of means across treatments", "\n")
  cat("Alternative: Heterogeneity of means across treatments", "\n")
  print(HypothesisTest(pvalTreatments, confidenceInterval))


  # Tukey table if applicable
  if ((pvalTreatments < confidenceInterval) || (pvalBlock < confidenceInterval) ){
    cat("P value less than confidence interval", "\n")
    cat("\n", "-------------------------------------------------------------------",
        "\n")
    cat("Tukey table provided to evaluate which means are different", "\n")
    stuff <- aov(observations~treatments+block)
    tukeTable <- TukeyHSD(stuff)
  print(tukeTable)
  if (plot == TRUE) {
    plot(tukeTable)
    }
  }
  }#closeANOVA
  if (plot == TRUE){
    # Plot it
    par(ask=TRUE)
    print(gghistogram(dataset, x = "observations", add = "mean",
                      color = "red", bins = 30, title = "For Internal Use Only"))
    print(gghistogram(dataset, x = "observations", add = "mean", bins = 30,
                      facet.by = "treatments", color = "treatments",
                      palette = "npc", title = "For Internal Use Only"))
    print(gghistogram(dataset, x = "observations", add = "mean", bins = 30,
                      facet.by = "block", color = "block",
                      title = "For Internal Use Only"))
    print(ggboxplot(dataset, x = "treatments", y = "observations", add = "jitter",
                    color = "treatments", palette = "npc",
                    xlab = "observations~treatments", title = "For Internal Use Only"))
    print(ggboxplot(dataset, x = "block", y = "observations", add = "jitter",
                    color = "treatments", palette = "npc",
                    title = "For Internal Use only"))
    print(ggboxplot(dataset, x = "block", y = "observations", add = "jitter",
                    color = "block", palette = "npc",
                    title = "For Internal Use Only"))

  }
}#close function
# Fin
