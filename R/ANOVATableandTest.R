#' @title ANOVATableandTest()
#'
#' @param observations A Numeric Vector
#' @param treatments A Vector of Factors
#' @param block A Vector of Factors
#' @param confidenceInterval Assumed to be 0.05 unless otherwise specified
#'
#'
#' @return ANOVA p-value for observations by treatments
#' @return ANOVA tables
#' @return Tukey Table if applicable
#'
#'
#' @examples ANOVATableandTest(someobs, atreat, blocking)
ANOVATableandTest <- function(observations, treatments,
                              block = NULL,
                              confidenceInterval=0.05,
                              plot = TRUE){

  observations %<>% as.numeric() # observations needs to be numeric
  treatments %<>% as.factor()    # treatments must be factors

  # For observations and treatments, no block
  if (is.null(block)){
    # Is data Homoscedastic or Heteroscedastic
    LeveneTreatmentPvalue <- LeveneTreatments(observations, treatments)
    cat("\n \n",
        "-------------------------------------------------------------------",
        "\n \n")

    # If data is Homoscedastic run aov()
    if (LeveneTreatmentPvalue > confidenceInterval){
      cat("\n Observations by Treatments Homoscedastic, running aov() \n")
      Ano <- aov(observations ~ treatments) # This has to be separate
      ANOVA <- anova(Ano) # This has to be separate too
      print(ANOVA)
      cat("\n", "Null Hypothesis: Means equal across treatments.")
      cat("\n",
          "Alternative: At least one set of treatment means is different. \n")
      ANOVA$`Pr(>F)`[1] %>% HypothesisTest(confidenceInterval)

      if (plot == TRUE) plot(Ano) # Plot ANOVA data

      if (ANOVA$`Pr(>F)`[1] < confidenceInterval){
        tukeyTable <- TukeyHSD(Ano)
        print(tukeyTable)
        if (plot == TRUE) plot(tukeyTable) # Plot tukeyTable
      }
      return(ANOVA$`Pr(>F)`[1]) # Return for use in other functions
    } else {

      # Use oneway.test if variance is heteroscedastic
      cat("\n",
          "Observations by Treatments Heteroscedastic, running oneway.test",
          "\n")
      oneWay <- oneway.test(observations ~ treatments, var.equal = FALSE) %>%
        print()
      cat("\n Null Hypothesis: Means equal across treatments. \n")
      cat("Alternative: At least one set of treatment means is different. \n")
      oneWay$p.value %>%
        HypothesisTest(confidenceInterval)
      cat("\n",
          "-------------------------------------------------------------------",
          "\n")
      cat("Cannot provide Tukey Table as data is heteroscedastic")
    }

  } else { # If there is a block argument

    # Make sure this is a factor!
    block %<>% as.factor()

    if (nlevels(block) == 2){
      cat("\n",
          "To use RBD effectively, block should have more than two levels",
          "\n",
          "Running ANOVA as observations ~ treatments")
      ANOVATableandTest(observations, treatments,
                        confidenceInterval = confidenceInterval)

    } else {

    # Testing interaction
    cat("\n", "Checking for interations between block and treatments \n")
    anovaTable <- lm(observations ~ treatments * block) %>%
      anova() %>%
      print()
    cat("Null: No effect between treatments and block", "\n")
    cat("Alternative: Block has an affect on treatments", "\n")
    anovaTable$`Pr(>F)`[3] %>% HypothesisTest(confidenceInterval)
    cat("\n \n",
        "-------------------------------------------------------------------",
        "\n \n")

  # If the block p-value is significant
   if (anovaTable$`Pr(>F)`[3] < confidenceInterval){
      cat("\n Cannot make accurate predictions about treatment means \n",
          "if there is a statistically significant affect")

    } else if (anovaTable$`Pr(>F)`[3] > confidenceInterval){

    # If the affect of block on treatments isn't significant,
    # rerun anova, but different
      anovaTableDos <- lm(observations ~ treatments + block) %>%
        anova() %>%
        print()
      pvalTreatments <- anovaTableDos$`Pr(>F)`[1]
      pvalBlock <- anovaTableDos$`Pr(>F)`[2]

      cat("Null: Homogeniety of means across treatments", "\n")
      cat("Alternative: Heterogeneity of means across treatments", "\n")
      pvalTreatments %>% HypothesisTest(confidenceInterval)

      # If a set of means is different across either treatments or block,
      # print a Tukey Table
}
        if (pvalTreatments < confidenceInterval){
          cat("P value of Treatments less than confidence interval", "\n")
          pvalTreatments %>% HypothesisTest(confidenceInterval)
        }
        if (pvalBlock < confidenceInterval) {
          cat("P value of Blocks less than confidence interval", "\n")
          pvalBlock %>% HypothesisTest(confidenceInterval)
        }
        cat("\n",
            "--------------------------------------------------------------",
            "\n \n")
        cat("Tukey table provided to evaluate which means are different",
            "\n")

        # Print a Tukey Table
        tukeTable <- aov(observations ~ treatments + block) %>%
          TukeyHSD()
        print(tukeTable)
        # Then plot it
        if (plot == TRUE) plot(tukeTable)
    }

}
}
