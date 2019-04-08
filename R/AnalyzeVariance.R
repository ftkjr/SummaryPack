# AnalyzeVariance(observations, treatments, block)
# Frederick T. Kaesmann Jr.

AnalyzeVariance <- function(observations, treatments, confidenceInterval,
                            block = NULL, ANOVA = TRUE,
                            post.hoc = NULL, is.nonparametric = FALSE,
                            plot = TRUE){
  # Runs Levene Test(s), then does one way or two way ANOVA
  #
  # Input:
  #   observations: Numeric Vector of Response Variables
  #   treatments: Vector of factors to group the observations
  #   block: additional vector of factors
  #
  # Returns:
  #   Levene Test for Treatments
  #   Levene Test for Block
  #   One way ANOVA if data heteroscedastic
  #   Regular ANOVA

  library(lawstat)
  library(MASS)
  library(dunn.test)
  library(SummaryPack)

  observations <- as.numeric(observations)
  treatments <- as.factor(treatments)

  if (missing(confidenceInterval) || !missing(confidenceInterval)){
    confidenceInterval <- CheckConfidence(confidenceInterval)
  }

  cat("\n \n %%%%%%%%%%%% Variance Analysis %%%%%%%%%%%% \n \n")

  # Checking for Homogeniety of Treatment Variance with Levene Test
  levy <- levene.test(observations, treatments)
  pvalLevy <- levy$p.value
  cat("Levene Test for Observations by Treatments: \n")
  print(levy)
  cat("\n", "Null Hypothesis: Homoscedasticity of Variance.")
  cat("\n", "Alternative: Heteroscedasticity of Variance. \n")
  HypothesisTest(pvalLevy, confidenceInterval)
  cat("\n", "-------------------------------------------------------------------",
      "\n \n")

  # Checking for Homogeniety of Block Variance with Levene Test
  if (!is.null(block)){
    block <- as.factor(block)
    levyBlock <- levene.test(observations, block)
    cat("Levene Test for Observations by Block:")
    pvalLevyBlock <- levyBlock$p.value
    print(levyBlock)
    cat("\n", "Null Hypothesis: Homoscedasticity of Variance.")
    cat("\n", "Alternative: Heteroscedasticity of Variance.\n ")
    HypothesisTest(pvalLevyBlock, confidenceInterval)
    cat("\n", "-------------------------------------------------------------------",
        "\n \n")
  }

  # If data heteroscedastic, run oneway.test
  if (is.nonparametric == TRUE){
    kruskal <- kruskal.test(observations ~ treatments)
    pvalKruskal <- kruskal$p.value
    print(kruskal)
    cat("\n", "Null Hypothesis: Means equal across treatments.")
    cat("\n", "Alternative: At least one set of treatment means is different. \n")
    HypothesisTest(pvalKruskal, confidenceInterval)
    if (pvalKruskal < confidenceInterval){
      if (is.null(post.hoc) || post.hoc == "wilcoxon" || post.hoc == "Wilcoxon"){
        pairwise.wilcox.test(observations, treatments)
      } else if (post.hoc == "dunn" || post.hoc == "Dunn"){
        dunn.test(observations, treatments, kw = FALSE, method = "holm")
      }
    }

  } else if (pvalLevy < confidenceInterval){
    cat("\n Data Heteroscedastic, running oneway.test \n")
    onewayout <- oneway.test(observations ~ treatments, var.equal = FALSE)
    pvaloneway <- onewayout$p.value
    print(onewayout)
    cat("\n Null Hypothesis: Means equal across treatments. \n")
    cat("Alternative: At least one set of treatment means is different. \n")
    HypothesisTest(pvaloneway, confidenceInterval)

  } else if (ANOVA == TRUE) {
    cat("\n Data Homoscedastic, running aov \n")
    anov <- aov(observations ~ treatments)
    AnovaTable <- anova(anov)
    pvalAnova <- AnovaTable$`Pr(>F)`[1]
    print(AnovaTable)
    cat("\n", "Null Hypothesis: Means equal across treatments.")
    cat("\n", "Alternative: At least one set of treatment means is different. \n")
    HypothesisTest(pvalAnova, confidenceInterval)

    if (pvalAnova < confidenceInterval){
      cat("\n", "-------------------------------------------------------------------",
          "\n")
      cat("There is at least one set of different means \n")
      cat("Tukey Table provided to evaluate which means are different: \n")
      tuke <- TukeyHSD(anov)
      print(tuke)
    }
    if (plot == TRUE){
      plot(anov)
      if (!is.null(pvalAnova) && pvalAnova < confidenceInterval){
        plot(tuke)
      }
    }
    }
  }
