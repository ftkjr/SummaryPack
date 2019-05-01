# AnalyzeVariance(observations, treatments, block)
# Frederick T. Kaesmann Jr.

AnalyzeVariance <- function(observations, treatments, block = NULL,
                            confidenceInterval = .05, ANOVA = TRUE,
                            top = TRUE, post.hoc = NULL,
                            is.nonparametric = FALSE, plot = TRUE){
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

  if (!require("lawstat")) install.packages("lawstat")
  library(lawstat)
  if (!require("MASS")) install.packages("MASS")
  library(MASS)
  if (!require("dunn.test")) install.packages("dunn.test")
  library(dunn.test)
  if (!require("s20x")) install.packages("s20x")
  library(s20x)
  if (!require("magrittr")) install.packages("magrittr")
  library(magrittr)
  library(SummaryPack)

  observations <- as.numeric(observations)
  treatments <- as.factor(treatments)

  confidenceInterval <- CheckConfidence(confidenceInterval)

if (top == TRUE){
  cat("\n \n %%%%%%%%%%%% Variance Analysis %%%%%%%%%%%% \n \n")

  # Checking for Homogeniety of Treatment Variance with Levene Test
  cat("Levene Test for Observations by Treatments: \n")
  levene <- lawstat::levene.test(observations, treatments) %>%
    print()
  cat("\n", "Null Hypothesis: Homoscedasticity of Variance.")
  cat("\n", "Alternative: Heteroscedasticity of Variance. \n")
  pvalueLevene <- levene$p.value
  HypothesisTest(pvalueLevene, confidenceInterval)
  cat("\n", "-------------------------------------------------------------------",
      "\n \n")

  # Checking for Homogeniety of Block Variance with Levene Test
  if (!is.null(block)){
    cat("Levene Test for Observations by Block:")
    leveneBlock <- lawstat::levene.test(observations, as.factor(block)) %>%
      print()
    cat("\n", "Null Hypothesis: Homoscedasticity of Variance.")
    cat("\n", "Alternative: Heteroscedasticity of Variance.\n ")
    pvalueLeveneBlock <- leveneBlock$p.value
    HypothesisTest(pvalueLeveneBlock, confidenceInterval)
    cat("\n", "-------------------------------------------------------------------",
        "\n \n")
  }
}

  # If nonparametric
  if (is.nonparametric == TRUE){
    kruskal <- kruskal.test(observations ~ treatments)
    print(kruskal)
    cat("\n", "Null Hypothesis: Means equal across treatments.")
    cat("\n", "Alternative: At least one set of treatment means is different. \n")
    pvalueKruskal <- kruskal$p.value
    HypothesisTest(pvalueKruskal, confidenceInterval)
    cat("\n",
        "-------------------------------------------------------------------",
        "\n \n")

    # If Kruskal-Wallis p-value is significant, then run a post hoc test
    if (pvalueKruskal < confidenceInterval){
      if (is.null(post.hoc)){
        # Assume Dunn test
        dunn.test(observations, treatments, alpha = confidenceInterval,
                  kw = FALSE, method = "holm")
        # If specified run a pairwise wilcoxon
      } else if (post.hoc == "wilcoxon" || post.hoc == "Wilcoxon"){
        pairwise.wilcox.test(observations, treatments)
        # Or specify Dunn
      } else if (post.hoc == "dunn" || post.hoc == "Dunn"){
        dunn.test(observations, treatments, alpha = confidenceInterval,
                  kw = FALSE, method = "holm")
      }
    }
    # If data heteroscedastic, run oneway.test
  } else if (pvalueLevene < confidenceInterval){
    cat("\n Data Heteroscedastic, running oneway.test \n")
    oneWay <- oneway.test(observations ~ treatments, var.equal = FALSE)
    oneWay$p.value %>%
      print()
    HypothesisTest(confidenceInterval)
    cat("\n Null Hypothesis: Means equal across treatments. \n")
    cat("Alternative: At least one set of treatment means is different. \n")

    cat("\n", "-------------------------------------------------------------------",
        "\n")
    cat("Cannot provide Tukey Table as data is heteroscedastic")

  } else if (ANOVA == TRUE) {
    cat("\n Data Homoscedastic, running aov \n")
    Ano <- aov(observations ~ treatments)
    ANOVA <- anova(Ano)
    print(ANOVA)
    cat("\n", "Null Hypothesis: Means equal across treatments.")
    cat("\n", "Alternative: At least one set of treatment means is different. \n")
    pvalAnova <- ANOVA$`Pr(>F)`[1]
    HypothesisTest(pvalAnova, confidenceInterval)

    if (pvalAnova < confidenceInterval){
      cat("\n", "-------------------------------------------------------------------",
          "\n")
      cat("There is at least one set of different means \n")
      cat("Tukey Table provided to evaluate which means are different: \n")
      tuke <- TukeyHSD(Ano)
      print(tuke)

    }
    if (plot == TRUE){
      if (!is.null(pvalAnova) && pvalAnova < confidenceInterval){
        plot(tuke)
      }
      plot(Ano)
    }
    }
  }
