

KruskalSummary <- function(observations, treatments, confidenceInterval = 0.05, post.hoc = "Dunn"){

  observations %<>% as.numeric()
  treatments %<>% as.factor()
  confidenceInterval %<>% CheckConfidence()


  kruskal <- kruskal.test(observations ~ treatments) %>%
    print()
  cat("\n", "Null Hypothesis: Means equal across treatments.")
  cat("\n", "Alternative: At least one set of treatment means is different. \n")
  kruskal$p.value %>% HypothesisTest(confidenceInterval)
  cat("\n",
      "-------------------------------------------------------------------",
      "\n \n")

  # If Kruskal-Wallis p-value is significant, then run a post hoc test
  if (kruskal$p.value < confidenceInterval){
    # Dunn's Test is assumed
    if (post.hoc == "dunn" || post.hoc == "Dunn"){
      dunn.test(observations, treatments,
                alpha = confidenceInterval,
                kw = FALSE,
                method = "holm")
      # If specified run a pairwise wilcoxon
    } else if (post.hoc == "wilcoxon" || post.hoc == "Wilcoxon"){
      pairwise.wilcox.test(observations, treatments)
    }
  }


}
