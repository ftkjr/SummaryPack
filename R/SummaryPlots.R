#' @title SummaryPlots()
#'
#' @param observations Numeric Vector
#' @param treatments Factors
#' @param block Factors
#'
#' @return Histograms and Box Plots
#'
#'
#' @examples SummaryPlots(someObservations, byTreatments)
SummaryPlots <- function(observations, treatments, block){

  if (!require("ggpubr")) install.packages("ggpubr")
  library(ggpubr)
  if (!require("tibble")) install.packages("tibble")
  library(tibble)

  observations %<>% as.numeric()
  sheet <- tibble(observations)

  # Ask before moving to the next plot
  par(ask=TRUE)

  if (!missing(treatments)) treatments %<>% as.factor()

  # Single Set Histogram
  print(gghistogram(sheet, "observations",
                    add = "mean",
                    bins = 30,
                    fill = "red",
                    title = "Histogram of observations"))

  # Single Set Boxplot
  print(ggboxplot(sheet,
                  y = "observations",
                  color = "red",
                  add = "jitter",
                  title = "Distribution of observations"))

  # observations by treatments
  if (!missing(treatments)){

    sheet <- tibble(observations, treatments)

    # Histogram of observations by treatments
    print(gghistogram(sheet, "observations",
                      add = "mean",
                      bins = 30,
                      facet.by = "treatments",
                      fill = "treatments",
                      palette = "npc",
                      title = "Histogram of observations
                      Faceted by treatments"))
     # Box Plot
      print(ggboxplot(sheet,
                      y = "observations",
                      x = "treatments",
                      add = "jitter",
                      color = "treatments",
                      title = "Distribution of observations
                      Grouped by treatments"))
  }

    # observations by block
    if (!missing(block)){
      block <- as.factor(block)
      sheet <- tibble(observations, treatments, block)

      # Histogram obs x block
      print(gghistogram(sheet,
                        x = "observations",
                        add = "mean",
                        bins = 30,
                        facet.by = "block",
                        color = "block",
                        title = "Histogram of observations
                        Faceted by block"))
      print(ggboxplot(sheet,
                      x = "block",
                      y = "observations",
                      add = "jitter",
                      color = "block",
                      palette = "npc",
                      title = "Distribution of observations
                      Grouped by block"))
      print(ggboxplot(sheet,
                      x = "block",
                      y = "observations",
                      add = "jitter",
                      color = "treatments",
                      palette = "npc",
                      title = "Distribution of treatments observations
                      Grouped by block"))
      print(ggboxplot(sheet,
                      x = "treatments",
                      y = "observations",
                      add = "jitter",
                      color = "block",
                      palette = "npc",
                      title = "Distribution of block observations
                      Grouped by treatments"))


  }

}
