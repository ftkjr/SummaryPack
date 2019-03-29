# CheckNormality(observations, treatment)
# Fred Kaesmann Jr  2019 - 03 - 04

CheckNormality <- function(observations, treatments, plot = FALSE, levenetest = FALSE){
  # Checks how normal the provided data is by IQR/SD, Boxplot, and Histogram
  #
  #
  # Inputs: Observations - Observation variable (numerical)
  #         treatments - treatment variable
  # Output: Prints the Inter Quartile Range, Standard Deviation, then the IQR/SD
  #

  library(SummaryPack)
  library(lawstat)
  library(MASS)
  library(ggpubr)

  # Let's make sure it's the right kind of variable
  observations <- as.numeric(observations)
  numbers <- as.data.frame(observations)

  # If there's no treatment variable then run this:
  if (missing(treatments) || nlevels(treatments) == 1){

  # If treatments not entered or treatments == 1, run single dataset
  cat("\n", "Checking for Normality of single dataset:", "\n \n")

  # The meat and potatoes

  iqr <- c("IQR:", IQR(observations))
  standdev <- c("Stand Dev:", sd(observations))

  # iqr/sd should be approx equal to 1.34 for normal data
  normalish <- c("IQR/Sigma:", IQR(observations)/sd(observations))

  # Combine them into a data frame
  numberset <- as.data.frame(rbind(iqr,standdev,normalish))

  # Print it
  print(numberset, row.names = F)

  # Graph it if TRUE
  if (plot == TRUE){

  # Wait for it
  par(ask = TRUE)

  # Histogram it
  print(gghistogram(numbers, x = "observations", add = "mean",
              title = "For Internal Use Only"))

  # Boxplot it
  print(ggboxplot(numbers, y = "observations", add = "jitter",
            title = "For Internal Use Only"))

  # Bop it
  }

  } else {  # If there is a treatment variable run this:

    # Treatments need to be factors
    dataset <- as.data.frame(cbind(observations,treatments))
    dataset$treatments <- as.factor(treatments)
    dataset$observations <- as.numeric(observations)


    # tapply to calculate for each treatment
    iqr <- c("IQR:", tapply(observations, treatments, IQR))
    standdev <- c("Stand Dev:", tapply(observations, treatments, sd))

    # They should be floating around 1.34 if the data is normal
    normalish <- c("IQR/Sigma:", tapply(observations, treatments, IQR) / tapply(observations, treatments, sd))

    # Comine them into one data frame
    numberset <- as.data.frame(rbind(iqr, standdev, normalish))

    # Print that ish
    print(numberset, row.names = F)

    if (levenetest == TRUE) {

      cat("\n", "Running a Levene Test because you asked:", "\n \n")
      print(levene.test(observations, treatments))
    }

    if (plot == TRUE){
    par(ask = TRUE)

    # Histogram and Boxplot
    print(gghistogram(dataset, x = "observations", facet.by = "treatments",
                add = "mean", color = "treatments",
                title = "For Internal Use Only"))

    print(ggboxplot(dataset, x = "treatments", y= "observations",
              add = "jitter", color = "treatments",
              title = "For Internal Use Only"))
    }
  }
}
# Fin