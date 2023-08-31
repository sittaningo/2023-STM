# tidyverse
if (!require("renv")) {
  install.packages("renv")
  library(renv)
}
renv::restore()

source("LibraryImport.R")
source("DataImport.R")
source("FeatureEngineering.R")
source("SkillMatching.R")
source("TargetPopulation.R")
source("featureEngineering_Additional.R")
source("Analysis.R")