# tidyverse
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

# SPSS, Stata, SASのデータを読み込む
if (!require("haven")) {
  install.packages("haven")
  library(haven)
}
if (!require("foreign")) {
  install.packages("foreign")
  library(foreign)
}

# 標準化
if (!require("jtools")) {
  install.packages("jtools")
  library("jtools")
}

# クロス表
if (!require("gtsummary")) {
  install.packages("gtsummary")
  library(gtsummary)
}

# 表をワードやエクセル形式で出力
if (!require("flextable")) {
  install.packages("flextable")
  library(flextable)
}

# モダンな可視化②
if (!require("ggpubr")) {
  install.packages("ggpubr")
  library(ggpubr)
}
