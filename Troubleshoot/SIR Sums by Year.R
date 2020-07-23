library(readxl)
setwd("~/GitHub/cprit_2020")
county_SIRs <- read_excel("Analysis/Clean Data for Plots/Every Year 1995-2015/All by Hist Type.xlsx", sheet = 1)
# sheet = 1 --> All histologic types
# sheet = 2 --> Adenocarcinoma
# sheet = 3 --> Small Cell Carcinoma
# sheet = 4 --> Squamous Cell Carcinoma
# sheet = 5 --> Other Non-small Cell Carcinoma
county_SIRs <- data.frame(county_SIRs)
sums <- c()
for (i in 1995:2015){
  data <- subset(county_SIRs, Year == i)
  sums <- append(sums,sum(data["SIR"]))
}
sums
plot(c(1995:2015),sums)
