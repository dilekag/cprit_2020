library(readxl)
setwd("~/GitHub/cprit_2020")
county_SIRs <- read_excel("Analysis/Clean Data for Plots/Every Year 1995-2015/All by Hist Type.xlsx", sheet = 1)
county_SIRs <- data.frame(county_SIRs)
sums <- c()
for (i in 1995:2015){
  data <- subset(county_SIRs, Year == i)
  sums <- append(sums,sum(data["SIR"]))
}
sums
plot(c(1995:2015),sums)
