library(tidyverse)
setwd("C:/Users/jra64/Documents/Presentations/EAA 2021")

power_stats <- read.csv("power law stats.csv")

#fixing problem in data file (I already fixed the error in the analyze group stats script, but had saved the file with the wrong sign for the values)
power_stats$test_statistic <- 0 - power_stats$test_statistic

boxplot <- function(Data, x, y, fill = "gray", outlier_fill = "black", outlier_shape = 21, title = NULL, xlab = NULL, ylab = NULL) {
  ggplot(Data, aes(x, y, width = 5, height = 3)) +
    geom_boxplot(fill= fill, color = "black", outlier.colour = "black", outlier.shape = outlier_shape,
                 outlier.size = 3, outlier.fill = outlier_fill) +
    labs(title = title, x= xlab, y = ylab) +
    theme_bw() +
    theme(plot.title = element_text(family="sans", color="black", size = 18, hjust = .5, face = "bold"),
          axis.title.x = element_text(color = "black", size = 14, face = "bold"),
          axis.title.y = element_text(color = "black", size = 14, face = "bold"))
}


boxplot(power_stats, x = as.factor(power_stats$year), y = power_stats$largest_territory, title = "Largest Simulated Territory", xlab = "Year", ylab = "Territory Size")
boxplot(power_stats, x = as.factor(power_stats$year), y = power_stats$test_statistic, title = "Power Law Test Statistic", xlab = "Year")
boxplot(power_stats, x = as.factor(power_stats$year), y = power_stats$alpha, title = "Simulated alpha (slope of best-fit power law)", xlab = "Year")



