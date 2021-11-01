library(tidyverse)


#load "system_stats.csv"
x <- as.data.frame(read.csv(file = file.choose()))

x25 <- filter(x, group_size == 25)
x50 <- filter(x, group_size == 50)
x75 <- filter(x, group_size == 75)
x100 <- filter(x, group_size == 100)
x150 <- filter(x, group_size == 150)
x200 <- filter(x, group_size == 200)

#create a dataframe to hold the population means
population_means <- as.data.frame(matrix(nrow = 700, ncol = 7))
colnames(population_means) <- c("Year", "group_size_25", "group_size_50", "group_size_75", "group_size_100", "group_size_150", "group_size_200")

#find average populations through time for each setting of the group_size variable
for (i in 600:1299) {
  temp <- filter(x25, Year == i)
  mean_25 <- mean(temp$Agents)
  temp <- filter(x50, Year == i)
  mean_50 <- mean(temp$Agents)
  temp <- filter(x75, Year == i)
  mean_75 <- mean(temp$Agents)
  temp <- filter(x100, Year == i)
  mean_100 <- mean(temp$Agents)
  temp <- filter(x150, Year == i)
  mean_150 <- mean(temp$Agents)
  temp <- filter(x200, Year == i)
  mean_200 <- mean(temp$Agents)
  current_row <- c(i, mean_25, mean_50, mean_75, mean_100, mean_150, mean_200)
  population_means[i-599,] <- current_row
}


#to plot population means by group_size

population_plot <- ggplot(population_means) +
  theme_bw() +
  geom_line(aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(aes(x = Year, y = group_size_75, color = "green")) +
  geom_line(aes(x = Year, y = group_size_100, color = "orange")) +
  geom_line(aes(x = Year, y = group_size_150, color = "purple")) +
  geom_line(aes(x = Year, y = group_size_200, color = "red")) +
  labs(title = "All Runs, Means by Group Size", x="Year", y = "Number of Agents") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red','blue'='blue', "black" = "black", "purple" = "purple", "orange" = "orange", "green" = "green"), 
                      labels = c("Group Size = 25", "Group Size = 50", "Group Size = 75", "Group Size  = 100", "Group Size = 150", "Group Size  = 200")) +
  theme(legend.position=c(.1,.8))

population_plot

ggsave(population_plot, file = "population_plot.jpg", dpi = 600)


#plot population for all the runs, with the means by group size on top
plot <- ggplot() +
  theme_bw()

add_line <- function(data, plot, i, clr) {
  list_runs <- unique(data$run)
  data <- filter(data, run == list_runs[i])
  plot <- plot + 
    geom_line(data = data, aes(x = Year, y = Agents), color = clr)  
  return(plot)  
}

list_runs <- unique(x$run)
for (i in 1:length(list_runs)) {
  plot <- add_line(x, plot, i, "lightblue")
}

plot

population_plot_all <- plot +
  theme_bw() +
  geom_line(data = population_means, aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(data = population_means, aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(data = population_means, aes(x = Year, y = group_size_75, color = "green")) +
  geom_line(data = population_means, aes(x = Year, y = group_size_100, color = "orange")) +
  geom_line(data = population_means, aes(x = Year, y = group_size_150, color = "purple")) +
  geom_line(data = population_means, aes(x = Year, y = group_size_200, color = "red")) +
  labs(title = "All Runs, Means by Group Size", x="Year", y = "Number of Agents") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red','blue'='blue', "black" = "black", "purple" = "purple", "orange" = "orange", "green" = "green"), 
                      labels = c("Group Size = 25", "Group Size = 50", "Group Size = 75", "Group Size  = 100", "Group Size = 150", "Group Size  = 200")) +
  theme(legend.position=c(.1,.8))

population_plot_all

ggsave(population_plot_all, file = "population_plot_all.jpg", dpi = 600)

#filter for the runs with s = .02, beta = .1, and mu = .1

x_s.02 <- filter(x, s == .02) %>%
  filter(beta == .1) %>%
  filter(mu == .1)

#split by group size
x_s.02_25 <- filter(x_s.02, group_size == 25)
x_s.02_50 <- filter(x_s.02, group_size == 50)
x_s.02_75 <- filter(x_s.02, group_size == 75)
x_s.02_100 <- filter(x_s.02, group_size == 100)
x_s.02_150 <- filter(x_s.02, group_size == 150)
x_s.02_200 <- filter(x_s.02, group_size == 200)

#create a dataframe to hold the population means
population_means_s.02 <- as.data.frame(matrix(nrow = 700, ncol = 7))
colnames(population_means_s.02) <- c("Year", "group_size_25", "group_size_50", "group_size_75", "group_size_100", "group_size_150", "group_size_200")

#find average populations through time for each setting of the group_size variable
for (i in 600:1299) {
  temp <- filter(x_s.02_25, Year == i)
  mean_25 <- mean(temp$Agents)
  temp <- filter(x_s.02_50, Year == i)
  mean_50 <- mean(temp$Agents)
  temp <- filter(x_s.02_75, Year == i)
  mean_75 <- mean(temp$Agents)
  temp <- filter(x_s.02_100, Year == i)
  mean_100 <- mean(temp$Agents)
  temp <- filter(x_s.02_150, Year == i)
  mean_150 <- mean(temp$Agents)
  temp <- filter(x_s.02_200, Year == i)
  mean_200 <- mean(temp$Agents)
  current_row <- c(i, mean_25, mean_50, mean_75, mean_100, mean_150, mean_200)
  population_means_s.02[i-599,] <- current_row
}

plot_means_s.02 <- ggplot(population_means_s.02) +
  theme_bw() +
  geom_line(aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(aes(x = Year, y = group_size_75, color = "green")) +
  geom_line(aes(x = Year, y = group_size_100, color = "orange")) +
  geom_line(aes(x = Year, y = group_size_150, color = "purple")) +
  geom_line(aes(x = Year, y = group_size_200, color = "red")) +
  labs(title = "Means by Group Size for s = .02, beta = .1, mu = .1", x="Year", y = "Number of Agents") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red','blue'='blue', "black" = "black", "purple" = "purple", "orange" = "orange", "green" = "green"), 
                      labels = c("Group Size = 25", "Group Size = 50", "Group Size = 75", "Group Size  = 100", "Group Size = 150", "Group Size  = 200")) +
  theme(legend.position=c(.1,.8))

plot_means_s.02

ggsave(plot_means_s.02, file = "plot_means_s.02.jpg", dpi = 600)

#add individual runs for group size = 100
plot <- ggplot() +
  theme_bw()



list_runs <- unique(x_s.02_100$run)
for (i in 1:length(list_runs)) {
  plot <- add_line(x_s.02_100, plot, i, "lightblue")
}

plot

#plot with individual runs for group size = 100, with means on top
plot_means_s.02_individual_runs <- plot +
  theme_bw() +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_75, color = "green")) +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_100, color = "orange")) +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_150, color = "purple")) +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_200, color = "red")) +
  labs(title = "Means by Group Size for s = .02, beta = .1, mu = .1; Individual runs for group size = 100", x="Year", y = "Number of Agents") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red','blue'='blue', "black" = "black", "purple" = "purple", "orange" = "orange", "green" = "green"), 
                      labels = c("Group Size = 25", "Group Size = 50", "Group Size = 75", "Group Size  = 100", "Group Size = 150", "Group Size  = 200")) +
  theme(legend.position=c(.1,.8))

plot_means_s.02_individual_runs

ggsave(plot_means_s.02_individual_runs, file = "plot_means_s.02_individual_runs.jpg", dpi = 600)


#filter for the runs with s = .05, beta = .9, and mu = .5
x_s.05 <- filter(x, s == .05) %>%
  filter(beta == .9) %>%
  filter(mu == .5)

#split by group size
x_s.05_25 <- filter(x_s.05, group_size == 25)
x_s.05_50 <- filter(x_s.05, group_size == 50)
x_s.05_75 <- filter(x_s.05, group_size == 75)
x_s.05_100 <- filter(x_s.05, group_size == 100)
x_s.05_150 <- filter(x_s.05, group_size == 150)
x_s.05_200 <- filter(x_s.05, group_size == 200)

#create a dataframe to hold the population means
population_means_s.05 <- as.data.frame(matrix(nrow = 700, ncol = 7))
colnames(population_means_s.05) <- c("Year", "group_size_25", "group_size_50", "group_size_75", "group_size_100", "group_size_150", "group_size_200")

#find average populations through time for each setting of the group_size variable
for (i in 600:1299) {
  temp <- filter(x_s.05_25, Year == i)
  mean_25 <- mean(temp$Agents)
  temp <- filter(x_s.05_50, Year == i)
  mean_50 <- mean(temp$Agents)
  temp <- filter(x_s.05_75, Year == i)
  mean_75 <- mean(temp$Agents)
  temp <- filter(x_s.05_100, Year == i)
  mean_100 <- mean(temp$Agents)
  temp <- filter(x_s.05_150, Year == i)
  mean_150 <- mean(temp$Agents)
  temp <- filter(x_s.05_200, Year == i)
  mean_200 <- mean(temp$Agents)
  current_row <- c(i, mean_25, mean_50, mean_75, mean_100, mean_150, mean_200)
  population_means_s.05[i-599,] <- current_row
}

plot_means_s.05 <- ggplot(population_means_s.05) +
  theme_bw() +
  geom_line(aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(aes(x = Year, y = group_size_75, color = "green")) +
  geom_line(aes(x = Year, y = group_size_100, color = "orange")) +
  geom_line(aes(x = Year, y = group_size_150, color = "purple")) +
  geom_line(aes(x = Year, y = group_size_200, color = "red")) +
  labs(title = "s = .05, beta = .9, mu = .5", x="Year", y = "Number of Agents") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red','blue'='blue', "black" = "black", "purple" = "purple", "orange" = "orange", "green" = "green"), 
                      labels = c("Group Size = 25", "Group Size = 50", "Group Size = 75", "Group Size  = 100", "Group Size = 150", "Group Size  = 200")) +
  theme(legend.position=c(.1,.8))

plot_means_s.05


plot_compare <- ggplot() +
  theme_bw() +
  geom_line(data = population_means_s.05, aes(x = Year, y = group_size_100, color = "black")) +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_50, color = "blue")) +
  labs(x="Year", y = "Number of Agents") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('blue'='blue', "black" = "black"), 
                      labels = c("Group Size  = 100, s = .05, beta = .9, s = .5", "Group Size = 50, s = .02, beta = .1, s = .1")) +
  theme(legend.position=c(.2,.85))
  
plot_compare

ggsave(plot_compare, file = "plot_compare.jpg", dpi = 600)

#plot mean population sizes plus all runs with a specific set of parameters
plot <- ggplot() +
  theme_bw()




# #adds all the runs for group size = 25
# list_runs <- unique(x_s.05_25$run)
# for (i in 1:length(list_runs)) {
#   plot <- add_line(x_s.05_25, plot, i, "light gray")
# }

# plot

#adds all the runs for group size = 50
list_runs <- unique(x_s.02_50$run)
for (i in 1:length(list_runs)) {
  plot <- add_line(x_s.02_50, plot, i, "lightblue")
}

plot

#adds all the runs for group size = 100
list_runs <- unique(x_s.05_100$run)
for (i in 1:length(list_runs)) {
  plot <- add_line(x_s.05_100, plot, i, "light gray")
}

plot


plot_compare2 <- plot +
  theme_bw() +
  geom_line(data = population_means_s.05, aes(x = Year, y = group_size_100, color = "black")) +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_50, color = "blue")) +
  labs(x="Year", y = "Number of Agents") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('blue'='blue', "black" = "black"), 
                      labels = c("Group Size  = 100, s = .05, beta = .9, s = .5", "Group Size = 50, s = .02, beta = .1, s = .1")) +
  theme(legend.position=c(.2,.85))

plot_compare2

ggsave(plot_compare2, file = "plot_compare2.jpg", dpi = 600)

x1290 <- filter(x, Year == 1290)

group_size_boxplot <- ggplot(x1290) +
  geom_boxplot(aes(x = as.factor(group_size), y = Agents), fill = "lightblue", outlier.colour="dark red", outlier.shape=18, outlier.size=3, outlier.fill="darkred") +
  theme_bw() +
  labs(title = "Boxplots of Population in Year 1290", x = "Group Size", y = "Number of Households")

ggsave(group_size_boxplot, file = "group_size_boxplot.jpg", dpi = 600)


s_boxplot <- ggplot(x1290) +
  geom_boxplot(aes(x = as.factor(s), y = Agents), fill = "lightblue", outlier.colour="dark red", outlier.shape=18, outlier.size=3, outlier.fill="darkred") +
  theme_bw() +
  labs(title = "Boxplots of Population in Year 1290", x = "s (tolerance for casualties)", y = "Number of Households")

s_boxplot

ggsave(s_boxplot, file = "s_boxplot.jpg", dpi = 600)

beta_boxplot <- ggplot(x1290) +
  geom_boxplot(aes(x = as.factor(beta), y = Agents), fill = "lightblue", outlier.colour="dark red", outlier.shape=18, outlier.size=3, outlier.fill="darkred") +
  theme_bw() +
  labs(title = "Boxplots of Population in Year 1290", x = "beta (tax on returns to the public goods game)", y = "Number of Households")

beta_boxplot

ggsave(beta_boxplot, file = "beta_boxplot.jpg", dpi = 600)

mu_boxplot <- ggplot(x1290) +
  geom_boxplot(aes(x = as.factor(mu), y = Agents), fill = "lightblue", outlier.colour="dark red", outlier.shape=18, outlier.size=3, outlier.fill="darkred") +
  theme_bw() +
  labs(title = "Boxplots of Population in Year 1290", x = "mu (pass-through tribute)", y = "Number of Households")

mu_boxplot

ggsave(mu_boxplot, file = "mu_boxplot.jpg", dpi = 600)


#draw random samples of 15 from x_s.02_50 and x_s.05_100
list_runs_x_s.02_50 <- unique(x_s.02_50$run)


sample_runs_s.02 <- as.data.frame(matrix(nrow = 700, ncol = 15))
sample_means_s.02 <- as.data.frame(matrix(nrow = 700, ncol = 26))
sample_means_s.02[,1] <- 600:1299

for (i in 1:25) {
  sample_list <- sample(list_runs_x_s.02_50, 15, replace = TRUE)
  for (j in 1:15) {
    temp <- filter(x_s.02_50, run == sample_list[j])
    sample_runs_s.02[,j] <- temp$Agents 
  }
  sample_means_s.02[,(i + 1)] <- rowMeans(sample_runs_s.02)
}





list_runs_x_s.05_100 <- unique(x_s.05_100$run)


sample_runs_s.05 <- as.data.frame(matrix(nrow = 700, ncol = 15))
sample_means_s.05 <- as.data.frame(matrix(nrow = 700, ncol = 26))
sample_means_s.05[,1] <- 600:1299

for (i in 1:25) {
  sample_list <- sample(list_runs_x_s.05_100, 15, replace = TRUE)
  for (j in 1:15) {
    temp <- filter(x_s.05_100, run == sample_list[j])
    sample_runs_s.05[,j] <- temp$Agents 
  }
  sample_means_s.05[,(i + 1)] <- rowMeans(sample_runs_s.05)
}


# plot the sample means of the n = 15 subsamples, plus the mean of the 110 runs
resampled_plot <- ggplot() +
  geom_line(data =sample_means_s.02, aes(x = V1, y = V2), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V3), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V4), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V5), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V6), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V7), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V8), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V9), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V10), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V11), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V12), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V13), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V14), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V15), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V16), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V17), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V18), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V19), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V20), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V21), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V22), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V23), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V24), color = "gray") +
  geom_line(data = sample_means_s.02, aes(x = V1, y = V25, color = "gray")) +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V2), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V3), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V4), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V5), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V6), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V7), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V8), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V9), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V10), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V11), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V12), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V13), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V14), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V15), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V16), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V17), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V18), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V19), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V20), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V21), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V22), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V23), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V24), color = "light blue") +
  geom_line(data = sample_means_s.05, aes(x = V1, y = V25, color = "light blue")) +
  geom_line(data = population_means_s.02, aes(x = Year, y = group_size_50, color = "black")) +
  geom_line(data = population_means_s.05, aes(x = Year, y = group_size_100, color = "blue")) +
  theme_bw() +
  labs(title = "Resampling Results", x="Year", y = "Number of Agents") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c("black" = "black",'blue'='blue', "gray" = "gray", "light blue" = "light blue"), 
                      labels = c("Mean of 100 runs, Group size = 100, s = .05, beta = .9, mu = .5", "Mean of 110 runs, Group Size = 50, s = .02, beta = .1", "Mean of sample of 15 runs", "Mean of sample of 15 runs")) +
  theme(legend.position=c(.2,.85))

resampled_plot
ggsave(resampled_plot, file = "resampled_plot.jpg", dpi = 600)
