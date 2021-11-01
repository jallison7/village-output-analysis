library(tidyverse)
parameters <- read.csv("C:/Users/jra64/Documents/Presentations/CAA 2021/Hooper_parameters_all_master.csv")
dirpath <- "C:/Users/jra64/Documents/Presentations/CAA 2021/system_stats"
files <- (list.files(dirpath, full.names = T))
rows <- length(files)
#reads the first file into a dataframe
x <- read.csv(files[1])
#extracts the run number from the file name: first pulls the last four characters before the extension from the file name, 
#then gets rid of extra characters if they are present (which they are if the run number is less than 1000), then converts the string to a number
z <- (str_sub(files[1],-8, -5)) %>%
 str_remove("_") %>%
 str_remove("n") %>%
 str_remove("u") %>%
 as.numeric()
#filters the parameters dataframe by the run number, then adds the run parameters to each of the 700 rows in x
run_parameters <- filter(parameters, RUN == z) 
x <-  mutate(x, run = z) %>%
  mutate(group_size = run_parameters$GROUP_SIZE) %>%
  mutate(s = run_parameters$S) %>%
  mutate(beta = run_parameters$BETA) %>%
  mutate(mu = run_parameters$MU)

#repeats the steps above for each of the runs in the "dirpath" folder, then appends 700 rows per run to x
# for (i in 2:rows) {
# y <- read.csv(files[i])
# z <- (str_sub(files[i],-8, -5)) %>%
#   str_remove("_") %>%
#   str_remove("n") %>%
#   str_remove("u") %>%
#   as.numeric()
# run_parameters <- filter(parameters, RUN == z) 
# y <- mutate(y,run = z) %>%
#   mutate(group_size = run_parameters$GROUP_SIZE) %>%
#   mutate(s = run_parameters$S) %>%
#   mutate(beta = run_parameters$BETA) %>%
#   mutate(mu = run_parameters$MU)
# x <- rbind(x,y) 
# }
# 
# write.csv(x, file = "system_stats.csv")

x <- read.csv("system_stats.csv")

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


#to plot population means by group_size (needs better labeling)

plot <- ggplot(population_means) +
  theme_bw() +
  geom_line(aes(x = Year, y = group_size_25), color = "red") +
  geom_line(aes(x = Year, y = group_size_50), color = "orange") +
  geom_line(aes(x = Year, y = group_size_75), color = "green") +
  geom_line(aes(x = Year, y = group_size_100), color = "blue") +
  geom_line(aes(x = Year, y = group_size_150), color = "purple") +
  geom_line(aes(x = Year, y = group_size_200), color = "black") +
  labs(x="Year", y = "Number of Agents")

plot


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
  geom_line(aes(x = Year, y = group_size_25), color = "red") +
  geom_line(aes(x = Year, y = group_size_50), color = "orange") +
  geom_line(aes(x = Year, y = group_size_75), color = "green") +
  geom_line(aes(x = Year, y = group_size_100), color = "blue") +
  geom_line(aes(x = Year, y = group_size_150), color = "purple") +
  geom_line(aes(x = Year, y = group_size_200), color = "black") +
  labs(title = "s = .02, beta = .1, mu = .1", x="Year", y = "Number of Agents")

plot_means_s.02



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
  geom_line(aes(x = Year, y = group_size_25), color = "red", size = 1.1) +
  geom_line(aes(x = Year, y = group_size_50), color = "orange", size = 1.1) +
  geom_line(aes(x = Year, y = group_size_75), color = "green", size = 1.1) +
  geom_line(aes(x = Year, y = group_size_100), color = "blue", size = 1.1) +
  geom_line(aes(x = Year, y = group_size_150), color = "purple", size = 1.1) +
  geom_line(aes(x = Year, y = group_size_200), color = "black", size = 1.1) +
  labs(title = "s = .05, beta = .9, mu = .9", x="Year", y = "Number of Agents")

plot_means_s.05





#plot mean population sizes plus all runs with a specific set of parameters
plot <- ggplot() +
  theme_bw()


add_line <- function(data, plot, i, clr) {
list_runs <- unique(data$run)
data <- filter(data, run == list_runs[i])
plot <- plot + 
geom_line(data = data, aes(x = Year, y = Agents), color = clr)  
return(plot)  
}

#adds all the runs for group size = 25
list_runs <- unique(x_s.05_25$run)
  for (i in 1:length(list_runs)) {
    plot <- add_line(x_s.05_25, plot, i, "light gray")
  }

plot

#adds all the runs for group size = 50
list_runs <- unique(x_s.05_50$run)
for (i in 1:length(list_runs)) {
  plot <- add_line(x_s.05_50, plot, i, "orange")
}

plot

#adds all the runs for group size = 100
list_runs <- unique(x_s.05_100$run)
for (i in 1:length(list_runs)) {
  plot <- add_line(x_s.05_100, plot, i, "lightblue")
}

plot

#adds the means for group size = 25 and 100

plot <- plot +
    geom_line(data = population_means_s.05, aes(x = Year, y = group_size_25), color = "black", size = 1.1) +
    geom_line(data = population_means_s.05, aes(x = Year, y = group_size_50), color = "orange", size = 1.1) +
    geom_line(data = population_means_s.05, aes(x = Year, y = group_size_75), color = "green", size = 1.1) +
    geom_line(data = population_means_s.05, aes(x = Year, y = group_size_100), color = "blue", size = 1.1) +
    # geom_line(data = population_means_s.05, aes(x = Year, y = group_size_150), color = "purple", size = 1.1) +
    # geom_line(data = population_means_s.05, aes(x = Year, y = group_size_200), color = "black", size = 1.1) +
    labs(title = "group size = 25 and 100, s = .05, beta = .9, mu = .9", x="Year", y = "Number of Agents")
    
plot
