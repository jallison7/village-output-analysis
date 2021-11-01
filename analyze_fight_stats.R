fight_data <- read.csv("condensed_fight_stats.csv")


#filter for the runs with s = .02, beta = .1, and mu = .1

fight_data_s.02 <- filter(fight_data, s == .02) %>%
  filter(beta == .1) %>%
  filter(mu == .1)

#split by group size
fight_data_s.02_25 <- filter(fight_data_s.02, group_size == 25)
fight_data_s.02_50 <- filter(fight_data_s.02, group_size == 50)
fight_data_s.02_75 <- filter(fight_data_s.02, group_size == 75)
fight_data_s.02_100 <- filter(fight_data_s.02, group_size == 100)
fight_data_s.02_150 <- filter(fight_data_s.02, group_size == 150)
fight_data_s.02_200 <- filter(fight_data_s.02, group_size == 200)


#create a data frame to store the annual mean  numbers of deaths from warfare
death_means_s.02 <- as.data.frame(matrix(nrow = 700, ncol = 7))
colnames(death_means_s.02) <- c("Year", "group_size_25", "group_size_50", "group_size_75", "group_size_100", "group_size_150", "group_size_200")

#find the annual mean numbers of deaths from warfare for different group sizes
for (i in 600:1299) {
  temp <- filter(fight_data_s.02_25, Year == i)
  mean_25 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.02_50, Year == i)
  mean_50 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.02_75, Year == i)
  mean_75 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.02_100, Year == i)
  mean_100 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.02_150, Year == i)
  mean_150 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.02_200, Year == i)
  mean_200 <- mean(temp$total_deaths)
  current_row <- c(i, mean_25, mean_50, mean_75, mean_100, mean_150, mean_200)
  death_means_s.02[i-599,] <- current_row

}

#replace Na with 0 
death_means_s.02[is.na(death_means_s.02)] <- 0

plot_death_means_s.02 <- ggplot(death_means_s.02) +
  theme_bw() +
  geom_line(aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(aes(x = Year, y = group_size_75, color = "green")) +
  geom_line(aes(x = Year, y = group_size_100, color = "orange")) +
  geom_line(aes(x = Year, y = group_size_150, color = "purple")) +
  geom_line(aes(x = Year, y = group_size_200, color = "red")) +
  xlim(600, 1298) +
  labs(title = "s = .02, beta = .1, mu = .1", x="Year", y = "Average Number of Deaths from Warfare") +
  scale_colour_manual(name = "", values =c('red'='red','orange'='orange', 'green' = 'green', 'blue' = 'blue', 'purple' = 'purple', 'black' = 'black'), 
                      labels = c("Group Size  = 25", "Group Size = 50", "Group Size = 75", "Group Size = 100", "Group Size = 150", "Group Size = 200")) +
  theme(legend.position=c(.1,.8))

plot_death_means_s.02


#filter for the runs with s = .05, beta = .9, and mu = .5
fight_data_s.05 <- filter(fight_data, s == .05) %>%
  filter(beta == .9) %>%
  filter(mu == .5)

#split by group size
fight_data_s.05_25 <- filter(fight_data_s.05, group_size == 25)
fight_data_s.05_50 <- filter(fight_data_s.05, group_size == 50)
fight_data_s.05_75 <- filter(fight_data_s.05, group_size == 75)
fight_data_s.05_100 <- filter(fight_data_s.05, group_size == 100)
fight_data_s.05_150 <- filter(fight_data_s.05, group_size == 150)
fight_data_s.05_200 <- filter(fight_data_s.05, group_size == 200)

#find the annual mean numbers of deaths from warfare for different group sizes
for (i in 600:1299) {
  temp <- filter(fight_data_s.05_25, Year == i)
  mean_25 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.05_50, Year == i)
  mean_50 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.05_75, Year == i)
  mean_75 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.05_100, Year == i)
  mean_100 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.05_150, Year == i)
  mean_150 <- mean(temp$total_deaths)
  temp <- filter(fight_data_s.05_200, Year == i)
  mean_200 <- mean(temp$total_deaths)
  current_row <- c(i, mean_25, mean_50, mean_75, mean_100, mean_150, mean_200)
  death_means_s.05[i-599,] <- current_row
  
}


#replace Na with 0 
death_means_s.05[is.na(death_means_s.05)] <- 0

plot_death_means_s.05 <- ggplot(death_means_s.05) +
  theme_bw() +
  geom_line(aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(aes(x = Year, y = group_size_75, color = "green")) +
  geom_line(aes(x = Year, y = group_size_100, color = "orange")) +
  geom_line(aes(x = Year, y = group_size_150, color = "purple")) +
  geom_line(aes(x = Year, y = group_size_200, color = "red")) +
  labs(title = "s = .05, beta = .9, mu = .5", x="Year", y = "Average Number of Deaths from Warfare") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red','orange'='orange', 'green' = 'green', 'blue' = 'blue', 'purple' = 'purple', 'black' = 'black'), 
                      labels = c("Group Size  = 25", "Group Size = 50", "Group Size = 75", "Group Size = 100", "Group Size = 150", "Group Size = 200")) +
  theme(legend.position=c(.1,.8))
  

plot_death_means_s.05


plot_compare <- ggplot() +
  theme_bw() +
  geom_line(data = death_means_s.02, aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(data = death_means_s.05, aes(x = Year, y = group_size_100, color = "purple"))  +
  labs(title = "Comparison of Means with Contrasting Settings", x="Year", y = "Average Number of Deaths from Warfare") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('purple'='purple','blue'='blue'), 
                      labels = c("Group Size = 50, s = .02, Beta = .1 mu = .1", "Group Size  = 100, s = .09, Beta = .9, mu = .5")) +
  theme(legend.position=c(.2,.9))

plot_compare

plot_compare2 <- ggplot() +
  theme_bw() +
  geom_line(data = death_means_s.02, aes(x = Year, y = group_size_50, color = "blue")) +
  geom_line(data = death_means_s.05, aes(x = Year, y = group_size_100, color = "purple"))  +
  geom_line(data = death_means_s.02, aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(data = death_means_s.05, aes(x = Year, y = group_size_200, color = "red"))  +
  labs(title = "Comparison of Means with Contrasting Settings", x="Year", y = "Average Number of Deaths from Warfare") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red','blue'='blue', "black" = "black", "purple" = "purple"), 
                      labels = c("Group Size = 25, s = .02, Beta = .1 mu = .1", "Group Size = 50, s = .02, Beta = .1 mu = .1", 
                                 "Group Size  = 100, s = .09, Beta = .9, mu = .5", "Group Size  = 200, s = .09, Beta = .9, mu = .5")) +
  theme(legend.position=c(.2,.9))

plot_compare2

plot_compare3 <- ggplot() +
  theme_bw() +
  geom_line(data = death_means_s.02, aes(x = Year, y = group_size_200, color = "black")) +
  geom_line(data = death_means_s.05, aes(x = Year, y = group_size_200, color = "red"))  +
  labs(title = "Comparison of Means with Group Size = 200, Contrasting Settings", x="Year", y = "Average Number of Deaths from Warfare") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red', "black" = "black"), 
                      labels = c("Group Size  = 200, s = .02, Beta = .1, mu = .1", "Group Size  = 200, s = .09, Beta = .9, mu = .5")) +
  theme(legend.position=c(.2,.9))

plot_compare3

plot_compare4 <- ggplot() +
  theme_bw() +
  geom_line(data = death_means_s.02, aes(x = Year, y = group_size_25, color = "black")) +
  geom_line(data = death_means_s.05, aes(x = Year, y = group_size_25, color = "red"))  +
  labs(title = "Comparison of Means with Group Size = 25, Contrasting Settings", x="Year", y = "Average Number of Deaths from Warfare") +
  xlim(600, 1298) +
  scale_colour_manual(name = "", values =c('red'='red', "black" = "black"), 
                      labels = c("Group Size  = 25, s = .02, Beta = .1, mu = .1", "Group Size  = 25, s = .09, Beta = .9, mu = .5")) +
  theme(legend.position=c(.2,.9))

plot_compare4

ggsave(plot_death_means_s.02, file = "mean_deaths_s.02.jpg", dpi = 600)
ggsave(plot_death_means_s.05, file = "mean_deaths_s.05.jpg", dpi = 600)
ggsave(plot_compare, file = "plot_compare.jpg", dpi = 600)
ggsave(plot_compare2, file = "plot_compare2.jpg", dpi = 600)
ggsave(plot_compare3, file = "plot_compare3.jpg", dpi = 600)
ggsave(plot_compare4, file = "plot_compare4.jpg", dpi = 600)



fight_data_1290 <- filter(fight_data, Year == 1290)

group_size_boxplot <- ggplot(fight_data_1290) +
  geom_boxplot(aes(x = as.factor(group_size), y = total_deaths), fill = "lightblue", outlier.colour="dark red", outlier.shape=18, outlier.size=3, outlier.fill="darkred") +
  theme_bw() +
  labs(title = "Boxplots of Deaths from Warfare in Year 1290", x = "Group Size", y = "Number of Deaths from Warfare")

group_size_boxplot

s_boxplot <- ggplot(fight_data_1290) +
  geom_boxplot(aes(x = as.factor(s), y = total_deaths), fill = "lightblue", outlier.colour="dark red", outlier.shape=18, outlier.size=3, outlier.fill="darkred") +
  theme_bw() +
  labs(title = "Boxplots of Deaths from Warfare in Year 1290", x = "s (tolerance for casualties)", y = "Number of Deaths from Warfare")

s_boxplot

beta_boxplot <- ggplot(fight_data_1290) +
  geom_boxplot(aes(x = as.factor(beta), y = total_deaths), fill = "lightblue", outlier.colour="dark red", outlier.shape=18, outlier.size=3, outlier.fill="darkred") +
  theme_bw() +
  labs(title = "Boxplots of Deaths from Warfare in Year 1290", x = "beta (tax on returns to the public goods game)", y = "Number of Deaths from Warfare")

beta_boxplot

mu_boxplot <- ggplot(fight_data_1290) +
  geom_boxplot(aes(x = as.factor(mu), y = total_deaths), fill = "lightblue", outlier.colour="dark red", outlier.shape=18, outlier.size=3, outlier.fill="darkred") +
  theme_bw() +
  labs(title = "Boxplots of Deaths from Warfare in Year 1290", x = "mu (pass-through tribute)", y = "Number of Deaths from Warfare")

mu_boxplot

ggsave(group_size_boxplot, file = "group_size_warfare_boxplot.jpg", dpi = 600)
ggsave(s_boxplot, file = "s_warfare_boxplot.jpg", dpi = 600)
ggsave(beta_boxplot, file = "beta_warfare_boxplot.jpg", dpi = 600)
ggsave(mu_boxplot, file = "mu_warfare_boxplot.jpg", dpi = 600)
