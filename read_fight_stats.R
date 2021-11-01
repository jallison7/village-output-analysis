library(tidyverse)
parameters <- read.csv("C:/Users/jra64/Documents/Presentations/CAA 2021/Hooper_parameters_all_master.csv")
dirpath <- "C:/Users/jra64/Documents/Presentations/CAA 2021/fight_stats"
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
  mutate(mu = run_parameters$MU) %>%
  mutate(total_deaths = (OffenseDeaths + DefenseDeaths))

#repeats the steps above for each of the runs in the "dirpath" folder, then appends 700 rows per run to x
for (i in 2:rows) {
  y <- read.csv(files[i])
  z <- (str_sub(files[i],-8, -5)) %>%
    str_remove("_") %>%
    str_remove("n") %>%
    str_remove("u") %>%
    as.numeric()
  run_parameters <- filter(parameters, RUN == z) 
  y <- mutate(y,run = z) %>%
    mutate(group_size = run_parameters$GROUP_SIZE) %>%
    mutate(s = run_parameters$S) %>%
    mutate(beta = run_parameters$BETA) %>%
    mutate(mu = run_parameters$MU)%>%
    mutate(total_deaths = (OffenseDeaths + DefenseDeaths))
  x <- rbind(x,y) 
}

#filter x so we're only looking at years with fight deaths
x1 <- filter(x, total_deaths > 0)

#condense the data so that each line compiles the total deaths for a year for one run
list_runs <- unique(x1$run)
 for (i in 1471:length(list_runs)) {
  temp_data <- filter(x1, run == list_runs[i])  
  list_years <- unique(temp_data$Year)  
  for (j in 1:length(list_years)) {
    z <-  filter(temp_data, Year == list_years[j])
    temp_line <- z[1,]
    temp_line$total_deaths <- sum(z$total_deaths)
    if (i == 1 && j == 1) {
          condensed_data <- temp_line 
    }  
    else  {
      condensed_data <- rbind(condensed_data, temp_line)
    }
 }

}
