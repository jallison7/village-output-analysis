library(tidyverse)
# parameters <- read.csv("C:/Users/jra64/Documents/Presentations/CAA 2021/Hooper_parameters_all_master.csv")
dirpath <- "C:/Users/jra64/Documents/Presentations/EAA 2021/group_stats"
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
 x <-  mutate(x, run = z)


#repeats the steps above for each of the runs in the "dirpath" folder, then appends 700 rows per run to x
for (i in 2:rows) {
y <- read.csv(files[i])
z <- (str_sub(files[i],-8, -5)) %>%
  str_remove("_") %>%
  str_remove("n") %>%
  str_remove("u") %>%
  as.numeric()
  y <- mutate(y,run = z) 

x <- rbind(x,y) 
}


 write.csv(x, file = "group_stats.csv")

dirpath <- "C:/Users/jra64/Documents/Presentations/EAA 2021/dom_stats"
files <- (list.files(dirpath, full.names = T))
domrows <- length(files)
#reads the first file into a dataframe
domx <- read.csv(files[1])
#extracts the run number from the file name: first pulls the last four characters before the extension from the file name, 
#then gets rid of extra characters if they are present (which they are if the run number is less than 1000), then converts the string to a number
domz <- (str_sub(files[1],-8, -5)) %>%
  str_remove("_") %>%
  str_remove("n") %>%
  str_remove("u") %>%
  as.numeric()
domx <-  mutate(domx, run = domz)

#repeats the steps above for each of the runs in the "dirpath" folder, then appends 700 rows per run to x
for (i in 2:domrows) {
  domy <- read.csv(files[i])
  domz <- (str_sub(files[i],-8, -5)) %>%
    str_remove("_") %>%
    str_remove("n") %>%
    str_remove("u") %>%
    as.numeric()
  domy <- mutate(domy,run = domz) 
  domx <- rbind(domx, domy) 
}

write.csv(domx, file = "dom_stats.csv")
