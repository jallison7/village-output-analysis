setwd("C:/Users/jra64/Documents/Presentations/EAA 2021")
library(tidyverse)
library(poweRlaw)
group_data <- read.csv("group_stats.csv")

dom_data <- read.csv("dom_stats.csv")


#create a list of the years that end demographic periods
list_years <- c(725, 800, 840, 880, 920, 980, 1020, 1060, 1100, 1140, 1180, 1225, 1260, 1280)
total_years <- length(list_years)



run_list <- unique(group_data$run)

total_runs <- length(run_list)

#create a dataframe to hold the statistics for each year and run
power_law_stats <- as.data.frame(matrix(nrow = 1526, ncol = 5))
colnames(power_law_stats) <- c("run", "year", "alpha", "test_statistic", "largest_territory")

#this is just a counter to keep track of which row in the data frame the power law stats need to go into
x <- 0

for (i in 1:total_runs) {
  groups_run <- filter(group_data, run == run_list[i])
  dom_run <- filter(dom_data, run == run_list[i])
  for (y in 1:total_years) {
    x <- x + 1
    groups <- filter(groups_run, Year  == list_years[y])
    dom <- filter(dom_run, Year  == list_years[y])
  

    #find territory and group sizes for dominant groups  
      dom_groups <- as.data.frame(unique(dom$DomGroup))
      colnames(dom_groups) <- "group"
      dom_groups <-  mutate(dom_groups, dom_size = NA, dom_territory = NA, complex_size = NA, complex_territory = NA)
  
 
      len <- length(dom_groups[,1])
      for (j in 1:len) {
        target_group = dom_groups[j,1]
        dom_groups[j,2] <- groups$Size[which(groups$Group == target_group)]
        dom_groups[j,3] <- groups$Territory_Size[which(groups$Group == target_group)]
      }
  
      dom_groups$complex_size <- dom_groups$dom_size
      dom_groups$complex_territory <- dom_groups$dom_territory

  #find territory and group sizes for subordinate groups   

      sub_groups <- dom %>%
      select(DomGroup, SubGroup) %>%
      mutate (size = NA, territory = NA, intermediate_level = FALSE, highest_group = DomGroup)
  
  
      len2 <- length(sub_groups[,1]) 


      for (j in 1:len2) {
        target_group <- sub_groups[j,2]
        sub_groups[j,3] <- groups$Size[which(groups$Group == target_group)]
        sub_groups[j,4] <- groups$Territory_Size[which(groups$Group == target_group)]
      }

  #find the highest level group for each simple group in a complex groups
      for (j in 1:len2) {
        target_group <- sub_groups[j,2]
        for (k in 1:len2) {
          if (sub_groups[k, 1] == target_group) {
          sub_groups[k, 5]  <- TRUE
          dom_groups <- dom_groups[!dom_groups$group == target_group, ]
          sub_groups[k,6] <- sub_groups[j, 1]
          }
        }
      }
 
#this sums subordinate and dominant group sizes and territories
    for (j in 1:len2) {
      target_group <- sub_groups[j,6]
      dom_groups[which(dom_groups$group == target_group),4] <- (sub_groups[j,3] +  dom_groups[which(dom_groups$group == target_group),4])
      dom_groups[which(dom_groups$group == target_group),5] <- (sub_groups[j,4] +  dom_groups[which(dom_groups$group == target_group),5])
    }

#find group sizes and territories for simple groups
    simple_group_size_distributions <- filter(groups, isComplex == "false") %>%
      select(Group, Size, Territory_Size)

#get the dom_groups dataframe in shape to add to size_distributions (more attention to naming variables, etc. earlier in the script would have helped)
    complex_group_size_distributions <- dom_groups %>%
      select(group, complex_size, complex_territory)
    colnames(complex_group_size_distributions) <- c("Group", "Size", "Territory_Size")


    size_distributions <- rbind(complex_group_size_distributions, simple_group_size_distributions)
    
#find power law and log normal models, set parameters, do tests
    
    power_model <- conpl$new(size_distributions$Territory_Size) 
    power_min <- estimate_xmin(power_model)
    power_model$setXmin(power_min)
    
    log_normal_model <- conlnorm$new(size_distributions$Territory_Size)
    log_normal_model$setXmin(power_min)
    est <- estimate_pars(log_normal_model)
    log_normal_model$setPars(est)
    
    comp <- compare_distributions(log_normal_model, power_model) 
    test_stat <- comp$test_statistic
    est <- estimate_pars(power_model)
    alpha <- est$pars
    large_territory <- max(size_distributions$Territory_Size)
    
    current_row <- c(run_list[i], list_years[y], alpha, test_stat, large_territory)
    power_law_stats[x,] <- current_row
    
  }
}


#somewhere in this loop are bugs that lead to error messages for some sets of data, so there are missing rows in the power_law_stats dataframe
# one error message looks likes this:
#Error in optim(par = theta_0, fn = negloglike, method = "L-BFGS-B", lower = c(-Inf,  : 
# non-finite value supplied by optim
# In addition: Warning messages:
#   1: In min(which(internal[["dat"]] >= (x - .Machine$double.eps^0.5))) :
#   no non-missing arguments to min; returning Inf
# 2: In min(which(internal[["dat"]] >= (x - .Machine$double.eps^0.5))) :
#   no non-missing arguments to min; returning Inf

# another one looks like this:
#   Error in check_ctn_data(dat) : 
#   Data should be strictly positive, i.e. no zeros.

write.csv(power_law_stats, file = "power law stats.csv")