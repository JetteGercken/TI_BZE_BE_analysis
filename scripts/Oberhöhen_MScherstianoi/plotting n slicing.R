library(dplyr)
library(magrittr)

trees <- data.frame(plot_ID = rep(1:3,c(30,40,50)), H_m = sample(1:100, 120, replace = T))

#calculate the proportions of each plot to be selected and write them in vector
#it is important to make sure the length of this is the same as the number of plots
#I chose some random values to test
proportions_plots <- c(0.4,0.5,0.3)

#create a vector to store final result(mean hight)
#set its length to number of unique plots
mean100top <- numeric(length(unique(trees$plot_ID)))

#make a for loop to do slice_max for each plot with it's corresponding proportion
# and write results in mean100top vector
for(id in unique(trees$plot_ID)){
  sliced_plot <- trees %>%
    filter(., plot_ID == id) %>%
    slice_max(.,H_m,prop = proportions_plots[id])
  mean100top[id] = mean(sliced_plot$H_m)
}
