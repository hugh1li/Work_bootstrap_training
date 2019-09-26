#'@ pollutant_col_id: the column id of your pollutant
#'@ distf is your dataframe
#'@ converging_or_not: the difference between the final simulated median and population median (multipled by 100)
#'@ deviation: we use the threshold to tell if final simulations converge to the selected population metric
#'@ n_sim: number of simulations
#* note since this simulation run without replacement. It is wrong to call it bootstrapping...

montecarlo_simulation <- function(pollutant_col_id, distf, deviation = 0.25, n_sim = 100) {
  # every point in the final plot is the median of simulated samples
  
  # import packges
  # library(tidyverse)
  # library(lubridate)
  # library(reshape2) # we will use the 'melt' function
  # library(cumstats)
  # library(ggpubr)
  
  pollutant = names(distf)[pollutant_col_id] # get the name of col id 
  
  target <- distf %>% select(DateTime, pollutant_col_id) %>% mutate(Dates = lubridate::date(DateTime)) %>% na.omit() # filter NA rows
  
  days <- unique(target$Dates) # get unique dates
  n_days <- length(days) # how many?
  pop_target <- target[[pollutant]] # this gets all the concentrations
  
  target1 <- target %>% select(Dates, pollutant)
  
  # this function draw one data point from each unique day, and then calculate median for each number of days
  random_add_median <- function(i) {
    target1 %>% group_by(Dates) %>% sample_n(1) %>% ungroup() %>% slice(sample(1:n())) %>% transmute(cum_median = cumstats::cummedian(get(pollutant))) %>% unlist(use.names = FALSE)
  }
  
  target_sample <- map(seq(n_sim), ~random_add_median(i))
  
  names(target_sample) <- seq(1, n_sim)
  
  ymax_suggested <- target_sample %>% purrr::map(quantile, 0.99) # this is for plotting. Know the approximate max in y axis.
  ymax_s1 <- max(unlist(ymax_suggested))
  
  # code below is to make the dataframe nicely formated based on Hadley Wickham.
  target2 <- bind_cols(target_sample) # equals bind_rows coz your tbl_df does not have same col names
  target3 <- reshape2::melt(target2)
  target4 <- as_tibble(t(target2))
  colnames(target4) <- seq(1:n_days)
  
  # calculate 2.5% and 97.5% of simulated samples
  q2.5 <- target4 %>% map_dbl(quantile, 0.025) %>% melt() %>% rowid_to_column()
  q97.5 <- target4 %>% map_dbl(quantile, 0.975) %>% melt() %>% rowid_to_column()
  target5 <- melt(target4)
  
  # median here for population
  ref_median <- median(pop_target)
  # standard_error <- sd(target)/ sqrt(length(target))
  ref_lower <- ref_median * (1- deviation)
  ref_high <- ref_median * (1 + deviation)
  
  plot1 <-
    ggplot(target5, aes(x = as.numeric(variable), y = value)) + geom_point(alpha = 0.1) + geom_hline(yintercept = ref_lower, color = "red", linetype = 3, size = 2) + geom_hline(yintercept = ref_high, color = "red", linetype = 3, size = 2) + geom_hline(yintercept = ref_median, color = "red", size = 2) + ggpubr::theme_pubr(x.text.angle = 90) + labs(y = pollutant, x = "Number of unique days", title = pollutant) + scale_x_continuous(breaks = seq(0, n_days, 10)) + geom_line(data = q2.5, aes(x = rowid, y = value), size = 2, color = "blue") +  geom_line(data = q97.5, aes(x = rowid, y = value), size = 2, color = "blue")
  
  ggsave(paste0(pollutant, deviation, ".pdf"))
  ggsave(paste0(pollutant, deviation, ".png"))
  
}