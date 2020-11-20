library(ggplot2)
library(gridExtra)
library(dplyr)

tabledrift <- read.csv("tableOfDriftValuesCalibration.csv")
tabledrift$trialTime = round(tabledrift$trialTime/1000, 2)
tabledrift$trial = factor(tabledrift$trial)
its_time_to_drift = tabledrift %>% filter(trialTime>=15) %>% filter(trialTime<=18)

simulate_one_trial <- function(idx){
  time_span = 3000
  sample_steps = 0:49
  num_samples = (time_span/length(sample_steps))
  time_stamps = 1:num_samples * num_samples
  drift_simulations = rnorm(time_span, 0, 0.13)
  sample_drifts = drift_simulations[sample_steps %% length(sample_steps) == 0]
  abs_drifts = cumsum(sample_drifts)
  # na.fail(dim(time_stamps))
  return(data.frame(cbind(trial =idx,  trialTime = time_stamps, posX = abs_drifts)))
}
n_trials = 1:50
simulated_trials = do.call(rbind,lapply(n_trials, simulate_one_trial))
simulated_trials$trialTime = simulated_trials$trialTime
simulated_trials$trial = factor(simulated_trials$trial)

num_bins = 50

p1 = ggplot(simulated_trials, aes(posX)) +
  geom_histogram(bins=100, fill="red")
p2 = ggplot(its_time_to_drift, aes(posX)) +
  geom_histogram(bins=100, fill="blue")
grid.arrange(p1, p2, ncol=2)