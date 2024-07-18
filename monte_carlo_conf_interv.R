library(ggplot2)

# True \theta_{VL_84} values under intervention on C12
true_theta84 <- read.csv("true_theta84.csv")

# Available case analysis, G_{main}
{
  ac_main <- read.csv("ac_Gmain.csv")
  
  difference_main <- sweep(ac_main[,-1], 2, true_theta84$x, '-')
  mean_diff_main <- apply(difference_main, 2, mean)
  se_diff_main <- apply(difference_main, 2, sd) / sqrt(nrow(difference_main))
  
  lower_bound_main <- mean_diff_main - 2 * se_diff_main
  upper_bound_main <- mean_diff_main + 2 * se_diff_main
   
  (ac_main_ci <- data.frame(interv = seq(0, 10, 0.5), lower_bound_main, upper_bound_main))
}

# Available case analysis, G_{alt1}
{
  ac_alt1 <- read.csv("ac_Galt1.csv")
  
  difference_alt1 <- sweep(ac_alt1[,-1], 2, true_theta84$x, '-')
  mean_diff_alt1 <- apply(difference_alt1, 2, mean)
  se_diff_alt1 <- apply(difference_alt1, 2, sd) / sqrt(nrow(difference_alt1))
  
  lower_bound_alt1 <- mean_diff_alt1 - 2 * se_diff_alt1
  upper_bound_alt1 <- mean_diff_alt1 + 2 * se_diff_alt1
  
  (ac_alt1_ci <- data.frame(interv = seq(0, 10, 0.5), lower_bound_alt1, upper_bound_alt1))
}

##
##
##

# Multiple imputation, G_{main}
{
  mi_main <- readRDS("mi_Gmain.rds")
  mean_mi_main <- Reduce(`+`, mi_main) / length(mi_main)
  difference_mi_main <- sweep(mean_mi_main, 2, true_theta84$x, '-')
  
  mean_diff_mi_main <- apply(difference_mi_main, 2, mean)
  se_diff_mi_main <- apply(difference_mi_main, 2, sd) / sqrt(nrow(difference_mi_main))
  
  lower_bound_mi_main <- mean_diff_mi_main - 2 * se_diff_mi_main
  upper_bound_mi_main <- mean_diff_mi_main + 2 * se_diff_mi_main
  
  (mi_main_ci <- data.frame(interv = seq(0, 10, 0.5), lower_bound_mi_main, upper_bound_mi_main))
  
}

# Multiple imputation, G_{alt1}
{
  mi_alt1 <- readRDS("mi_Galt1.rds")
  mean_mi_alt1 <- Reduce(`+`, mi_alt1) / length(mi_alt1)
  difference_mi_alt1 <- sweep(mean_mi_alt1, 2, true_theta84$x, '-')
  
  mean_diff_mi_alt1 <- apply(difference_mi_alt1, 2, mean)
  se_diff_mi_alt1 <- apply(difference_mi_alt1, 2, sd) / sqrt(nrow(difference_mi_alt1))
  
  lower_bound_mi_alt1 <- mean_diff_mi_alt1 - 2 * se_diff_mi_alt1
  upper_bound_mi_alt1 <- mean_diff_mi_alt1 + 2 * se_diff_mi_alt1
  
  (mi_alt1_ci <- data.frame(interv = seq(0, 10, 0.5), lower_bound_mi_alt1, upper_bound_mi_alt1))
}