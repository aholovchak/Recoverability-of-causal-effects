# Simulation study for estimation of \theta_{84} under G_{main}

# simulate m = 1000 data sets each of size n = 5000 based on the m-DAG,
# estimate causal effects for each of the data sets, and compute the mean
# at the end, compare estimated causal effects with the ground truth

library(simcausal) # for causal data simulation
library(ggplot2) # for visualization
library(doParallel) # for parallelization

source("ce_estim_VL_84.R")
source("missing_fct.R")

# generate a new data set each time and estimate causal effect

c12_int_values <- seq(0, 10, 0.5)
num_int <- length(c12_int_values) # number of interventions
int_names <- paste("C12.cont.", 1:num_int, sep = "") # intervention names

# Simulation loop
cl <- makeCluster(max(detectCores() - 1, 1)) # use all cores of computer , minus 1
registerDoParallel(cl) # start parallelization

runs <- 2

comb <- function(x, ...) {
  lapply(
    seq_along(x),
    function(i) c(x[[i]], lapply(list(...), function(y) y[[i]]))
  )
}


causal_loop <- foreach(
  r = 1:runs, .combine = "comb", .multicombine = TRUE,
  .init = list(list(), list()), .packages = "simcausal"
) %dopar% {
  setwd("/Users/aholovchak/Documents/missingness_DAGs/PAPER/aktuell")
  source("sim_data_Gmain.R") # input simulated m-DAG

  Odat <- simcausal::sim(DAG = Dset_main, n = 5000, rndseed = r, wide = TRUE)

  sim_data_six_msr <- Odat
  head(sim_data_six_msr)

  {
    sim_data_six_msr$Sex <- as.factor(sim_data_six_msr$Sex)
    sim_data_six_msr$Genotype <- as.factor(sim_data_six_msr$Genotype)
    sim_data_six_msr$VL_0 <- as.factor(sim_data_six_msr$VL_0)
    sim_data_six_msr$VL_1 <- as.factor(sim_data_six_msr$VL_1)
    sim_data_six_msr$VL_2 <- as.factor(sim_data_six_msr$VL_2)
    sim_data_six_msr$VL_3 <- as.factor(sim_data_six_msr$VL_3)
    sim_data_six_msr$VL_4 <- as.factor(sim_data_six_msr$VL_4)
    sim_data_six_msr$VL_5 <- as.factor(sim_data_six_msr$VL_5)
    names(sim_data_six_msr)[names(sim_data_six_msr) == "Age_0"] <- "Age"
    names(sim_data_six_msr)[names(sim_data_six_msr) == "Sex_0"] <- "Sex"
    names(sim_data_six_msr)[names(sim_data_six_msr) == "Genotype_0"] <- "Genotype"
  }

  # completely observed data
  data_complete <- sim_data_six_msr
  data_partial <- data_missing_wide(sim_data_six_msr)

  estim_values_complete <- numeric(num_int)
  estim_values_partial <- numeric(num_int)

  for (j in 1:num_int) {
    estim_values_complete[j] <- estimator_data_84(c12_int_values[j], data_frame = data_complete)
    estim_values_partial[j] <- estimator_data_84(c12_int_values[j], data_frame = data_partial)
  }

  list(estim_values_complete, estim_values_partial)
}

stopCluster(cl) # stop parallelization

data_ci_complete <- do.call(rbind.data.frame, causal_loop[[1]])
data_ci_partial <- do.call(rbind.data.frame, causal_loop[[2]])

names(data_ci_complete) <- int_names
names(data_ci_partial) <- int_names

# generate true causal effects from counterfactual data sets

t_end <- 5

# intervention on DAG given a continuous intervention
for (j in 0:num_int) {
  assign(paste("C12.cont.", j, sep = ""), c(node("C12", t = 1:t_end, distr = "rconst", const = NA))) # NA because indexing with [j] not possible
}

modify_int_const <- function(int, number) {
  int[[5]]$const <- number
  return(int)
}

for (j in 1:length(int_names)) { # trick to correctly assign continuous intervention by overwriting default
  for (k in 1:length(0:t_end)) {
    assign(paste("C12.cont.", j, sep = ""), lapply(get(paste("C12.cont.", j, sep = "")), modify_int_const, number = c12_int_values[j]))
  }
}

source("sim_data_main_upd.R") # input simulated TRUE DAG

for (j in 1:num_int) {
  compl_Dset_main <- compl_Dset_main + action(int_names[j], nodes = get(int_names[j]))
}

# simulate for the intervention
sim_cont <- simcausal::sim(DAG = compl_Dset_main, actions = int_names, n = 100000, rndseed = 241280)

Y_names <- colnames(sim_cont[[j]])[sort(c(grep("VL_", colnames(sim_cont[[j]]))))]
for (j in 1:num_int) {
  assign(paste("psi", c(1:num_int)[j], sep = ""), apply(sim_cont[[j]][, Y_names], 2, mean))
}

psi_true <- as.data.frame(cbind(
  matrix(unlist(mget(paste("psi", c(1:num_int), sep = ""))), ncol = 1),
  rep(0:(t_end), num_int), rep(c12_int_values, each = (t_end + 1))
))
colnames(psi_true)[1] <- "psi"
colnames(psi_true)[2] <- "times"
colnames(psi_true)[3] <- "int"
psi_true <- psi_true[order(psi_true$times), ]


psi_true_end <- psi_true[which(psi_true$times == "5"), "psi"] # fifth time point (week 84)
psi_true_end


# compute means of causal effects

means_complete <- colMeans(data_ci_complete)

means_incomplete <- colMeans(data_ci_partial)

k <- length(c12_int_values) # number of interventions of interest


data_final_vl_84 <- data.frame(
  efv_int = numeric(3 * num_int), caus_eff = numeric(3 * num_int),
  method = factor(3 * num_int)
)
data_final_vl_84$method <- c(rep("complete", num_int), rep("incomplete", num_int), rep("true", num_int))
data_final_vl_84$efv_int <- c(rep(c12_int_values, 3))
data_final_vl_84$caus_eff <- c(means_complete, means_incomplete, psi_true_end)

# MSE (complete vs true)
mean((data_final_vl_84[which(data_final_vl_84$method == "complete"), "caus_eff"] -
  data_final_vl_84[which(data_final_vl_84$method == "true"), "caus_eff"])^2)

# MSE (incomplete vs true)
mean((data_final_vl_84[which(data_final_vl_84$method == "incomplete"), "caus_eff"] -
  data_final_vl_84[which(data_final_vl_84$method == "true"), "caus_eff"])^2)

################################################################################
write.csv(data_final_vl_84, "final_vals_vl84_main_2miss.csv", row.names = FALSE)

# data_final_vl_84 <- read.csv("final_vals_vl84_main.csv")
################################################################################

# visualization of results

# line
ggplot(data_final_vl_84, aes(x = efv_int, y = caus_eff)) +
  geom_point(aes(col = method), alpha = 0.9, size = 3) +
  geom_line(aes(col = method), linetype = 2, size = 1.1, alpha = 0.65) +
  xlab("Efavirenz Mid−Dose Concentrations (mg/L),
       intervened on at each time point") +
  ylab("Viral Failure after 84 weeks (in %)") +
  scale_color_brewer(
    palette = "Dark2", labels = c("g-computation, no missing data", "g-computation, available cases, missing data", "true CRC"),
    name = "Estimated on"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal")

###
###
# For Monte Carlo confidence intervals in Appendix D.3
write.csv(data_ci_partial, "ac_Gmain.csv", row.names = FALSE)
