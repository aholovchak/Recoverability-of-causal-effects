#' Estimation of \theta_{VL_84} under G_{alt2} based on multiple imputation

library(dplyr)
library(ggplot2)
library(mgcv)
library(parallel)
library(doParallel)
library(RColorBrewer)
library(Amelia)

source("sim_data_Galt2.R")
source("missing_fct.R")

Dset <- Dset_altern2

Odat <- simcausal::sim(DAG = Dset, n = 5000, rndseed = 01111920, wide = T)
head(Odat)
summary(Odat)

Odat_cols <- c(
  "ID", "Sex_0", "Genotype_0", "Age_0", "NRTI_0", "SES_0",
  "Weight_0", "CoMo_0", "VL_0", "TI_0", "MV_0",
  "M.weight_0", "M.VL_0",
  "Weight_1", "CoMo_1", "Dose_1", "C12_1", "VL_1", "TI_1", "MEMS_1", "MV_1",
  "M.weight_1", "M.MEMS_1", "M.C12_1", "M.VL_1",
  "Weight_2", "CoMo_2", "Dose_2", "C12_2", "VL_2", "TI_2", "MEMS_2", "MV_2",
  "M.weight_2", "M.MEMS_2", "M.C12_2", "M.VL_2",
  "Weight_3", "CoMo_3", "Dose_3", "C12_3", "VL_3", "TI_3", "MEMS_3", "MV_3",
  "M.weight_3", "M.MEMS_3", "M.C12_3", "M.VL_3",
  "Weight_4", "CoMo_4", "Dose_4", "C12_4", "VL_4", "TI_4", "MEMS_4", "MV_4",
  "M.weight_4", "M.MEMS_4", "M.C12_4", "M.VL_4",
  "Weight_5", "CoMo_5", "Dose_5", "C12_5", "VL_5", "TI_5", "MEMS_5", "MV_5",
  "M.weight_5", "M.MEMS_5", "M.C12_5", "M.VL_5"
)

# Reorder the columns
Odat <- Odat[Odat_cols]


# in long format
Odat2 <- simcausal::sim(DAG = Dset, n = 5000, rndseed = 01111920, wide = F)
head(Odat2)

Odat2_cols <- c(
  "ID", "Sex", "Genotype", "Age", "NRTI", "Weight",
  "CoMo", "Dose", "MEMS", "C12", "VL", "SES", "TI", "MV",
  "M.weight", "M.MEMS", "M.C12", "M.VL"
)

Odat2 <- Odat2[Odat2_cols]

# If NA in missingness indicators
Odat2$M.weight <- ifelse(is.na(Odat2$M.weight), 0, Odat2$M.weight)
Odat2$M.MEMS <- ifelse(is.na(Odat2$M.MEMS), 0, Odat2$M.MEMS)
Odat2$M.C12 <- ifelse(is.na(Odat2$M.C12), 0, Odat2$M.C12)
Odat2$M.VL <- ifelse(is.na(Odat2$M.VL), 0, Odat2$M.VL)


# set values as missing (for both, wide and long format, correspondingly)

Odat <- data_missing_wide(Odat)
summary(Odat)

Odat2 <- data_missing_long(Odat2)
summary(Odat2)


# Imputation in Amelia in Long format

# Logic: we do not have captured SES, or TI, or MV - as in the real data
# we still impute with what is available. Data are MNAR because prob(missingness) depends on unobserved variables
# important: CC with this setup should still be valid  -@Anastasiia: this is what you did in the MA, right?
imp_data <- subset(Odat2, select = -c(M.weight, M.MEMS, M.C12, M.VL, SES, TI, MV))
time_function <- function(mat) {
  mat$visit <- seq(1:nrow(mat)) - 1
  return(mat)
} # function to add time/visit indicator
imp_data <- unsplit(lapply(split(imp_data, imp_data$ID), time_function), imp_data$ID)

M <- 5

imputation <- amelia(imp_data, # data
  m = M, # number of imputations
  p2s = 2, # print output? I used extensive output (suppress with p2s=0)
  cs = "ID", # cross section indicator variable
  ts = "visit", # time variable
  polytime = 3, splinetime = 3, # setup for complexity of time modeling per cross-section unit
  intercs = FALSE, # interaction of cross-section and time unit, ideally TRUE; but for this short-follow-up FALSE
  lags = c("Weight", "CoMo", "Dose", "MEMS", "C12", "VL"), # past variables to consider for imputation, e.g. use weight[t-1] for weight[t]
  leads = c("Weight", "CoMo", "Dose", "MEMS", "C12", "VL"), # future variables to consider for imputation, e.g. use weight[t+1] for weight[t]; controversial, but practically useful
  noms = c("Sex", "Genotype", "NRTI", "CoMo", "Dose", "MEMS", "VL"), # nominal/categorical variables
  logs = NULL, # log-transformation
  empri = 0.05 * nrow(imp_data), # empirical prior, relevant for convergence
  bounds = matrix(c(
    4, 0, 3, # matrix with logical bounds for continuous variables: IMPORTANT!(e.g., 4th variable, log(age), has limits of 0 and 3)
    6, 2, 4,
    10, 0, 35
  ), ncol = 3, byrow = T)
)

# check diagnostics (check manual=JSS article if you like)
plot(imputation) # Diagnostic 1:  problem is that with complex distribution of C12 and weight imputations are not perfect (see shape of imputed and original values)
# there is no solution to this with this or other packages
# Option 1: leave as it is
# Alternatively, we could change the DGP for this simulation (normal, not truncated normal)
# Or: no missing data in C12 and weight
disperse(imputation, dims = 1, m = 10) # Diagnostic 2: convergence very good
disperse(imputation, dims = 2, m = 10) # Diagnostic 3: convergence very good
tscsPlot(imputation, cs = 6, var = "Weight") # Diagnostic 4: check if time trend is o.k.: can be done with many units; unit 6 looks o.k.

# reshape in wide format
for (m in 1:M) {
  assign(
    paste0("idata.", m),
    reshape(imputation$imputations[[m]],
      v.names = c("Weight", "CoMo", "Dose", "MEMS", "C12", "VL"),
      timevar = "visit", idvar = "ID", direction = "wide", sep = "_"
    )
  )
}



results_imp <- as.data.frame(matrix(0, nrow = 21, ncol = 6))

c12_int_values <- seq(0, 10, 0.5)
results_imp$V1 <- c12_int_values


source("ce_estim_VL_84.R")

for (i in 1:M) {
  data <- get(paste("idata.", i, sep = ""))

  {
    data$Sex <- as.factor(data$Sex)
    data$Genotype <- as.factor(data$Genotype)
    data$VL_0 <- as.factor(data$VL_0)
    data$VL_1 <- as.factor(data$VL_1)
    data$VL_2 <- as.factor(data$VL_2)
    data$VL_3 <- as.factor(data$VL_3)
    data$VL_4 <- as.factor(data$VL_4)
    data$VL_5 <- as.factor(data$VL_5)
  }
  summary(data)

  # intervention sequence

  c12_int_values <- seq(0, 10, 0.5)
  k <- length(c12_int_values) # number of interventions of interest
  estim_values_chapas3 <- numeric(k)

  ################################################################################
  # Estimation, mean over multiple seeds
  ################################################################################

  # For loop (repeat estimation runs times, and compute the mean)

  cl <- makeCluster(max(detectCores() - 1, 1)) # use all cores of computer , minus 1
  registerDoParallel(cl) # start parallelization

  ## INCREASE number of runs!
  runs <- 1000
  estim_values_chapas3_runs <- numeric(k)

  result <- foreach(i = 1:runs, .combine = rbind) %dopar% {
    set.seed(01102022 + i)
    for (j in 1:k) {
      estim_values_chapas3_runs[j] <- estimator_data_84(c12_int_values[j], data_frame = data)
    }
    return(estim_values_chapas3_runs)
  }

  stopCluster(cl) # stop parallelization

  data_final_vl84 <- data.frame(efv_int = c12_int_values, caus_eff = colMeans(result))

  results_imp[, i + 1] <- data_final_vl84$caus_eff
}
################################################################################

results_imp$caus_eff_means <- rowMeans(subset(results_imp, select = 2:6))
names(results_imp)[1] <- c("EFV_interv")
names(results_imp)[2:(length(results_imp[1, ]) - 1)] <- paste("run", 1:(length(results_imp[1, ]) - 2), sep = "")

ggplot(results_imp, aes(x = EFV_interv, y = caus_eff_means)) +
  geom_point(col = "coral", size = 1.5) +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Efavirenz Mid-Dose Concentrations (mg/L),
       intervened on at each time point") +
  ylab("Viral Failure after 84 weeks (in %)") +
  ylim(0, 0.8) +
  theme_minimal()

write.csv(results_imp, file = "results_imp_vl84_altern2_2miss.csv")
