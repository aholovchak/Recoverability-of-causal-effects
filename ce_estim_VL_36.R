#' Causal Effect Estimator for VL_36
#'
#' Computes estimator for causal effect using parametric g-formula (variables MEMS_i included)
#'
#' @param c12_int continuous intervention on C12 (for both time points)
#' @param data_frame data to be used
#'
#' @return estimator for causal effect

estimator_data_36 <- function(c12_int, data_frame) {
  if (class(c12_int) != "numeric") {
    stop("C12 has to be numeric.")
  }
  if (c12_int < 0 || c12_int > 10) {
    warning("Estimator only meaningful for interventions between 0 and 10 mg/L")
  }
  n <- length(data_frame$Sex) # number of observations

  ##############################################################################

  # # estimate P(VL_0|C12_0)
  # 
  # data_mod_vl0 <- data_frame[, c(
  #   "VL_0", "C12_0"
  # )]
  # 
  # mod_vl0 <- glm(
  #   VL_0 ~ I(sqrt(C12_0)),
  #   family = binomial, data = na.omit(data_mod_vl0)
  # )
  # 
  # # simulate VL_0 (set C12_0 = a)
  # 
  # data_sim_vl0 <- data_mod_vl0
  # data_sim_vl0$C12_0 <- rep(c12_int, n) # overwrite intervention
  # 
  # data_frame$VL_0_sim <- rbinom(n, 1, predict(mod_vl0, type = "response", newdata = data_sim_vl0))

  ##############################################################################

  # # estimate P(CoMo_1|Age, Sex, CoMo_0, Weight_0, Genotype, Dose_0, EFV_0, VL_0_sim)
  # 
  # data_mod_como1 <- data_frame[, c(
  #   "CoMo_1", "Age", "Sex", "CoMo_0", "Weight_0", "Genotype", "Dose_0", "C12_0", "VL_0_sim"
  # )]
  # 
  # mod_como1 <- glm(
  #   CoMo_1 ~ Age + Sex + CoMo_0 + Weight_0 + Genotype + Dose_0 + I(sqrt(C12_0)) + VL_0_sim,
  #   family = binomial, data = na.omit(data_mod_como1)
  # )
  # 
  # # simulate CoMo_1 (set C12_0 = a)
  # 
  # data_sim_como1 <- data_mod_como1
  # data_sim_como1$C12_0 <- rep(c12_int, n) # overwrite intervention
  # 
  # data_frame$CoMo_1_sim <- rbinom(n, 1, predict(mod_como1, type = "response", newdata = data_sim_como1))

  ##############################################################################

  # estimate P(VL_1|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1, Weight_1, Dose_1, C12_1, VL_0)

  data_mod_vl1 <- data_frame[, c(
    "VL_1", "Age", "Sex", "CoMo_0", "Weight_0", "Genotype", "MEMS_1", "Weight_1", "Dose_1", "C12_1", "VL_0"
  )]

  mod_vl1 <- glm(
    VL_1 ~ Age + Sex + CoMo_0 + Weight_0 + Genotype + MEMS_1 + Weight_1 + Dose_1 + I(sqrt(C12_1)) + VL_0,
    family = binomial, data = na.omit(data_mod_vl1)
  )

  # simulate VL_1 (C12_1 = a)

  data_sim_vl1 <- data_mod_vl1
  #data_sim_vl1$C12_0 <- rep(c12_int, n) 
  data_sim_vl1$C12_1 <- rep(c12_int, n) # overwrite intervention

  data_frame$VL_1_sim <- rbinom(n, 1, predict(mod_vl1, type = "response", newdata = data_sim_vl1))

  ##############################################################################
  # estimate P(VL_2|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_1, C12_1, VL_0,
  #                 CoMo_1, VL_1_sim, MEMS_2,
  #                 Weight_2, Dose_2, C12_2)

  data_mod_vl2 <- data_frame[, c(
    "VL_2", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1", "VL_0", 
    "CoMo_1", "VL_1_sim","MEMS_2",
    "Weight_2", "Dose_2", "C12_2"
  )]

  mod_vl2 <- glm(
    VL_2 ~ Age + Sex + CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1 + Dose_1 + I(sqrt(C12_1)) + VL_0 + CoMo_1 + 
      VL_1_sim + MEMS_2 + Weight_2 + Dose_2 + I(sqrt(C12_2)),
    family = binomial, data = na.omit(data_mod_vl2)
  )

  # simulate VL_2 (set C12_1 = a, C12_2 = a)

  data_sim_vl2 <- data_mod_vl2
  data_sim_vl2$C12_1 <- rep(c12_int, n) # overwrite intervention
  data_sim_vl2$C12_2 <- rep(c12_int, n)

  data_frame$VL_2_sim <- rbinom(n, 1, predict(mod_vl2, type = "response", newdata = data_sim_vl2))

  ##############################################################################

  return(mean(na.omit(data_frame$VL_2_sim)))
}
