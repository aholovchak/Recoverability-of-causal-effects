#' Causal Effect Estimator for VL_84
#'
#' Computes estimator for causal effect using parametric g-formula (variables MEMS_i included)
#'
#' @param c12_int continuous intervention on C12 (for both time points)
#' @param data_frame data to be used
#'
#' @return estimator for causal effect

estimator_data_84 <- function(c12_int, data_frame) {
  if (class(c12_int) != "numeric") {
    stop("C12 has to be numeric.")
  }
  if (c12_int < 0 || c12_int > 10) {
    warning("Estimator only meaningful for interventions between 0 and 10 mg/L")
  }
  n <- length(data_frame$Sex) # number of observations

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
  data_sim_vl1$C12_1 <- rep(c12_int, n) # overwrite intervention

  data_frame$VL_1_sim <- rbinom(n, 1, predict(mod_vl1, type = "response", newdata = data_sim_vl1))

  ##############################################################################

  # # estimate P(MEMS_2|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1, VL_0_sim, CoMo_1_sim)
  # 
  # data_mod_mems2 <- data_frame[, c(
  #   "MEMS_2", "Age", "Sex", "CoMo_0", "Weight_0", "Genotype", "MEMS_1", "VL_0", "CoMo_1"
  # )]
  # 
  # mod_mems2 <- glm(
  #   MEMS_2 ~ Age + Sex + CoMo_0 + Weight_0 + Genotype + MEMS_1 + VL_0 + CoMo_1,
  #   family = binomial, data = na.omit(data_mod_mems2)
  # )
  # 
  # # simulate MEMS_2 (set C12_1 = a)
  # 
  # data_sim_mems2 <- data_mod_mems2
  # data_sim_mems2$C12_1 <- rep(c12_int, n) # overwrite intervention
  # 
  # data_frame$MEMS_2_sim <- rbinom(n, 1, predict(mod_mems2, type = "response", newdata = data_sim_mems2))

  ##############################################################################

  # # estimate P(Weight_2|Age, Sex, CoMo_0, Weight_0, Genotype, Weight_1, Dose_0, C12_0, VL_0_sim, CoMo_1_sim)
  # 
  # data_mod_weight2 <- data_frame[, c(
  #   "Weight_2", "Age", "Sex", "CoMo_0", "Weight_0", "Genotype", "Weight_1", "Dose_0", "C12_0", "VL_0_sim", "CoMo_1_sim"
  # )]
  # 
  # mod_weight2 <- lm(
  #   Weight_2 ~ Age + Sex + CoMo_0 + Weight_0 + Genotype + Weight_1 + Dose_0 + I(sqrt(C12_0)) + VL_0_sim + CoMo_1_sim,
  #   data = na.omit(data_mod_weight2)
  # )
  # 
  # # simulate MEMS_2 (set C12_0 = a)
  # 
  # data_sim_weight2 <- data_mod_weight2
  # data_sim_weight2$C12_0 <- rep(c12_int, n) # overwrite intervention
  # 
  # data_frame$Weight_2_sim <- rnorm(n, mean = predict(mod_weight2, newdata = data_sim_weight2),
  #                                  sd = summary(mod_weight2)$sigma)

  ##############################################################################
  # estimate P(VL_2|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_1, C12_1, VL_0,
  #                 CoMo_1, VL_1_sim, MEMS_2,
  #                 Weight_2, Dose_2, C12_2)

  data_mod_vl2 <- data_frame[, c(
    "VL_2", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1",
    "VL_0", "CoMo_1", "VL_1_sim", "MEMS_2",
    "Weight_2", "Dose_2", "C12_2"
  )]

  mod_vl2 <- glm(
    VL_2 ~ Age + Sex + CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1 + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim + MEMS_2 + Weight_2 + Dose_2 +
      I(sqrt(C12_2)),
    family = binomial, data = na.omit(data_mod_vl2)
  )

  # simulate VL_2 (set C12_1 = a, C12_2 = a)

  data_sim_vl2 <- data_mod_vl2
  data_sim_vl2$C12_1 <- rep(c12_int, n) # overwrite intervention
  data_sim_vl2$C12_2 <- rep(c12_int, n)

  data_frame$VL_2_sim <- rbinom(n, 1, predict(mod_vl2, type = "response", newdata = data_sim_vl2))

  ##############################################################################
  # estimate P(CoMo_2|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_0, Dose_1, C12_0, C12_1, VL_0,
  #                 CoMo_1_sim, VL_1_sim)

  data_mod_como2 <- data_frame[, c(
    "CoMo_2", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1",
    "VL_0", "CoMo_1", "VL_1_sim"
  )]

  mod_como2 <- glm(
    CoMo_2 ~ Age + Sex + CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1  + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim,
    family = binomial, data = na.omit(data_mod_como2)
  )

  # simulate CoMo_2 (set C12_1 = a)

  data_sim_como2 <- data_mod_como2
  data_sim_como2$C12_1 <- rep(c12_int, n) # overwrite intervention

  data_frame$CoMo_2_sim <- rbinom(n, 1, predict(mod_como2, type = "response", newdata = data_sim_como2))

  ##############################################################################
  # estimate P(MEMS_3|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_0, Dose_1, C12_0, C12_1, VL_0_sim,
  #                 CoMo_1, VL_1_sim, MEMS_2_sim, CoMo_2_sim)

  data_mod_mems3 <- data_frame[, c(
    "MEMS_3", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1", "VL_0", "CoMo_1", 
    "VL_1_sim", "MEMS_2", "CoMo_2_sim"
  )]

  mod_mems3 <- glm(
    MEMS_3 ~ Age + Sex +  CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1  + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim + MEMS_2 + CoMo_2_sim,
    family = binomial, data = na.omit(data_mod_mems3)
  )

  # simulate MEMS_3 (set C12_0 = a, C12_1 = a)

  data_sim_mems3 <- data_mod_mems3  
  data_sim_mems3$C12_1 <- rep(c12_int, n)

  data_frame$MEMS_3_sim <- rbinom(n, 1, predict(mod_mems3, type = "response", newdata = data_sim_mems3))

  ##############################################################################
  # estimate P(Weight_3|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_0, Dose_1, C12_0, C12_1, VL_0_sim,
  #                 CoMo_1, VL_1_sim, MEMS_2_sim, CoMo_2_sim)

  data_mod_weight3 <- data_frame[, c(
    "Weight_3", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1",
    "VL_0", "CoMo_1", "VL_1_sim", "MEMS_2", "CoMo_2_sim"
  )]

  mod_weight3 <- lm(
    Weight_3 ~ Age + Sex +  CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1 + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim + MEMS_2 + CoMo_2_sim,
    data = na.omit(data_mod_weight3)
  )

  # simulate MEMS_3 (set C12_1 = a)

  data_sim_weight3 <- data_mod_weight3
  data_sim_weight3$C12_1 <- rep(c12_int, n)

  data_frame$Weight_3_sim <- rnorm(n, mean = predict(mod_weight3, newdata = data_sim_weight3),
                                   sd = summary(mod_weight3)$sigma)

  ##############################################################################
  # estimate P(CoMo_3|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_1, C12_1, VL_0,
  #                 CoMo_1, VL_1_sim, MEMS_2, Weight_2, CoMo_2_sim,
  #                 Dose_2, C12_2, VL_2_sim)

  data_mod_como3 <- data_frame[, c(
    "CoMo_3", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1", "VL_0", "CoMo_1", 
    "VL_1_sim", "MEMS_2", "Weight_2", "CoMo_2_sim",
    "Dose_2", "C12_2", "VL_2_sim"
  )]

  mod_como3 <- glm(
    CoMo_3 ~ Age + Sex +  CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1 + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim + MEMS_2 + Weight_2 + CoMo_2_sim +
      Dose_2 + I(sqrt(C12_2)) + VL_2_sim,
    family = binomial, data = na.omit(data_mod_como3)
  )

  # simulate CoMo_3 (set C12_1 = a, C12_2 = a)

  data_sim_como3 <- data_mod_como3
  data_sim_como3$C12_1 <- rep(c12_int, n)
  data_sim_como3$C12_2 <- rep(c12_int, n)


  data_frame$CoMo_3_sim <- rbinom(n, 1, predict(mod_como3, type = "response", newdata = data_sim_como3))

  ##############################################################################
  # estimate P(VL_3|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_1, C12_1, VL_0,
  #                 CoMo_1, VL_1_sim, MEMS_2, Weight_2, CoMo_2_sim,
  #                 Dose_2, MEMS_3_sim, Weight_3_sim,
  #                 C12_2, Dose_3, VL_2_sim, C12_3)

  data_mod_vl3 <- data_frame[, c(
    "VL_3", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1", "Dose_1", "C12_1",
    "VL_0", "CoMo_1", "VL_1_sim", "MEMS_2", "Weight_2", "CoMo_2_sim",
    "Dose_2", "MEMS_3_sim", "Weight_3_sim", "C12_2", "Dose_3", "VL_2_sim", "C12_3"
  )]

  mod_vl3 <- glm(
    VL_3 ~ Age + Sex +  CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1 + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim + MEMS_2 + Weight_2 + CoMo_2_sim +
      Dose_2 + MEMS_3_sim + Weight_3_sim + I(sqrt(C12_2)) + Dose_3 + 
      VL_2_sim + I(sqrt(C12_3)),
    family = binomial, data = na.omit(data_mod_vl3)
  )

  # simulate VL_3 (set C12_1 = a, C12_2 = a, C12_3 = a)

  data_sim_vl3 <- data_mod_vl3
  data_sim_vl3$C12_1 <- rep(c12_int, n)
  data_sim_vl3$C12_2 <- rep(c12_int, n)
  data_sim_vl3$C12_3 <- rep(c12_int, n)

  data_frame$VL_3_sim <- rbinom(n, 1, predict(mod_vl3, type = "response", newdata = data_sim_vl3))

  ##############################################################################
  # estimate P(CoMo_4|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_1, C12_1, VL_0,
  #                 CoMo_1, VL_1_sim, MEMS_2, Weight_2, CoMo_2_sim,
  #                 Dose_2, MEMS_3_sim, Weight_3_sim,
  #                 C12_2, Dose_3, VL_2_sim, C12_3, CoMo_3_sim, VL_3_sim)

  data_mod_como4 <- data_frame[, c(
    "CoMo_4", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1",
    "VL_0", "CoMo_1", "VL_1_sim", "MEMS_2", "Weight_2", "CoMo_2_sim",
    "Dose_2", "MEMS_3_sim", "Weight_3_sim", "C12_2", "Dose_3", "VL_2_sim", "C12_3",
    "CoMo_3_sim", "VL_3_sim"
  )]

  mod_como4 <- glm(
    CoMo_4 ~ Age + Sex +  CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1 + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim + MEMS_2 + Weight_2 + CoMo_2_sim +
      Dose_2 + MEMS_3_sim + Weight_3_sim + I(sqrt(C12_2)) + Dose_3 + VL_2_sim + 
      I(sqrt(C12_3)) + CoMo_3_sim + VL_3_sim,
    family = binomial, data = na.omit(data_mod_como4)
  )

  # simulate CoMo_4 (set C12_1 = a, C12_2 = a, C12_3 = a)

  data_sim_como4 <- data_mod_como4
  data_sim_como4$C12_1 <- rep(c12_int, n)
  data_sim_como4$C12_2 <- rep(c12_int, n)
  data_sim_como4$C12_3 <- rep(c12_int, n)

  data_frame$CoMo_4_sim <- rbinom(n, 1, predict(mod_como4, type = "response", newdata = data_sim_como4))

  ##############################################################################
  # estimate P(VL_4|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_1, C12_1, VL_0,
  #                 CoMo_1, VL_1_sim, MEMS_2, Weight_2, CoMo_2_sim,
  #                 Dose_2, MEMS_3_sim, Weight_3_sim,
  #                 C12_2, Dose_3, VL_2_sim, C12_3, CoMo_3_sim, VL_3_sim,
  #                 MEMS_4, Weight_4, Dose_4, C12_4)

  data_mod_vl4 <- data_frame[, c(
    "VL_4", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1",
    "VL_0", "CoMo_1", "VL_1_sim", "MEMS_2", "Weight_2", "CoMo_2_sim",
    "Dose_2", "MEMS_3_sim", "Weight_3_sim", "C12_2", "Dose_3", "VL_2_sim", "C12_3",
    "CoMo_3_sim", "VL_3_sim", "MEMS_4", "Weight_4", "Dose_4", "C12_4"
  )]

  mod_vl4 <- glm(
    VL_4 ~ Age + Sex +  CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1 + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim + MEMS_2 + Weight_2 + CoMo_2_sim +
      Dose_2 + MEMS_3_sim + Weight_3_sim + I(sqrt(C12_2)) + Dose_3 + VL_2_sim + I(sqrt(C12_3)) +
      CoMo_3_sim + VL_3_sim + MEMS_4 + Weight_4 + Dose_4 + I(sqrt(C12_4)),
    family = binomial, data = na.omit(data_mod_vl4)
  )

  # simulate VL_4 (set C12_1 = a, C12_2 = a, C12_3 = a, C12_4 = a)

  data_sim_vl4 <- data_mod_vl4
  data_sim_vl4$C12_1 <- rep(c12_int, n)
  data_sim_vl4$C12_2 <- rep(c12_int, n)
  data_sim_vl4$C12_3 <- rep(c12_int, n)
  data_sim_vl4$C12_4 <- rep(c12_int, n)

  data_frame$VL_4_sim <- rbinom(n, 1, predict(mod_vl4, type = "response", newdata = data_sim_vl4))

  ##############################################################################

  ##############################################################################
  # estimate P(VL_5|Age, Sex, CoMo_0, Weight_0, Genotype, MEMS_1,
  #                 Weight_1, Dose_0, Dose_1, C12_0, C12_1, VL_0_sim,
  #                 CoMo_1, VL_1_sim, MEMS_2, Weight_2, CoMo_2_sim,
  #                 Dose_2, MEMS_3_sim, Weight_3_sim,
  #                 C12_2, Dose_3, VL_2_sim, C12_3, CoMo_3_sim, VL_3_sim,
  #                 MEMS_4, Weight_4, CoMo_4_sim, Dose_4, MEMS_5, Weight_5,
  #                 C12_4, Dose_5, VL_4_sim, C12_5)

  data_mod_vl5 <- data_frame[, c(
    "VL_5", "Age", "Sex", "CoMo_0", "Weight_0",
    "Genotype", "MEMS_1", "Weight_1",
    "Dose_1", "C12_1",
    "VL_0", "CoMo_1", "VL_1_sim", "MEMS_2", "Weight_2", "CoMo_2_sim",
    "Dose_2", "MEMS_3_sim", "Weight_3_sim", "C12_2", "Dose_3", "VL_2_sim", "C12_3",
    "CoMo_3_sim", "VL_3_sim", "MEMS_4", "Weight_4", "CoMo_4_sim", "Dose_4", "MEMS_5",
    "Weight_5", "C12_4", "Dose_5", "VL_4_sim", "C12_5"
  )]

  mod_vl5 <- glm(
    VL_5 ~ Age + Sex +  CoMo_0 + Weight_0 + Genotype + MEMS_1 +
      Weight_1 + Dose_1 + I(sqrt(C12_1)) +
      VL_0 + CoMo_1 + VL_1_sim + MEMS_2 + Weight_2 + CoMo_2_sim +
      Dose_2 + MEMS_3_sim + Weight_3_sim + I(sqrt(C12_2)) + Dose_3 + VL_2_sim + I(sqrt(C12_3)) +
      CoMo_3_sim + VL_3_sim + MEMS_4 + Weight_4 + CoMo_4_sim + Dose_4 + MEMS_5 +
      Weight_5 + I(sqrt(C12_4)) + Dose_5 + VL_4_sim + I(sqrt(C12_5)),
    family = binomial, data = na.omit(data_mod_vl5)
  )

  # simulate VL_5 (set C12_1 = a, C12_2 = a, C12_3 = a, C12_4 = a, C12_5 = a)

  data_sim_vl5 <- data_mod_vl5
  data_sim_vl5$C12_1 <- rep(c12_int, n)
  data_sim_vl5$C12_2 <- rep(c12_int, n)
  data_sim_vl5$C12_3 <- rep(c12_int, n)
  data_sim_vl5$C12_4 <- rep(c12_int, n)
  data_sim_vl5$C12_5 <- rep(c12_int, n)

  data_frame$VL_5_sim <- rbinom(n, 1, predict(mod_vl5, type = "response", newdata = data_sim_vl5))

  ##############################################################################

  return(mean(na.omit(data_frame$VL_5_sim)))
}
