#' Simulate a data set corresponding to G_{alt1} (See Appendix A for corresponding SCMs)

library(simcausal)
set.seed(241280)

###########################
# Data generating process #
###########################

#
rnorm_trunc <- function(n, mean, sd, minval = 0, maxval = 10000,
                        min.low = 0, max.low = 50, min.high = 5000, max.high = 10000)
{
  out <- rnorm(n = n, mean = mean, sd = sd)
  minval <- minval[1]; min1 <- min.low[1]; max1 <- max.low[1]
  maxval <- maxval[1]; min2 <- min.high[1]; max2 <- max.high[1]
  leq.zero <- length(out[out <= minval])
  geq.max <- length(out[out >= maxval])
  out[out <= minval] <- runif(n = leq.zero, min = min1, max = max1)
  out[out >= maxval] <- runif(n = geq.max, min = min2, max = max2)
  out
}
#

t.end <-5


M <- DAG.empty()
M <- M +
  node("Sex",
       t=0,
       distr = "rbern",
       prob = .5) +
  node("Genotype",
       t=0,
       dist= "rcat.b1",
       probs = {
         plogis(-0.103 + ifelse(Sex[0] == 1, 0.223, 0.173));
         plogis(-0.086 + ifelse(Sex[0] == 1, 0.198, 0.214));
         plogis(-0.309 + ifelse(Sex[0] == 1, 0.082, 0.107))
       }
  ) +
  node("Age",
       t=0,
       dist= "rnorm_trunc",
       mean = 1.501,
       minval = 0.693,
       min.low = 0.693,
       min.high = 1,
       maxval = 2.8,
       max.low = 2.7,
       max.high = 2.8,
       sd = 0.3690) +
  node("Weight",
       t=0,
       dist= "rnorm_trunc",
       mean = 1.5 + (.2 * Sex[0] + Age[0] * 0.774) *.94  ,
       sd = 0.3,
       minval = 2.26,
       min.low = 2.26,
       min.high = 2.67,
       maxval = 3.37,
       max.low = 3.02,
       max.high = 3.37
  ) +
  node("NRTI",
       t=0,
       dist= "rcat.b1",
       probs = {
         plogis(-0.006 + ifelse(Age[0] > 1.4563, Age[0] * 0.1735, Age[0] * 0.157));
         plogis(-0.006 + ifelse(Age[0] > 1.4563, Age[0] * 0.1735, Age[0] * 0.157));
         plogis(-0.006 + ifelse(Age[0] > 1.4563, Age[0] * 0.1570, Age[0] * 0.1818))
       })   +
  node("CoMo",
       t=0,
       dist ="rbern",
       prob= 0.15
  ) +
  node('VL',
       t=0,
       dist ="rbern",
       prob= 1 - plogis(0.4))


######
M <- M +
  node("Weight",
       t = 1,
       distr = "rnorm_trunc",
       mean =  I(CoMo[t-1]==1) * -0.05 + Weight[t - 1] * 1.04,
       sd = .4,
       minval = 2.26,
       min.low = 2.26,
       min.high = 2.473,
       maxval = 3.37,
       max.low =  3.2,
       max.high = 3.37
  ) +
  node("Dose",
       t=1,
       dist= "rcat.b1",
       probs ={
         plogis(5 + sqrt(Weight[t]) * 8 - Age[0] * 10);
         plogis(4 + sqrt(Weight[t]) * 8.768 - Age[0] * 9.060);
         plogis(3 + sqrt(Weight[t]) * 6.562 - Age[0] * 8.325)
       }) +
  node("SES",
       t = 0,
       distr = "rpois",
       lambda = 3) +
  node("MEMS",
       t = 1,
       distr = "rbern",
       prob = plogis(CoMo[t - 1] * 0.31 + 0.71 - 0.5 * SES[0])
  ) +
  node("MEMS",
       t = 2:t.end,
       distr = "rbern",
       prob = plogis(CoMo[t - 1] * 0.31 + MEMS[t - 1] * 0.31 + 0.71)
  ) +
  node("Weight",
       t = 2:t.end,
       distr = "rnorm_trunc",
       mean =  I(CoMo[t-1]==1) * -0.05 + Weight[t - 1] * 1.04,
       sd = .4,
       minval = 2.26,
       min.low = 2.26,
       min.high = 2.473,
       maxval = 3.37,
       max.low =  3.2,
       max.high = 3.37
  ) +
  node("CoMo",
       t = 1:t.end ,
       distr = "rbern",
       prob = 1 - (plogis(I(CoMo[t - 1] == 1) * .5 + Age[0] * .1 + Weight[t - 1] * .1)
       )
  ) +
  node("Dose",
       t = 2:t.end ,
       distr = "rcat.b1",
       prob = {
         plogis(4 + (Dose[t - 1] * 0.5) + sqrt(Weight[t]) * 4 - Age[0] * 10);
         plogis(-8 + (Dose[t - 1] * 0.5) + sqrt(Weight[t]) * 8.568 - Age[0] * 9.060);
         plogis(20 + (Dose[t - 1] * 0.5) + sqrt(Weight[t]) * 6.562 - Age[0] * 18.325)
       }
  ) +
  node("C12",
       t = 1:t.end ,
       distr = "rnorm_trunc", # was DOSE[0] with Daniel
       mean = Dose[t] * .1 + MEMS[t] * .1 + (Genotype[0] <= 2) * 2.66 +
         (Genotype[0] == 3) * 4.6,
       minval = 0.2032,
       min.low = 0.2032,
       min.high = 0.88,
       maxval = 21.84,
       max.low = 8.37,
       max.high = 21.84,
       sd = 4.06
  ) +
  node("VL",
       t = 1,
       distr = "rbern",
       prob = 1 - plogis(0.4 + CoMo[t - 1] * .1 + 2 * sqrt(C12[t]))
  ) +
  node("VL",
       t = 2,
       distr = "rbern",
       prob = 1 - plogis(CoMo[t - 1] * .1 + 2 * sqrt(C12[t]))
  )+
  node("VL",
       t = 3,
       distr = "rbern",
       prob = 1 - plogis(CoMo[t - 1] * .1 + 1.8 * sqrt(C12[t]))
  ) +
  node("VL",
       t = 4:t.end,
       distr = "rbern",
       prob = 1 - plogis(-.2 + CoMo[t - 1] * .1 + 2 * sqrt(C12[t]))
  )

compl_Dset_altern <- simcausal::set.DAG(M)


################################################################################
# Generating missingness mechanism
################################################################################

M <- M +
  node("MV",
       t = 0,
       distr = "rbern",
       prob = plogis(-2.95 + 0.1 * SES[0] + 2 * VL[0])) +
  node("MV",
       t = 1:t.end,
       distr = "rbern",
       prob = plogis(-2.95 + 0.1 * SES[0] + MV[t - 1] + 2 * VL[t])) +
  node("TI",
       t = 0:t.end,
       distr = "rbern",
       prob = 0.05) +
  node("M.C12",
       t = 1:t.end,
       distr = "rbern",
       prob = I(MV[t] == 1) * 1 + I(MV[t] == 0) * I(TI[t] == 1) * 0.5) +
  node("M.weight",
       t = 0:t.end,
       distr = "rbern",
       prob = I(MV[t] == 1) * 1) +
  node("M.VL",
       t = 0:t.end,
       distr = "rbern",
       prob = I(MV[t] == 1) * 1 + I(MV[t] == 0) * I(TI[t] == 1) * 0.5) +
  node("M.MEMS",
       t = 1:t.end,
       distr = "rbern",
       prob = I(TI[t] == 1) * 0.5 + 0.2)

Dset_altern <- simcausal::set.DAG(M)
