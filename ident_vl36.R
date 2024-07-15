library(igraph)
library(causaleffect)


g_vl36_main <- graph.formula(Age -+ CoMo_0,
                         Age -+ CoMo_36,
                         Age -+ CoMo_6,
                         Age -+ NRTI,
                         Age -+ Weight_0,
                         CoMo_0 -+ CoMo_6,
                         CoMo_0 -+ MEMS_6,
                         CoMo_0 -+ VL_6,
                         CoMo_0 -+ Weight_6,
                         CoMo_6 -+ CoMo_36,
                         CoMo_6 -+ MEMS_36,
                         CoMo_6 -+ VL_36,
                         CoMo_6 -+ Weight_36,
                         #Dose_0 -+ Dose_6,
                         #Dose_0 -+ EFV_0,
                         Dose_36 -+ EFV_36,
                         Dose_6 -+ Dose_36,
                         Dose_6 -+ EFV_6,
                         #EFV_0 -+ VL_0,
                         #EFV_0 -+ VL_36,
                         #EFV_0 -+ VL_6,
                         EFV_36 -+ VL_36,
                         EFV_6 -+ VL_36,
                         EFV_6 -+ VL_6,
                         #Genotype -+ EFV_0,
                         Genotype -+ EFV_36,
                         Genotype -+ EFV_6,
                         MEMS_36 -+ EFV_36,
                         MEMS_6 -+ CoMo_36,
                         MEMS_6 -+ EFV_6,
                         MEMS_6 -+ MEMS_36,
                         Sex -+ CoMo_0,
                         Sex -+ Genotype,
                         Sex -+ Weight_0,
                         VL_0 -+ VL_6,
                         VL_6 -+ VL_36,
                         VL_0 -+ CoMo_6,
                         VL_6 -+ CoMo_36,
                         Weight_0 -+ CoMo_6,
                         #Weight_0 -+ Dose_0,
                         Weight_0 -+ Weight_6,
                         Weight_36 -+ Dose_36,
                         Weight_6 -+ CoMo_36,
                         Weight_6 -+ Dose_6,
                         Weight_6 -+ Weight_36
                         #EFV_0 -+ MEMS_6,
                         #EFV_6 -+ MEMS_36
)

plot(g_vl36_main)

caus_eff_vl36_main <- causal.effect(y = "VL_36", x = c("EFV_6", "EFV_36"),
                               G = g_vl36_main,
                               steps = FALSE, simp = FALSE, prune = TRUE, expr = TRUE,
                               stop_on_nonid = TRUE)

cat(caus_eff_vl36_main)


################################################################################

g_vl36_alter <- graph.formula(Age -+ CoMo_0,
                              Age -+ CoMo_36,
                              Age -+ CoMo_6,
                              Age -+ NRTI,
                              Age -+ Weight_0,
                              CoMo_0 -+ CoMo_6,
                              CoMo_0 -+ MEMS_6,
                              CoMo_0 -+ VL_6,
                              CoMo_0 -+ Weight_6,
                              CoMo_6 -+ CoMo_36,
                              CoMo_6 -+ MEMS_36,
                              CoMo_6 -+ VL_36,
                              CoMo_6 -+ Weight_36,
                              #Dose_0 -+ Dose_6,
                              #Dose_0 -+ EFV_0,
                              Dose_36 -+ EFV_36,
                              Dose_6 -+ Dose_36,
                              Dose_6 -+ EFV_6,
                              #EFV_0 -+ VL_0,
                              #EFV_0 -+ VL_36,
                              #EFV_0 -+ VL_6,
                              EFV_36 -+ VL_36,
                              EFV_6 -+ VL_36,
                              EFV_6 -+ VL_6,
                              #Genotype -+ EFV_0,
                              Genotype -+ EFV_36,
                              Genotype -+ EFV_6,
                              MEMS_36 -+ EFV_36,
                              MEMS_6 -+ CoMo_36,
                              MEMS_6 -+ EFV_6,
                              MEMS_6 -+ MEMS_36,
                              Sex -+ CoMo_0,
                              Sex -+ Genotype,
                              Sex -+ Weight_0,
                              VL_0 -+ VL_6,
                              VL_6 -+ VL_36,
                              VL_0 -+ CoMo_6,
                              VL_6 -+ CoMo_36,
                              Weight_0 -+ CoMo_6,
                              #Weight_0 -+ Dose_0,
                              Weight_0 -+ Weight_6,
                              Weight_36 -+ Dose_36,
                              Weight_6 -+ CoMo_36,
                              Weight_6 -+ Dose_6,
                              Weight_6 -+ Weight_36,
                              # ADDITIONAL EDGES
                              VL_0 -+ MV_0, VL_6 -+ MV_6, VL_36 -+ MV_36,
                              BMQ -+ MV_0, BMQ -+ MV_6, BMQ -+ MV_36,
                              SES -+ MV_0, SES -+ MV_6, SES -+ MV_36,
                              BHV -+ MV_0, BHV -+ MV_6, BHV -+ MV_36,
                              MV_0 -+ M.VL_0, MV_0 -+ M.Weight_0, #MV_0 -+ M.EFV_0,
                              TI_0 -+ M.VL_0, #TI_0 -+ M.EFV_0,
                              MV_6 -+ M.VL_6, MV_6 -+ M.Weight_6, MV_6 -+ M.EFV_6,
                              TI_6 -+ M.VL_6, TI_6 -+ M.EFV_6, TI_6 -+ M.MEMS_6,
                              MV_36 -+ M.VL_36, MV_36 -+ M.Weight_36, MV_36 -+ M.EFV_36,
                              TI_36 -+ M.VL_36, TI_36 -+ M.EFV_36, TI_36 -+ M.MEMS_36
)

plot(g_vl36_alter)

caus_eff_vl36_alter <- causal.effect(y = "VL_36", x = c("EFV_6", "EFV_36"),
                                    G = g_vl36_alter,
                                    steps = FALSE, simp = FALSE, prune = TRUE, expr = TRUE,
                                    stop_on_nonid = TRUE)

cat(caus_eff_vl36_alter)

################################################################################

################################################################################

g_vl36_alter2 <- graph.formula(Age -+ CoMo_0,
                              Age -+ CoMo_36,
                              Age -+ CoMo_6,
                              Age -+ NRTI,
                              Age -+ Weight_0,
                              CoMo_0 -+ CoMo_6,
                              CoMo_0 -+ MEMS_6,
                              CoMo_0 -+ VL_6,
                              CoMo_0 -+ Weight_6,
                              CoMo_6 -+ CoMo_36,
                              CoMo_6 -+ MEMS_36,
                              CoMo_6 -+ VL_36,
                              CoMo_6 -+ Weight_36,
                              #Dose_0 -+ Dose_6,
                              #Dose_0 -+ EFV_0,
                              Dose_36 -+ EFV_36,
                              Dose_6 -+ Dose_36,
                              Dose_6 -+ EFV_6,
                              EFV_0 -+ VL_0,
                              EFV_0 -+ VL_36,
                              EFV_0 -+ VL_6,
                              EFV_36 -+ VL_36,
                              EFV_6 -+ VL_36,
                              EFV_6 -+ VL_6,
                              Genotype -+ EFV_0,
                              Genotype -+ EFV_36,
                              Genotype -+ EFV_6,
                              MEMS_36 -+ EFV_36,
                              MEMS_6 -+ CoMo_36,
                              MEMS_6 -+ EFV_6,
                              MEMS_6 -+ MEMS_36,
                              Sex -+ CoMo_0,
                              Sex -+ Genotype,
                              Sex -+ Weight_0,
                              VL_0 -+ VL_6,
                              VL_6 -+ VL_36,
                              VL_0 -+ CoMo_6,
                              VL_6 -+ CoMo_36,
                              Weight_0 -+ CoMo_6,
                              #Weight_0 -+ Dose_0,
                              Weight_0 -+ Weight_6,
                              Weight_36 -+ Dose_36,
                              Weight_6 -+ CoMo_36,
                              Weight_6 -+ Dose_6,
                              Weight_6 -+ Weight_36,
                              # ADDITIONAL EDGES
                              SES -+ MEMS_6, SES -+ MEMS_36,
                              BMQ -+ MV_0, BMQ -+ MV_6, BMQ -+ MV_36,
                              SES -+ MV_0, SES -+ MV_6, SES -+ MV_36,
                              BHV -+ MV_0, BHV -+ MV_6, BHV -+ MV_36,
                              MV_0 -+ M.VL_0, MV_0 -+ M.Weight_0, MV_0 -+ M.EFV_0,
                              TI_0 -+ M.VL_0, TI_0 -+ M.EFV_0,
                              MV_6 -+ M.VL_6, MV_6 -+ M.Weight_6, MV_6 -+ M.EFV_6,
                              TI_6 -+ M.VL_6, TI_6 -+ M.EFV_6, TI_6 -+ M.MEMS_6,
                              MV_36 -+ M.VL_36, MV_36 -+ M.Weight_36, MV_36 -+ M.EFV_36,
                              TI_36 -+ M.VL_36, TI_36 -+ M.EFV_36, TI_36 -+ M.MEMS_36
)

plot(g_vl36_alter2)

caus_eff_vl36_alter2 <- causal.effect(y = "VL_36", x = c("EFV_0", "EFV_6", "EFV_36"),
                                     G = g_vl36_alter2,
                                     steps = FALSE, simp = FALSE, prune = TRUE, expr = TRUE,
                                     stop_on_nonid = TRUE)

cat(caus_eff_vl36_alter2)

dSep(graph = g_vl36_alter2, x = "VL_0", y = c("M.VL_0", "M.EFV_0"), z = c("EFV_0"))

################################################################################

