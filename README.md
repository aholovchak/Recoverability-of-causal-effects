# Reproducing Simulation Results from Section 5

This repository contains `.R` files necessary for reproducing the simulation results from Section 5 of the paper "Recoverability of Causal Effects under Presence of Missing Data: a Longitudinal Case Study" by Holovchak et al. (2024).

## Relevant Files

The relevant files to run are:

- `sim_ac_vl84_Gmain.R`, `sim_ac_vl84_Galt1.R`, `sim_ac_vl84_Galt2.R`:
  - Simulation for estimation of $\theta_{84}$ based on available case analysis (AC) as defined in the main document.
  - Each file corresponds to one of the three m-DAGs: $G_{main}$, $G_{alt1}$, and $G_{alt2}$.

- `sim_mi_vl84_Gmain.R`, `sim_mi_vl84_Galt1.R`, `sim_mi_vl84_Galt2.R`:
  - Simulation for estimation of $\theta_{84}$ based on multiple imputation (MI) as defined in the main document.
  - Each file corresponds to one of the three m-DAGs: $G_{main}$, $G_{alt1}$, and $G_{alt2}$.

- `monte_carlo_conf_interv.R`:
  - Computation of Monte Carlo confidence intervals as defined in Appendix D.3.

## Additional Information

The remaining files are called by the files mentioned above.
