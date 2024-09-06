

# Longitudinal trajectories of SYMPTOMS explained by baseline MRI centils and time from baseline
# -----------------------------------------------------------------------------------------------

#library(lme4)
library(lmerTest)

##### SYMPTOMS

X_Centiles_FEP_MRI_baseline_symptoms_raw_scores<-read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_symptoms_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Keep only rows without NaN in symptoms (SAPS.total or SANS.total)
columns_to_check <- c("SAPS.total", "SANS.total")
rows_to_keep <- !is.nan(X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SAPS.total) & !is.nan(X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SANS.total)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores[rows_to_keep, ]

# Get 'etiv_residuals'
etiv_model <- lm(etiv.b ~ Age_inclusion + sex, data = X_Centiles_FEP_MRI_baseline_symptoms_raw_scores)
etiv_residuals <- resid(etiv_model)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$etiv_residuals <- etiv_residuals

# HACER COPIA DE LOS VALORES RAW DE SANS.total y SAPS.total
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SANS.total2 <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SANS.total
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SAPS.total2 <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SAPS.total

# CONVERTIR EN ENTEROS LOS VALORES 'Y' DE SAPS Y SANS TOTAL (con poisson no pueden tener decimales)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SANS.total <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SANS.total/0.25
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SAPS.total <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$SAPS.total/0.25

# convert mg CPZ to g CPZ
# X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$CPZ <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$CPZ/1000

# Hacer copia de los valores raw de Time_clinical
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$Time_clinical2 <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$Time_clinical

# Reescale to z scores (x values)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$Time_clinical <- scale(X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$Time_clinical)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$Age_inclusion <- scale(X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$Age_inclusion)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$CPZ <- scale(X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$CPZ)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$etiv_residuals <- scale(X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$etiv_residuals)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$centiles_GMV <- scale(X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$centiles_GMV)
X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$centiles_sGMV <- scale(X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$centiles_sGMV)

### LINEAR MIXED EFFECT (LME) MODEL

# Define a vector of variables you want to analyze
variables <- c(
  "SANS.affect.flatt", "SANS.alogia", "SANS.avolition", "SANS.anhedonia",
  "SANS.attention", "SANS.total", "SAPS.hallucinations", "SAPS.delusions",
  "SAPS.bizarre.behav", "SAPS.formal.thought", "SAPS.total"
)

# Initialize empty lists to store results for each variable
results_BL_centils_cortical <- list()
results_BL_centils_subcortical <- list()
results_time <- list()
results_CPZ <- list()
results_age <- list()
results_sex <- list()
results_etiv <- list()
results_interaction_cortical <- list()
results_interaction_subcortical <- list()

# Loop through each variable
for (var in variables) {
  # Initialize empty lists to store results for this variable
  beta_values_BL_centils_cortical <- list()
  p_values_BL_centils_cortical <- list()
  beta_values_BL_centils_subcortical <- list()
  p_values_BL_centils_subcortical <- list()
  beta_values_time <- list()
  p_values_time <- list()
  beta_values_CPZ <- list()
  p_values_CPZ <- list()
  beta_values_age <- list()
  p_values_age <- list()
  beta_values_sex <- list()
  p_values_sex <- list()
  beta_values_etiv <- list()
  p_values_etiv <- list()
  beta_values_interaction_cortical <- list()
  p_values_interaction_cortical <- list()
  beta_values_interaction_subcortical <- list()
  p_values_interaction_subcortical <- list()
  
  # Fit the LME model
  model_GMV <- glmer(
    formula = as.formula(
      paste0(var, " ~ centiles_GMV + centiles_sGMV + Time_clinical + CPZ + Age_inclusion + sex + etiv_residuals + centiles_GMV*Time_clinical + centiles_sGMV*Time_clinical + (1 | participant)")), family = poisson(),
    data = X_Centiles_FEP_MRI_baseline_symptoms_raw_scores,
    ,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
  print(summary(model_GMV))
  
  # Store beta and p-values in lists
  beta_values_BL_centils_cortical <- c(coef(summary(model_GMV))[2,3])
  p_values_BL_centils_cortical <- c(coef(summary(model_GMV))[2,4])
  beta_values_BL_centils_subcortical <- c(coef(summary(model_GMV))[3,3])
  p_values_BL_centils_subcortical <- c(coef(summary(model_GMV))[3,4])
  beta_values_time <- c(coef(summary(model_GMV))[4,3])
  p_values_time <- c(coef(summary(model_GMV))[4,4])
  beta_values_CPZ <- c(coef(summary(model_GMV))[5,3])
  p_values_CPZ <- c(coef(summary(model_GMV))[5,4])
  beta_values_age <- c(coef(summary(model_GMV))[6,3])
  p_values_age <- c(coef(summary(model_GMV))[6,4])
  beta_values_sex <- c(coef(summary(model_GMV))[7,3])
  p_values_sex <- c(coef(summary(model_GMV))[7,4])
  beta_values_etiv <- c(coef(summary(model_GMV))[8,3])
  p_values_etiv <- c(coef(summary(model_GMV))[8,4])
  beta_values_interaction_cortical <- c(coef(summary(model_GMV))[9,3])
  p_values_interaction_cortical <- c(coef(summary(model_GMV))[9,4])
  beta_values_interaction_subcortical <- c(coef(summary(model_GMV))[10,3])
  p_values_interaction_subcortical <- c(coef(summary(model_GMV))[10,4])
  
  # Store results for this variable
  results_BL_centils_cortical[[var]] <- list(beta_values = beta_values_BL_centils_cortical, p_values = p_values_BL_centils_cortical)
  results_BL_centils_subcortical[[var]] <- list(beta_values = beta_values_BL_centils_subcortical, p_values = p_values_BL_centils_subcortical)
  results_time[[var]] <- list(beta_values = beta_values_time, p_values = p_values_time)
  results_CPZ[[var]] <- list(beta_values = beta_values_CPZ, p_values = p_values_CPZ)
  results_age[[var]] <- list(beta_values = beta_values_age, p_values = p_values_age)
  results_sex[[var]] <- list(beta_values = beta_values_sex, p_values = p_values_sex)
  results_etiv[[var]] <- list(beta_values = beta_values_etiv, p_values = p_values_etiv)
  results_interaction_cortical[[var]] <- list(beta_values = beta_values_interaction_cortical, p_values = p_values_interaction_cortical)
  results_interaction_subcortical[[var]] <- list(beta_values = beta_values_interaction_subcortical, p_values = p_values_interaction_subcortical)
}

# ----------------------------------------------------------------------------------------

### ARRANGE BETA AND P VALUES IN A SINGLE DATA FRAME 

# Initialize an empty list to store results for each variable
results_list_BL_centils_cortical <- list()
results_list_BL_centils_subcortical <- list()
results_list_time <- list()
results_list_CPZ <- list()
results_list_age <- list()
results_list_sex <- list()
results_list_etiv <- list()
results_list_interaction_cortical <- list()
results_list_interaction_subcortical <- list()

# Initialize empty vectors to store concatenated p values for all comparisons (2x11)
all_ps_BL_centils_cortical <- c()
all_ps_BL_centils_subcortical <- c()
all_ps_time <- c()
all_ps_CPZ <- c()
all_ps_age <- c()
all_ps_sex <- c()
all_ps_etiv <- c()
all_ps_interaction_cortical <- c()
all_ps_interaction_subcortical <- c()

for (var in variables) {
  
  # Initialize empty vectors to store concatenated values
  first_betas_BL_centils_cortical <- c()
  first_ps_BL_centils_cortical <- c()
  
  first_betas_BL_centils_subcortical <- c()
  first_ps_BL_centils_subcortical <- c()
  
  first_betas_time <- c()
  first_ps_time <- c()
  
  first_betas_CPZ <- c()
  first_ps_CPZ <- c()
  
  first_betas_age <- c()
  first_ps_age <- c()
  
  first_betas_sex <- c()
  first_ps_sex <- c()
  
  first_betas_etiv <- c()
  first_ps_etiv <- c()
  
  first_betas_interaction_cortical <- c()
  first_ps_interaction_cortical <- c()
  
  first_betas_interaction_subcortical <- c()
  first_ps_interaction_subcortical <- c()
  
  # Extract the first and second numeric values for the current brain region
  first_beta_BL_centils_cortical <- results_BL_centils_cortical[[var]]$beta_values[1]
  first_p_BL_centils_cortical <- results_BL_centils_cortical[[var]]$p_values[1]
  
  first_beta_BL_centils_subcortical <- results_BL_centils_subcortical[[var]]$beta_values[1]
  first_p_BL_centils_subcortical <- results_BL_centils_subcortical[[var]]$p_values[1]
  
  first_beta_time <- results_time[[var]]$beta_values[1]
  first_p_time <- results_time[[var]]$p_values[1]
  
  first_beta_CPZ <- results_CPZ[[var]]$beta_values[1]
  first_p_CPZ <- results_CPZ[[var]]$p_values[1]
  
  first_beta_age <- results_age[[var]]$beta_values[1]
  first_p_age <- results_age[[var]]$p_values[1]
  
  first_beta_sex <- results_sex[[var]]$beta_values[1]
  first_p_sex <- results_sex[[var]]$p_values[1]
  
  first_beta_etiv <- results_etiv[[var]]$beta_values[1]
  first_p_etiv <- results_etiv[[var]]$p_values[1]
  
  first_beta_interaction_cortical <- results_interaction_cortical[[var]]$beta_values[1]
  first_p_interaction_cortical <- results_interaction_cortical[[var]]$p_values[1]
  
  first_beta_interaction_subcortical <- results_interaction_subcortical[[var]]$beta_values[1]
  first_p_interaction_subcortical <- results_interaction_subcortical[[var]]$p_values[1]
  
  # Append the first and second numeric values to their respective vectors
  first_betas_BL_centils_cortical <- c(first_betas_BL_centils_cortical, first_beta_BL_centils_cortical)
  first_ps_BL_centils_cortical <- c(first_ps_BL_centils_cortical, first_p_BL_centils_cortical)
  
  first_betas_BL_centils_subcortical <- c(first_betas_BL_centils_subcortical, first_beta_BL_centils_subcortical)
  first_ps_BL_centils_subcortical <- c(first_ps_BL_centils_subcortical, first_p_BL_centils_subcortical)
  
  first_betas_time <- c(first_betas_time, first_beta_time)
  first_ps_time <- c(first_ps_time, first_p_time)
  
  first_betas_CPZ <- c(first_betas_CPZ, first_beta_CPZ)
  first_ps_CPZ <- c(first_ps_CPZ, first_p_CPZ)
  
  first_betas_age <- c(first_betas_age, first_beta_age)
  first_ps_age <- c(first_ps_age, first_p_age)
  
  first_betas_sex <- c(first_betas_sex, first_beta_sex)
  first_ps_sex <- c(first_ps_sex, first_p_sex)
  
  first_betas_etiv <- c(first_betas_etiv, first_beta_etiv)
  first_ps_etiv <- c(first_ps_etiv, first_p_etiv)
  
  first_betas_interaction_cortical <- c(first_betas_interaction_cortical, first_beta_interaction_cortical)
  first_ps_interaction_cortical <- c(first_ps_interaction_cortical, first_p_interaction_cortical)
  
  first_betas_interaction_subcortical <- c(first_betas_interaction_subcortical, first_beta_interaction_subcortical)
  first_ps_interaction_subcortical <- c(first_ps_interaction_subcortical, first_p_interaction_subcortical)
  
  # FOR BL_CENTILS_CORTICAL
  
  values <- first_betas_BL_centils_cortical
  p <- first_ps_BL_centils_cortical
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_BL_centils_cortical[[var]] <- data
  # Store the p of all variables for 11 comparisons
  all_ps_BL_centils_cortical <- c(all_ps_BL_centils_cortical, p)
  
  # FOR BL_CENTILS_SUBCORTICAL
  
  values <- first_betas_BL_centils_subcortical
  p <- first_ps_BL_centils_subcortical
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_BL_centils_subcortical[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_BL_centils_subcortical <- c(all_ps_BL_centils_subcortical, p)
  
  # FOR TIME
  
  values <- first_betas_time
  p <- first_ps_time
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_time[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_time <- c(all_ps_time, p) 
  
  # FOR CPZ
  
  values <- first_betas_CPZ
  p <- first_ps_CPZ
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_CPZ[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_CPZ <- c(all_ps_CPZ, p)  
  
  # FOR AGE
  
  values <- first_betas_age
  p <- first_ps_age
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_age[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_age <- c(all_ps_age, p)
  
  # FOR SEX
  
  values <- first_betas_sex
  p <- first_ps_sex
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_sex[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_sex <- c(all_ps_sex, p)
  
  # FOR ETIV
  
  values <- first_betas_etiv
  p <- first_ps_etiv
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_etiv[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_etiv <- c(all_ps_etiv, p)
  
  # FOR INTERACTION CORTICAL
  
  values <- first_betas_interaction_cortical
  p <- first_ps_interaction_cortical
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_interaction_cortical[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_interaction_cortical <- c(all_ps_interaction_cortical, p)
  
  # FOR INTERACTION SUBCORTICAL
  
  values <- first_betas_interaction_subcortical
  p <- first_ps_interaction_subcortical
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_interaction_subcortical[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_interaction_subcortical <- c(all_ps_interaction_subcortical, p)
  
}

# MULTIPLE CORRECTION 

# *** Perform FDR correction on p-values for 11 comparisons (symptoms)
p_fdr2_BL_centils_cortical <- p.adjust(all_ps_BL_centils_cortical, method = "fdr")
sig2_BL_centils_cortical <- c(p_fdr2_BL_centils_cortical < 0.05)
p_fdr2_BL_centils_subcortical <- p.adjust(all_ps_BL_centils_subcortical, method = "fdr")
sig2_BL_centils_subcortical <- c(p_fdr2_BL_centils_subcortical < 0.05)
p_fdr2_time <- p.adjust(all_ps_time, method = "fdr")
sig2_time <- c(p_fdr2_time < 0.05)  
p_fdr2_CPZ <- p.adjust(all_ps_CPZ, method = "fdr")
sig2_CPZ <- c(p_fdr2_CPZ < 0.05) 
p_fdr2_age <- p.adjust(all_ps_age, method = "fdr")
sig2_age <- c(p_fdr2_age < 0.05) 
p_fdr2_sex <- p.adjust(all_ps_sex, method = "fdr")
sig2_sex <- c(p_fdr2_sex < 0.05) 
p_fdr2_etiv <- p.adjust(all_ps_etiv, method = "fdr")
sig2_etiv <- c(p_fdr2_etiv < 0.05) 
p_fdr2_interaction_cortical <- p.adjust(all_ps_interaction_cortical, method = "fdr")
sig2_interaction_cortical <- c(p_fdr2_interaction_cortical < 0.05)
p_fdr2_interaction_subcortical <- p.adjust(all_ps_interaction_subcortical, method = "fdr")
sig2_interaction_subcortical <- c(p_fdr2_interaction_subcortical < 0.05)

results_list2_BL_centils_cortical <- list()
results_list2_BL_centils_subcortical <- list()
results_list2_time <- list()
results_list2_CPZ <- list()
results_list2_age <- list()
results_list2_sex <- list()
results_list2_etiv <- list()
results_list2_interaction_cortical <- list()
results_list2_interaction_subcortical <- list()

# Update results_list2 with the new significance results

for (j in seq_along(variables)) {
  var <- variables[j]
  
  # Replace the values in 'values', 'p_fdr' and 'sig' columns with the corresponding values from the indices
  results_list2_BL_centils_cortical[[var]][["values"]] <- results_list_BL_centils_cortical[[var]][["values"]][1]
  results_list2_BL_centils_cortical[[var]][["p_fdr"]] <- p_fdr2_BL_centils_cortical[j]
  results_list2_BL_centils_cortical[[var]][["sig"]] <- sig2_BL_centils_cortical[j]
  results_list2_BL_centils_subcortical[[var]][["values"]] <- results_list_BL_centils_subcortical[[var]][["values"]][1]
  results_list2_BL_centils_subcortical[[var]][["p_fdr"]] <- p_fdr2_BL_centils_subcortical[j]
  results_list2_BL_centils_subcortical[[var]][["sig"]] <- sig2_BL_centils_subcortical[j]
  results_list2_time[[var]][["values"]] <- results_list_time[[var]][["values"]][1]
  results_list2_time[[var]][["p_fdr"]] <- p_fdr2_time[j]
  results_list2_time[[var]][["sig"]] <- sig2_time[j]
  results_list2_CPZ[[var]][["values"]] <- results_list_CPZ[[var]][["values"]][1]
  results_list2_CPZ[[var]][["p_fdr"]] <- p_fdr2_CPZ[j]
  results_list2_CPZ[[var]][["sig"]] <- sig2_CPZ[j]
  results_list2_age[[var]][["values"]] <- results_list_age[[var]][["values"]][1]
  results_list2_age[[var]][["p_fdr"]] <- p_fdr2_age[j]
  results_list2_age[[var]][["sig"]] <- sig2_age[j]
  results_list2_sex[[var]][["values"]] <- results_list_sex[[var]][["values"]][1]
  results_list2_sex[[var]][["p_fdr"]] <- p_fdr2_sex[j]
  results_list2_sex[[var]][["sig"]] <- sig2_sex[j]
  results_list2_etiv[[var]][["values"]] <- results_list_etiv[[var]][["values"]][1]
  results_list2_etiv[[var]][["p_fdr"]] <- p_fdr2_etiv[j]
  results_list2_etiv[[var]][["sig"]] <- sig2_etiv[j]
  results_list2_interaction_cortical[[var]][["values"]] <- results_list_interaction_cortical[[var]][["values"]][1]
  results_list2_interaction_cortical[[var]][["p_fdr"]] <- p_fdr2_interaction_cortical[j]
  results_list2_interaction_cortical[[var]][["sig"]] <- sig2_interaction_cortical[j]
  results_list2_interaction_subcortical[[var]][["values"]] <- results_list_interaction_subcortical[[var]][["values"]][1]
  results_list2_interaction_subcortical[[var]][["p_fdr"]] <- p_fdr2_interaction_subcortical[j]
  results_list2_interaction_subcortical[[var]][["sig"]] <- sig2_interaction_subcortical[j]
  
}

### PLOT RESULTS AS AN HORIZONTAL BAR CHART 

library(ggplot2)

# Initialize lists to store beta and sig values
beta_BL_centils_cortical <- list()
beta_BL_centils_subcortical <- list()
beta_time <- list()
beta_CPZ <- list()
beta_age <- list()
beta_sex <- list()
beta_etiv <- list()
beta_interaction_cortical <- list()
beta_interaction_subcortical <- list()

sig_BL_centils_cortical <- list()
sig_BL_centils_subcortical <- list()
sig_time <- list()
sig_CPZ <- list()
sig_age <- list()
sig_sex <- list()
sig_etiv <- list()
sig_interaction_cortical <- list()
sig_interaction_subcortical <- list()

# Initialize result_types list
result_types <- list()

for (var in variables) {

beta_BL_centils_cortical[[var]] <- c(results_list2_BL_centils_cortical[[var]]$values)
beta_BL_centils_subcortical[[var]] <- c(results_list2_BL_centils_subcortical[[var]]$values)
beta_time[[var]] <- c(results_list2_time[[var]]$values)
beta_CPZ[[var]] <- c(results_list2_CPZ[[var]]$values)
beta_age[[var]] <- c(results_list2_age[[var]]$values)
beta_sex[[var]] <- c(results_list2_sex[[var]]$values)
beta_etiv[[var]] <- c(results_list2_etiv[[var]]$values)
beta_interaction_cortical[[var]] <- c(results_list2_interaction_cortical[[var]]$values)
beta_interaction_subcortical[[var]] <- c(results_list2_interaction_subcortical[[var]]$values)
sig_BL_centils_cortical[[var]] <- c(results_list2_BL_centils_cortical[[var]]$sig)
sig_BL_centils_subcortical[[var]] <- c(results_list2_BL_centils_subcortical[[var]]$sig)
sig_time[[var]] <- c(results_list2_time[[var]]$sig)
sig_CPZ[[var]] <- c(results_list2_CPZ[[var]]$sig)
sig_age[[var]] <- c(results_list2_age[[var]]$sig)
sig_sex[[var]] <- c(results_list2_sex[[var]]$sig)
sig_etiv[[var]] <- c(results_list2_etiv[[var]]$sig)
sig_interaction_cortical[[var]] <- c(results_list2_interaction_cortical[[var]]$sig)
sig_interaction_subcortical[[var]] <- c(results_list2_interaction_subcortical[[var]]$sig)

}

##### GAF

X_Centiles_FEP_MRI_baseline_GAF_raw_scores<-read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_GAF_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Keep only rows without NaN in GAF
columns_to_check <- c("GAF")
rows_to_keep <- !is.nan(X_Centiles_FEP_MRI_baseline_GAF_raw_scores$GAF)
X_Centiles_FEP_MRI_baseline_GAF_raw_scores <- X_Centiles_FEP_MRI_baseline_GAF_raw_scores[rows_to_keep, ]

# Get 'etiv_residuals'
etiv_model <- lm(etiv.b ~ Age_inclusion + sex, data = X_Centiles_FEP_MRI_baseline_GAF_raw_scores)
etiv_residuals <- resid(etiv_model)
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$etiv_residuals <- etiv_residuals

# Hacer copia de los valores raw de GAF
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$GAF2 <- X_Centiles_FEP_MRI_baseline_GAF_raw_scores$GAF

# Hacer copia de los valores raw de Time_clinical
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Time_clinical2 <- X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Time_clinical

# Rescale to z scores (x values)
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Time_clinical <- scale(X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Time_clinical)
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Age_inclusion <- scale(X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Age_inclusion)
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$CPZ <- scale(X_Centiles_FEP_MRI_baseline_GAF_raw_scores$CPZ)
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$etiv_residuals <- scale(X_Centiles_FEP_MRI_baseline_GAF_raw_scores$etiv_residuals)
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Centiles_GMV <- scale(X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Centiles_GMV)
X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Centiles_sGMV <- scale(X_Centiles_FEP_MRI_baseline_GAF_raw_scores$Centiles_sGMV)

### LINEAR MIXED EFFECT (LME) MODEL

# Define a vector of variables you want to analyze
variables <- c(
  "GAF"
)

# Initialize empty lists to store results for each variable
results_BL_centils_cortical <- list()
results_BL_centils_subcortical <- list()
results_time <- list()
results_CPZ <- list()
results_age <- list()
results_sex <- list()
results_etiv <- list()
results_interaction_cortical <- list()
results_interaction_subcortical <- list()

# Loop through each variable
for (var in variables) {
  # Initialize empty lists to store results for this variable
  beta_values_BL_centils_cortical <- list()
  p_values_BL_centils_cortical <- list()
  beta_values_BL_centils_subcortical <- list()
  p_values_BL_centils_subcortical <- list()
  beta_values_time <- list()
  p_values_time <- list()
  beta_values_CPZ <- list()
  p_values_CPZ <- list()
  beta_values_age <- list()
  p_values_age <- list()
  beta_values_sex <- list()
  p_values_sex <- list()
  beta_values_etiv <- list()
  p_values_etiv <- list()
  beta_values_interaction_cortical <- list()
  p_values_interaction_cortical <- list()
  beta_values_interaction_subcortical <- list()
  p_values_interaction_subcortical <- list()
  
  # Fit the LME model
  model_GMV <- glmer(
    formula = as.formula(
      paste0(var, " ~ Centiles_GMV + Centiles_sGMV + Time_clinical + CPZ + Age_inclusion + sex + etiv_residuals + Centiles_GMV*Time_clinical + Centiles_sGMV*Time_clinical + (1 | participant)")), family = poisson(),
    data = X_Centiles_FEP_MRI_baseline_GAF_raw_scores,
    ,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  )
  print(summary(model_GMV))
  
  # Store beta and p-values in lists
  beta_values_BL_centils_cortical <- c(coef(summary(model_GMV))[2,3])
  p_values_BL_centils_cortical <- c(coef(summary(model_GMV))[2,4])
  beta_values_BL_centils_subcortical <- c(coef(summary(model_GMV))[3,3])
  p_values_BL_centils_subcortical <- c(coef(summary(model_GMV))[3,4])
  beta_values_time <- c(coef(summary(model_GMV))[4,3])
  p_values_time <- c(coef(summary(model_GMV))[4,4])
  beta_values_CPZ <- c(coef(summary(model_GMV))[5,3])
  p_values_CPZ <- c(coef(summary(model_GMV))[5,4])
  beta_values_age <- c(coef(summary(model_GMV))[6,3])
  p_values_age <- c(coef(summary(model_GMV))[6,4])
  beta_values_sex <- c(coef(summary(model_GMV))[7,3])
  p_values_sex <- c(coef(summary(model_GMV))[7,4])
  beta_values_etiv <- c(coef(summary(model_GMV))[8,3])
  p_values_etiv <- c(coef(summary(model_GMV))[8,4])
  beta_values_interaction_cortical <- c(coef(summary(model_GMV))[9,3])
  p_values_interaction_cortical <- c(coef(summary(model_GMV))[9,4])
  beta_values_interaction_subcortical <- c(coef(summary(model_GMV))[10,3])
  p_values_interaction_subcortical <- c(coef(summary(model_GMV))[10,4])
  
  # Store results for this variable
  results_BL_centils_cortical[[var]] <- list(beta_values = beta_values_BL_centils_cortical, p_values = p_values_BL_centils_cortical)
  results_BL_centils_subcortical[[var]] <- list(beta_values = beta_values_BL_centils_subcortical, p_values = p_values_BL_centils_subcortical)
  results_time[[var]] <- list(beta_values = beta_values_time, p_values = p_values_time)
  results_CPZ[[var]] <- list(beta_values = beta_values_CPZ, p_values = p_values_CPZ)
  results_age[[var]] <- list(beta_values = beta_values_age, p_values = p_values_age)
  results_sex[[var]] <- list(beta_values = beta_values_sex, p_values = p_values_sex)
  results_etiv[[var]] <- list(beta_values = beta_values_etiv, p_values = p_values_etiv)
  results_interaction_cortical[[var]] <- list(beta_values = beta_values_interaction_cortical, p_values = p_values_interaction_cortical)
  results_interaction_subcortical[[var]] <- list(beta_values = beta_values_interaction_subcortical, p_values = p_values_interaction_subcortical)
}

# ----------------------------------------------------------------------------------------

### ARRANGE BETA AND P VALUES IN A SINGLE DATA FRAME 

# Initialize an empty list to store results for each variable
results_list_BL_centils_cortical <- list()
results_list_BL_centils_subcortical <- list()
results_list_time <- list()
results_list_CPZ <- list()
results_list_age <- list()
results_list_sex <- list()
results_list_etiv <- list()
results_list_interaction_cortical <- list()
results_list_interaction_subcortical <- list()

# Initialize empty vectors to store concatenated p values for all comparisons (2x11)
all_ps_BL_centils_cortical <- c()
all_ps_BL_centils_subcortical <- c()
all_ps_time <- c()
all_ps_CPZ <- c()
all_ps_age <- c()
all_ps_sex <- c()
all_ps_etiv <- c()
all_ps_interaction_cortical <- c()
all_ps_interaction_subcortical <- c()

for (var in variables) {
  
  # Initialize empty vectors to store concatenated values
  first_betas_BL_centils_cortical <- c()
  first_ps_BL_centils_cortical <- c()
  
  first_betas_BL_centils_subcortical <- c()
  first_ps_BL_centils_subcortical <- c()
  
  first_betas_time <- c()
  first_ps_time <- c()
  
  first_betas_CPZ <- c()
  first_ps_CPZ <- c()
  
  first_betas_age <- c()
  first_ps_age <- c()
  
  first_betas_sex <- c()
  first_ps_sex <- c()
  
  first_betas_etiv <- c()
  first_ps_etiv <- c()
  
  first_betas_interaction_cortical <- c()
  first_ps_interaction_cortical <- c()
  
  first_betas_interaction_subcortical <- c()
  first_ps_interaction_subcortical <- c()
  
  # Extract the first and second numeric values for the current brain region
  first_beta_BL_centils_cortical <- results_BL_centils_cortical[[var]]$beta_values[1]
  first_p_BL_centils_cortical <- results_BL_centils_cortical[[var]]$p_values[1]
  
  first_beta_BL_centils_subcortical <- results_BL_centils_subcortical[[var]]$beta_values[1]
  first_p_BL_centils_subcortical <- results_BL_centils_subcortical[[var]]$p_values[1]
  
  first_beta_time <- results_time[[var]]$beta_values[1]
  first_p_time <- results_time[[var]]$p_values[1]
  
  first_beta_CPZ <- results_CPZ[[var]]$beta_values[1]
  first_p_CPZ <- results_CPZ[[var]]$p_values[1]
  
  first_beta_age <- results_age[[var]]$beta_values[1]
  first_p_age <- results_age[[var]]$p_values[1]
  
  first_beta_sex <- results_sex[[var]]$beta_values[1]
  first_p_sex <- results_sex[[var]]$p_values[1]
  
  first_beta_etiv <- results_etiv[[var]]$beta_values[1]
  first_p_etiv <- results_etiv[[var]]$p_values[1]
  
  first_beta_interaction_cortical <- results_interaction_cortical[[var]]$beta_values[1]
  first_p_interaction_cortical <- results_interaction_cortical[[var]]$p_values[1]
  
  first_beta_interaction_subcortical <- results_interaction_subcortical[[var]]$beta_values[1]
  first_p_interaction_subcortical <- results_interaction_subcortical[[var]]$p_values[1]
  
  # Append the first and second numeric values to their respective vectors
  first_betas_BL_centils_cortical <- c(first_betas_BL_centils_cortical, first_beta_BL_centils_cortical)
  first_ps_BL_centils_cortical <- c(first_ps_BL_centils_cortical, first_p_BL_centils_cortical)
  
  first_betas_BL_centils_subcortical <- c(first_betas_BL_centils_subcortical, first_beta_BL_centils_subcortical)
  first_ps_BL_centils_subcortical <- c(first_ps_BL_centils_subcortical, first_p_BL_centils_subcortical)

  first_betas_time <- c(first_betas_time, first_beta_time)
  first_ps_time <- c(first_ps_time, first_p_time)
  
  first_betas_CPZ <- c(first_betas_CPZ, first_beta_CPZ)
  first_ps_CPZ <- c(first_ps_CPZ, first_p_CPZ)
  
  first_betas_age <- c(first_betas_age, first_beta_age)
  first_ps_age <- c(first_ps_age, first_p_age)
  
  first_betas_sex <- c(first_betas_sex, first_beta_sex)
  first_ps_sex <- c(first_ps_sex, first_p_sex)
  
  first_betas_etiv <- c(first_betas_etiv, first_beta_etiv)
  first_ps_etiv <- c(first_ps_etiv, first_p_etiv)
  
  first_betas_interaction_cortical <- c(first_betas_interaction_cortical, first_beta_interaction_cortical)
  first_ps_interaction_cortical <- c(first_ps_interaction_cortical, first_p_interaction_cortical)
  
  first_betas_interaction_subcortical <- c(first_betas_interaction_subcortical, first_beta_interaction_subcortical)
  first_ps_interaction_subcortical <- c(first_ps_interaction_subcortical, first_p_interaction_subcortical)
  
  # FOR BL_CENTILS_CORTICAL
  
  values <- first_betas_BL_centils_cortical
  p <- first_ps_BL_centils_cortical
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_BL_centils_cortical[[var]] <- data
  # Store the p of all variables for 11 comparisons
  all_ps_BL_centils_cortical <- c(all_ps_BL_centils_cortical, p)
  
  # FOR BL_CENTILS_SUBCORTICAL
  
  values <- first_betas_BL_centils_subcortical
  p <- first_ps_BL_centils_subcortical
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_BL_centils_subcortical[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_BL_centils_subcortical <- c(all_ps_BL_centils_subcortical, p)
  
  # FOR TIME
  
  values <- first_betas_time
  p <- first_ps_time
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_time[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_time <- c(all_ps_time, p) 
  
  # FOR CPZ
  
  values <- first_betas_CPZ
  p <- first_ps_CPZ
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_CPZ[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_CPZ <- c(all_ps_CPZ, p)  
  
  # FOR AGE
  
  values <- first_betas_age
  p <- first_ps_age
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_age[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_age <- c(all_ps_age, p)
  
  # FOR SEX
  
  values <- first_betas_sex
  p <- first_ps_sex
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_sex[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_sex <- c(all_ps_sex, p)
  
  # FOR ETIV
  
  values <- first_betas_etiv
  p <- first_ps_etiv
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_etiv[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_etiv <- c(all_ps_etiv, p)
  
  # FOR INTERACTION CORTICAL
  
  values <- first_betas_interaction_cortical
  p <- first_ps_interaction_cortical
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_interaction_cortical[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_interaction_cortical <- c(all_ps_interaction_cortical, p)
  
  # FOR INTERACTION SUBCORTICAL
  
  values <- first_betas_interaction_subcortical
  p <- first_ps_interaction_subcortical
  p_fdr <- p
  sig <- c(p < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_interaction_subcortical[[var]] <- data
  # Store the ps of all variables for 11 comparisons
  all_ps_interaction_subcortical <- c(all_ps_interaction_subcortical, p)
  
}

# MULTIPLE CORRECTION 

# *** Perform FDR correction on p-values for 11 comparisons (symptoms)
p_fdr2_BL_centils_cortical <- p.adjust(all_ps_BL_centils_cortical, method = "fdr")
sig2_BL_centils_cortical <- c(p_fdr2_BL_centils_cortical < 0.05)
p_fdr2_BL_centils_subcortical <- p.adjust(all_ps_BL_centils_subcortical, method = "fdr")
sig2_BL_centils_subcortical <- c(p_fdr2_BL_centils_subcortical < 0.05)
p_fdr2_time <- p.adjust(all_ps_time, method = "fdr")
sig2_time <- c(p_fdr2_time < 0.05)  
p_fdr2_CPZ <- p.adjust(all_ps_CPZ, method = "fdr")
sig2_CPZ <- c(p_fdr2_CPZ < 0.05) 
p_fdr2_age <- p.adjust(all_ps_age, method = "fdr")
sig2_age <- c(p_fdr2_age < 0.05) 
p_fdr2_sex <- p.adjust(all_ps_sex, method = "fdr")
sig2_sex <- c(p_fdr2_sex < 0.05) 
p_fdr2_etiv <- p.adjust(all_ps_etiv, method = "fdr")
sig2_etiv <- c(p_fdr2_etiv < 0.05) 
p_fdr2_interaction_cortical <- p.adjust(all_ps_interaction_cortical, method = "fdr")
sig2_interaction_cortical <- c(p_fdr2_interaction_cortical < 0.05)
p_fdr2_interaction_subcortical <- p.adjust(all_ps_interaction_subcortical, method = "fdr")
sig2_interaction_subcortical <- c(p_fdr2_interaction_subcortical < 0.05)

results_list2_BL_centils_cortical <- list()
results_list2_BL_centils_subcortical <- list()
results_list2_time <- list()
results_list2_CPZ <- list()
results_list2_age <- list()
results_list2_sex <- list()
results_list2_etiv <- list()
results_list2_interaction_cortical <- list()
results_list2_interaction_subcortical <- list()

# Update results_list2 with the new significance results

for (var in variables) {
  
  # Replace the values in 'values', 'p_fdr' and 'sig' columns with the corresponding values from the indices
  results_list2_BL_centils_cortical[[var]][["values"]] <- results_list_BL_centils_cortical[[var]][["values"]][1]
  results_list2_BL_centils_cortical[[var]][["p_fdr"]] <- p_fdr2_BL_centils_cortical[1]
  results_list2_BL_centils_cortical[[var]][["sig"]] <- sig2_BL_centils_cortical[1]
  results_list2_BL_centils_subcortical[[var]][["values"]] <- results_list_BL_centils_subcortical[[var]][["values"]][1]
  results_list2_BL_centils_subcortical[[var]][["p_fdr"]] <- p_fdr2_BL_centils_subcortical[1]
  results_list2_BL_centils_subcortical[[var]][["sig"]] <- sig2_BL_centils_subcortical[1]
  results_list2_time[[var]][["values"]] <- results_list_time[[var]][["values"]][1]
  results_list2_time[[var]][["p_fdr"]] <- p_fdr2_time[1]
  results_list2_time[[var]][["sig"]] <- sig2_time[1]
  results_list2_CPZ[[var]][["values"]] <- results_list_CPZ[[var]][["values"]][1]
  results_list2_CPZ[[var]][["p_fdr"]] <- p_fdr2_CPZ[1]
  results_list2_CPZ[[var]][["sig"]] <- sig2_CPZ[1]
  results_list2_age[[var]][["values"]] <- results_list_age[[var]][["values"]][1]
  results_list2_age[[var]][["p_fdr"]] <- p_fdr2_age[1]
  results_list2_age[[var]][["sig"]] <- sig2_age[1]
  results_list2_sex[[var]][["values"]] <- results_list_sex[[var]][["values"]][1]
  results_list2_sex[[var]][["p_fdr"]] <- p_fdr2_sex[1]
  results_list2_sex[[var]][["sig"]] <- sig2_sex[1]
  results_list2_etiv[[var]][["values"]] <- results_list_etiv[[var]][["values"]][1]
  results_list2_etiv[[var]][["p_fdr"]] <- p_fdr2_etiv[1]
  results_list2_etiv[[var]][["sig"]] <- sig2_etiv[1]
  results_list2_interaction_cortical[[var]][["values"]] <- results_list_interaction_cortical[[var]][["values"]][1]
  results_list2_interaction_cortical[[var]][["p_fdr"]] <- p_fdr2_interaction_cortical[1]
  results_list2_interaction_cortical[[var]][["sig"]] <- sig2_interaction_cortical[1]
  results_list2_interaction_subcortical[[var]][["values"]] <- results_list_interaction_subcortical[[var]][["values"]][1]
  results_list2_interaction_subcortical[[var]][["p_fdr"]] <- p_fdr2_interaction_subcortical[1]
  results_list2_interaction_subcortical[[var]][["sig"]] <- sig2_interaction_subcortical[1]
  
}

### PLOT RESULTS AS AN HORIZONTAL BAR CHART 

library(ggplot2)

# Initialize lists to store beta and sig values
GAF_beta_BL_centils_cortical <- list()
GAF_beta_BL_centils_subcortical <- list()
GAF_beta_time <- list()
GAF_beta_CPZ <- list()
GAF_beta_age <- list()
GAF_beta_sex <- list()
GAF_beta_etiv <- list()
GAF_beta_interaction_cortical <- list()
GAF_beta_interaction_subcortical <- list()

GAF_sig_BL_centils_cortical <- list()
GAF_sig_BL_centils_subcortical <- list()
GAF_sig_time <- list()
GAF_sig_CPZ <- list()
GAF_sig_age <- list()
GAF_sig_sex <- list()
GAF_sig_etiv <- list()
GAF_sig_interaction_cortical <- list()
GAF_sig_interaction_subcortical <- list()

# Initialize result_types list
result_types <- list()

for (var in variables) {

GAF_beta_BL_centils_cortical[[var]] <- c(results_list2_BL_centils_cortical[[var]]$values)
GAF_beta_BL_centils_subcortical[[var]] <- c(results_list2_BL_centils_subcortical[[var]]$values)
GAF_beta_time[[var]] <- c(results_list2_time[[var]]$values)
GAF_beta_CPZ[[var]] <- c(results_list2_CPZ[[var]]$values)
GAF_beta_age[[var]] <- c(results_list2_age[[var]]$values)
GAF_beta_sex[[var]] <- c(results_list2_sex[[var]]$values)
GAF_beta_etiv[[var]] <- c(results_list2_etiv[[var]]$values)
GAF_beta_interaction_cortical[[var]] <- c(results_list2_interaction_cortical[[var]]$values)
GAF_beta_interaction_subcortical[[var]] <- c(results_list2_interaction_subcortical[[var]]$values)
GAF_sig_BL_centils_cortical[[var]] <- c(results_list2_BL_centils_cortical[[var]]$sig)
GAF_sig_BL_centils_subcortical[[var]] <- c(results_list2_BL_centils_subcortical[[var]]$sig)
GAF_sig_time[[var]] <- c(results_list2_time[[var]]$sig)
GAF_sig_CPZ[[var]] <- c(results_list2_CPZ[[var]]$sig)
GAF_sig_age[[var]] <- c(results_list2_age[[var]]$sig)
GAF_sig_sex[[var]] <- c(results_list2_sex[[var]]$sig)
GAF_sig_etiv[[var]] <- c(results_list2_etiv[[var]]$sig)
GAF_sig_interaction_cortical[[var]] <- c(results_list2_interaction_cortical[[var]]$sig)
GAF_sig_interaction_subcortical[[var]] <- c(results_list2_interaction_subcortical[[var]]$sig)

}

### Concatenate symptoms data and GAF data within the vectors

beta_BL_centils_cortical <- c(beta_BL_centils_cortical, GAF_beta_BL_centils_cortical)
beta_BL_centils_subcortical <- c(beta_BL_centils_subcortical, GAF_beta_BL_centils_subcortical)
beta_time <- c(beta_time, GAF_beta_time)
beta_CPZ <- c(beta_CPZ, GAF_beta_CPZ)
beta_age <- c(beta_age, GAF_beta_age)
beta_sex <- c(beta_sex, GAF_beta_sex)
beta_etiv <- c(beta_etiv, GAF_beta_etiv)
beta_interaction_cortical <- c(beta_interaction_cortical, GAF_beta_interaction_cortical)
beta_interaction_subcortical <- c(beta_interaction_subcortical, GAF_beta_interaction_subcortical)

sig_BL_centils_cortical <- c(sig_BL_centils_cortical, GAF_sig_BL_centils_cortical)
sig_BL_centils_subcortical <- c(sig_BL_centils_subcortical, GAF_sig_BL_centils_subcortical)
sig_time <- c(sig_time, GAF_sig_time)
sig_CPZ <- c(sig_CPZ, GAF_sig_CPZ)
sig_age <- c(sig_age, GAF_sig_age)
sig_sex <- c(sig_sex, GAF_sig_sex)
sig_etiv <- c(sig_etiv, GAF_sig_etiv)
sig_interaction_cortical <- c(sig_interaction_cortical, GAF_sig_interaction_cortical)
sig_interaction_subcortical <- c(sig_interaction_subcortical, GAF_sig_interaction_subcortical)

# Define the result types and their respective file names and titles
result_types <- list(
  BL_centiles_cortical = list(beta = beta_BL_centils_cortical, sig = sig_BL_centils_cortical, title = expression("Cortical Centiles "[BL]), file_name = "BL_centils_cor"),
  BL_centiles_subcortical = list(beta = beta_BL_centils_subcortical, sig = sig_BL_centils_subcortical, title = expression("Subcortical Centiles "[BL]), file_name = "BL_centils_sub"),
  time = list(beta = beta_time, sig = sig_time, title = "Time", file_name = "Time"),
  CPZ = list(beta = beta_CPZ, sig = sig_CPZ, title = "Antipsychotic treatment", file_name = "CPZ"),
  age = list(beta = beta_age, sig = sig_age, title = "Age at Inclusion", file_name = "Age"),
  sex = list(beta = beta_sex, sig = sig_sex, title = "Sex (male)", file_name = "Sex"),
  etiv = list(beta = beta_etiv, sig = sig_etiv, title = "Etiv", file_name = "Etiv"),
  interaction_cortical = list(beta = beta_interaction_cortical, sig = sig_interaction_cortical, title = expression("Time × Cortical Centiles "[BL]), file_name = "Interaction_cor"),
  interaction_subcortical = list(beta = beta_interaction_subcortical, sig = sig_interaction_subcortical, title = expression("Time × Subcortical Centiles "[BL]), file_name = "Interaction_sub")
)

# ---------------------------------------------------------------------------

# ADD A BLANK SPACE AFTER THE THIRD BAR

# Example new list to add
new_list <- list(new_space = NA)

# Define the function to add a new list after the third variable
add_blank_after_third <- function(result_list) {
  lapply(result_list, function(sublist) {
    # For each sublist, add the new_list after the third item
    sublist_modified <- c(sublist[1:3], new_list, sublist[4:length(sublist)])
    return(sublist_modified)
  })
}

# Apply the function to each item in result_types
result_types_modified <- lapply(result_types, add_blank_after_third)

# Check the modified structure
str(result_types_modified)

# ---------------------------------------------------------------------------

# Define y-limits for specific result_keys
y_limit_mapping <- list(
  "time" = c(-20, 20),
  "CPZ" = c(-20, 20),
  "default" = c(-5, 5)  # Default y-limits
)

for (result_key in names(result_types_modified)) {
  result <- result_types_modified[[result_key]]
  betas <- unlist(result$beta)
  sig <- unlist(result$sig)
  
  # Create a data frame for each variable    
  data <- data.frame(
    category <- c("Affective flattening", "Alogia", "Avolition", " ", "Anhedonia",
                  "Inattention", "SANS", "Hallucinations", "Delusions",
                  "Bizarre behaviour", "Formal thought", "SAPS", "GAF"),
    value = betas,
    sig = sig
  )
  
  # Define the desired order of categories
  desired_order <- c(
    "SANS", "SAPS", "GAF", " ", "Affective flattening", "Alogia", 
    "Avolition", "Anhedonia", "Inattention", "Hallucinations", 
    "Delusions", "Bizarre behaviour", "Formal thought" 
  ) 
  
  # Convert category to factor with desired order
  data$category <- factor(data$category, levels = desired_order)

  bar_colors <- c("#3B8B23", "#90336C", "#E97132", "white", rep("#3B8B23", 5), rep("#90336C", 4))
  
  # New column to the data that specifies the alpha value based on significance
  data$alpha <- ifelse(data$sig, "1", "0.5")  # 1 for significant (opaque), 0.5 for non-significant (semi-transparent)
  
  # Determine y-limits based on result_key
  if (result_key %in% names(y_limit_mapping)) {
    y_limits <- y_limit_mapping[[result_key]]
  } else {
    y_limits <- y_limit_mapping[["default"]]
  }
  
  # Create the bar chart
  plot <- ggplot(data, aes(x = category, y = value, fill = category, alpha = alpha)) +
    geom_bar(stat = "identity", width = 0.6) +  
    geom_bar(data = subset(data, sig == TRUE), aes(color = sig), fill = NA, stat = "identity", linewidth = 2, width = 0.6) +  # Highlight significant bars with thicker borders
    scale_fill_manual(values = setNames(bar_colors, levels(data$category)), guide = "none") +  # Define fixed colors for fill
    scale_color_manual(values = c("black"), guide = "none") +  # Remove legend for significant bars
    scale_alpha_manual(values = c("0.5" = 0.5, "1" = 1), guide = "none") +  # Explicitly set alpha values
    theme_minimal() +
    labs(title = result$title, x = NULL, y = "z values", fill = "z values") +
    ylim(y_limits) +  # Set y-axis limits
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 40),  # Center the title
      axis.title.x = element_blank(),          # Remove x-axis label
      axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, margin = margin(t = 0, r = 10, b = 0, l = 0), size = 36),  # Set y-axis label to "z values" vertically
      axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1, margin = margin(t = 0, r = 0, b = 20, l = 0), size = 30),  # Rotate x-axis labels and adjust margin
      axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1, margin = margin(t = 0, r = 10, b = 0, l = 0), size = 32)  # Align y-axis labels to the right
    )
  
  # Render the plot
  print(plot)
  
  # Save the plot to a specific folder as a PNG file
  ggsave(paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_covariates/",result$file_name,".png"), plot, width = 10, height = 8, bg = "white")
  
}
