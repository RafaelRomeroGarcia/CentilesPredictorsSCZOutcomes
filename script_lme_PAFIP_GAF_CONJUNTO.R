
# Longitudinal trajectories of GAF (Global Assessment of Functioning) explained by baseline MRI centils and time from baseline
# -----------------------------------------------------------------------------------------------

#library(lme4)
library(lmerTest)

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

# Rescale centils of each cortical brain region to z scores (x values)
brain_regions_pre <- c(
  "lh.GM.bankssts.b", "lh.GM.caudalanteriorcingulate.b", "lh.GM.caudalmiddlefrontal.b", 
  "lh.GM.cuneus.b", "lh.GM.entorhinal.b", "lh.GM.frontalpole.b", "lh.GM.fusiform.b", "lh.GM.inferiorparietal.b", 
  "lh.GM.inferiortemporal.b", "lh.GM.insula.b", "lh.GM.isthmuscingulate.b", "lh.GM.lateraloccipital.b", 
  "lh.GM.lateralorbitofrontal.b", "lh.GM.lingual.b", "lh.GM.medialorbitofrontal.b", "lh.GM.middletemporal.b", 
  "lh.GM.paracentral.b", "lh.GM.parahippocampal.b", "lh.GM.parsopercularis.b", "lh.GM.parsorbitalis.b", 
  "lh.GM.parstriangularis.b", "lh.GM.pericalcarine.b", "lh.GM.postcentral.b", "lh.GM.posteriorcingulate.b", 
  "lh.GM.precentral.b", "lh.GM.precuneus.b", "lh.GM.rostralanteriorcingulate.b", "lh.GM.rostralmiddlefrontal.b", 
  "lh.GM.superiorfrontal.b", "lh.GM.superiorparietal.b", "lh.GM.superiortemporal.b", "lh.GM.supramarginal.b", 
  "lh.GM.temporalpole.b", "lh.GM.transversetemporal.b",
  "rh.GM.bankssts.b", "rh.GM.caudalanteriorcingulate.b", "rh.GM.caudalmiddlefrontal.b", 
  "rh.GM.cuneus.b", "rh.GM.entorhinal.b", "rh.GM.frontalpole.b", "rh.GM.fusiform.b", "rh.GM.inferiorparietal.b", 
  "rh.GM.inferiortemporal.b", "rh.GM.insula.b", "rh.GM.isthmuscingulate.b", "rh.GM.lateraloccipital.b", 
  "rh.GM.lateralorbitofrontal.b", "rh.GM.lingual.b", "rh.GM.medialorbitofrontal.b", "rh.GM.middletemporal.b", 
  "rh.GM.paracentral.b", "rh.GM.parahippocampal.b", "rh.GM.parsopercularis.b", "rh.GM.parsorbitalis.b", 
  "rh.GM.parstriangularis.b", "rh.GM.pericalcarine.b", "rh.GM.postcentral.b", "rh.GM.posteriorcingulate.b", 
  "rh.GM.precentral.b", "rh.GM.precuneus.b", "rh.GM.rostralanteriorcingulate.b", "rh.GM.rostralmiddlefrontal.b", 
  "rh.GM.superiorfrontal.b", "rh.GM.superiorparietal.b", "rh.GM.superiortemporal.b", "rh.GM.supramarginal.b", 
  "rh.GM.temporalpole.b", "rh.GM.transversetemporal.b", "lh.GM.Thalamus.Proper", "lh.GM.Caudate", "lh.GM.Putamen", "lh.GM.Pallidum",
  "lh.GM.Hippocampus", "lh.GM.Amygdala", "lh.GM.Accumbens.area",
  "rh.GM.Thalamus.Proper", "rh.GM.Caudate", "rh.GM.Putamen", "rh.GM.Pallidum",
  "rh.GM.Hippocampus", "rh.GM.Amygdala", "rh.GM.Accumbens.area"
)

# Hacer copia de los centiles antes de escalar a valores z
for (region in brain_regions_pre) {
  X_Centiles_FEP_MRI_baseline_GAF_raw_scores[[paste0(region,"2")]] <- X_Centiles_FEP_MRI_baseline_GAF_raw_scores[[region]]
}

# Convertir centiles en z scores
for (region in brain_regions_pre) {
  X_Centiles_FEP_MRI_baseline_GAF_raw_scores[[region]] <- scale(X_Centiles_FEP_MRI_baseline_GAF_raw_scores[[region]])
}

### LINEAR MIXED EFFECT (LME) MODEL

# Define a vector of variables you want to analyze
variables <- c(
  "GAF"
)

# Define a vector of brain region names
brain_regions <- c(
  "bankssts.b", "caudalanteriorcingulate.b", "caudalmiddlefrontal.b", 
  "cuneus.b", "entorhinal.b", "fusiform.b", "inferiorparietal.b", 
  "inferiortemporal.b", "isthmuscingulate.b", "lateraloccipital.b", 
  "lateralorbitofrontal.b", "lingual.b", "medialorbitofrontal.b", "middletemporal.b", 
  "parahippocampal.b", "paracentral.b", "parsopercularis.b", "parsorbitalis.b", 
  "parstriangularis.b", "pericalcarine.b", "postcentral.b", "posteriorcingulate.b", 
  "precentral.b", "precuneus.b", "rostralanteriorcingulate.b", "rostralmiddlefrontal.b", 
  "superiorfrontal.b", "superiorparietal.b", "superiortemporal.b", "supramarginal.b", 
  "frontalpole.b", "temporalpole.b", "transversetemporal.b", "insula.b", "Accumbens.area",
  "Amygdala", "Caudate", "Hippocampus", "Pallidum", "Putamen", "Thalamus.Proper" 
)

# Initialize empty lists to store results for each variable
results_BL_centils <- list()
results_time <- list()
results_CPZ <- list()
results_interaction <- list()

# Loop through each variable
for (var in variables) {
  # Initialize empty lists to store results for this variable
  beta_values_BL_centils <- list()
  p_values_BL_centils <- list()
  beta_values_time <- list()
  p_values_time <- list()
  beta_values_CPZ <- list()
  p_values_CPZ <- list()
  beta_values_interaction <- list()
  p_values_interaction <- list()
  
  # Loop through each brain region and fit the model
  for (region in brain_regions) {
    
    # Fit the LME model for left hemisphere GM
    model_lh.GM <- glmer(
      formula = as.formula(
        paste0(var, " ~ lh.GM.", region, "*Time_clinical + CPZ + Age_inclusion + sex + etiv_residuals + (1 | participant)")), family = poisson(),
      data = X_Centiles_FEP_MRI_baseline_GAF_raw_scores,
      ,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
    )
    print(summary(model_lh.GM))
    
    # Fit the LME model for right hemisphere GM
    model_rh.GM <- glmer(
      formula = as.formula(
        paste0(var, " ~ rh.GM.", region, "*Time_clinical + CPZ + Age_inclusion + sex + etiv_residuals + (1 | participant)")), family = poisson(),
      data = X_Centiles_FEP_MRI_baseline_GAF_raw_scores,
      ,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
    )
    print(summary(model_rh.GM))
    
    # Store beta and p-values in lists
    beta_values_BL_centils[[region]] <- c(coef(summary(model_lh.GM))[2,3], coef(summary(model_rh.GM))[2,3])
    p_values_BL_centils[[region]] <- c(coef(summary(model_lh.GM))[2,4], coef(summary(model_rh.GM))[2,4])
    beta_values_time[[region]] <- c(coef(summary(model_lh.GM))[3,3], coef(summary(model_rh.GM))[3,3])
    p_values_time[[region]] <- c(coef(summary(model_lh.GM))[3,4], coef(summary(model_rh.GM))[3,4])
    beta_values_CPZ[[region]] <- c(coef(summary(model_lh.GM))[4,3], coef(summary(model_rh.GM))[4,3])
    p_values_CPZ[[region]] <- c(coef(summary(model_lh.GM))[4,4], coef(summary(model_rh.GM))[4,4])
    beta_values_interaction[[region]] <- c(coef(summary(model_lh.GM))[8,3], coef(summary(model_rh.GM))[8,3])
    p_values_interaction[[region]] <- c(coef(summary(model_lh.GM))[8,4], coef(summary(model_rh.GM))[8,4])
  }
  
  # Store results for this variable
  results_BL_centils[[var]] <- list(beta_values = beta_values_BL_centils, p_values = p_values_BL_centils)
  results_time[[var]] <- list(beta_values = beta_values_time, p_values = p_values_time)
  results_CPZ[[var]] <- list(beta_values = beta_values_CPZ, p_values = p_values_CPZ)
  results_interaction[[var]] <- list(beta_values = beta_values_interaction, p_values = p_values_interaction)
}

# ----------------------------------------------------------------------------------------

### ARRANGE BETA AND P VALUES IN A SINGLE DATA FRAME COMPATIBLE WITH 'script_regional_brainmap_representation_borders_Natalia' below

# Initialize an empty list to store results for each variable
results_list_BL_centils <- list()
results_list_time <- list()
results_list_CPZ <- list()
results_list_interaction <- list()

# Initialize empty vectors to store concatenated p values for all comparisons (68x11)
all_ps_BL_centils <- c()
all_ps_time <- c()
all_ps_CPZ <- c()
all_ps_interaction <- c()

for (var in variables) {
  
  # Initialize empty vectors to store concatenated values
  first_betas_BL_centils <- c()
  second_betas_BL_centils <- c()
  first_ps_BL_centils <- c()
  second_ps_BL_centils <- c()
  
  first_betas_time <- c()
  second_betas_time <- c()
  first_ps_time <- c()
  second_ps_time <- c()
  
  first_betas_CPZ <- c()
  second_betas_CPZ <- c()
  first_ps_CPZ <- c()
  second_ps_CPZ <- c()
  
  first_betas_interaction <- c()
  second_betas_interaction <- c()
  first_ps_interaction <- c()
  second_ps_interaction <- c()
  
  # Loop through each brain region
  for (region in brain_regions) {
    # Extract the first and second numeric values for the current brain region
    first_beta_BL_centils <- results_BL_centils[[var]]$beta_values[[region]][1]
    second_beta_BL_centils <- results_BL_centils[[var]]$beta_values[[region]][2]
    first_p_BL_centils <- results_BL_centils[[var]]$p_values[[region]][1]
    second_p_BL_centils <- results_BL_centils[[var]]$p_values[[region]][2]
    
    first_beta_time <- results_time[[var]]$beta_values[[region]][1]
    second_beta_time <- results_time[[var]]$beta_values[[region]][2]
    first_p_time <- results_time[[var]]$p_values[[region]][1]
    second_p_time <- results_time[[var]]$p_values[[region]][2]
    
    first_beta_CPZ <- results_CPZ[[var]]$beta_values[[region]][1]
    second_beta_CPZ <- results_CPZ[[var]]$beta_values[[region]][2]
    first_p_CPZ <- results_CPZ[[var]]$p_values[[region]][1]
    second_p_CPZ <- results_CPZ[[var]]$p_values[[region]][2]
    
    first_beta_interaction <- results_interaction[[var]]$beta_values[[region]][1]
    second_beta_interaction <- results_interaction[[var]]$beta_values[[region]][2]
    first_p_interaction <- results_interaction[[var]]$p_values[[region]][1]
    second_p_interaction <- results_interaction[[var]]$p_values[[region]][2]
    
    # Append the first and second numeric values to their respective vectors
    first_betas_BL_centils <- c(first_betas_BL_centils, first_beta_BL_centils)
    second_betas_BL_centils <- c(second_betas_BL_centils, second_beta_BL_centils)
    first_ps_BL_centils <- c(first_ps_BL_centils, first_p_BL_centils)
    second_ps_BL_centils <- c(second_ps_BL_centils, second_p_BL_centils)
    
    first_betas_time <- c(first_betas_time, first_beta_time)
    second_betas_time <- c(second_betas_time, second_beta_time)
    first_ps_time <- c(first_ps_time, first_p_time)
    second_ps_time <- c(second_ps_time, second_p_time)    
    
    first_betas_CPZ <- c(first_betas_CPZ, first_beta_CPZ)
    second_betas_CPZ <- c(second_betas_CPZ, second_beta_CPZ)
    first_ps_CPZ <- c(first_ps_CPZ, first_p_CPZ)
    second_ps_CPZ <- c(second_ps_CPZ, second_p_CPZ) 
    
    first_betas_interaction <- c(first_betas_interaction, first_beta_interaction)
    second_betas_interaction <- c(second_betas_interaction, second_beta_interaction)
    first_ps_interaction <- c(first_ps_interaction, first_p_interaction)
    second_ps_interaction <- c(second_ps_interaction, second_p_interaction)
  }
  
  # FOR BL_CENTILS
  
  # Concatenate the first and second numeric values
  values <- c(first_betas_BL_centils, second_betas_BL_centils)
  p <- c(first_ps_BL_centils, second_ps_BL_centils)
  
  # *** Perform FDR correction on p-values for 68+14=82 comparisons (total brain regions)
  p_fdr <- p.adjust(p, method = "fdr")
  # Convert 'sig' values to TRUE if higher than 0.05, FALSE otherwise
  sig <- c(p_fdr < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_BL_centils[[var]] <- data
  
  # FOR TIME
  
  # Concatenate the first and second numeric values
  values <- c(first_betas_time, second_betas_time)
  p <- c(first_ps_time, second_ps_time)
  
  # *** Perform FDR correction on p-values for 68+14=82 comparisons (total brain regions)
  p_fdr <- p.adjust(p, method = "fdr")
  # Convert 'sig' values to TRUE if higher than 0.05, FALSE otherwise
  sig <- c(p_fdr < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_time[[var]] <- data
  
  # FOR CPZ
  
  # Concatenate the first and second numeric values
  values <- c(first_betas_CPZ, second_betas_CPZ)
  p <- c(first_ps_CPZ, second_ps_CPZ)
  
  # *** Perform FDR correction on p-values for 68+14=82 comparisons (total brain regions)
  p_fdr <- p.adjust(p, method = "fdr")
  # Convert 'sig' values to TRUE if higher than 0.05, FALSE otherwise
  sig <- c(p_fdr < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_CPZ[[var]] <- data
  
  # FOR INTERACTION
  
  values <- c(first_betas_interaction, second_betas_interaction)
  p <- c(first_ps_interaction, second_ps_interaction)
  
  # *** Perform FDR correction on p-values for 68+14=82 comparisons (total brain regions)
  p_fdr <- p.adjust(p, method = "fdr")
  # Convert 'sig' values to TRUE if higher than 0.05, FALSE otherwise
  sig <- c(p_fdr < 0.05)
  # Combine beta and p values (true or false) into a single data frame for each symptom
  data <- data.frame(values, p, p_fdr, sig) 
  # Store the results for this variable in the results list
  results_list_interaction[[var]] <- data
}

  # SEPARATE CORTICAL AND SUBCORTICAL DATA FOR PLOTTING 
  
  # Initialize empty vectors to store concatenated values
  results_list_BL_centils_cor <- c()
  results_list_time_cor <- c()
  results_list_CPZ_cor <- c()
  results_list_interaction_cor <- c()
  
  results_list_BL_centils_sub <- c()
  results_list_time_sub <- c()
  results_list_CPZ_sub <- c()
  results_list_interaction_sub <- c()
  
  for (var in variables) {
    # Define the indices to be concatenated
    indices_cor <- c(1:34, 42:75)
    indices_sub <- c(35:41, 76:82)
    
    # Fill the vectors
    results_list_BL_centils_cor[[var]] <- results_list_BL_centils[[var]][indices_cor, ] # cortical
    results_list_time_cor[[var]] <- results_list_time[[var]][indices_cor, ]
    results_list_CPZ_cor[[var]] <- results_list_CPZ[[var]][indices_cor, ]
    results_list_interaction_cor[[var]] <- results_list_interaction[[var]][indices_cor, ]
    
    results_list_BL_centils_sub[[var]] <- results_list_BL_centils[[var]][indices_sub, ] # subcortical
    results_list_time_sub[[var]] <- results_list_time[[var]][indices_sub, ]
    results_list_CPZ_sub[[var]] <- results_list_CPZ[[var]][indices_sub, ]
    results_list_interaction_sub[[var]] <- results_list_interaction[[var]][indices_sub, ]
  }

# --------------------------------------------------------------------------------------

##### CORTICAL PLOTS #####


### APPLY NATALIA'S FUNCTION FOR REGIONAL BRAINMAP REPRESENTATION BORDERS

# 'script_regional_brainmap_representation_borders_Natalia'

#  Copyright (C) 2023 University of Seville
# 
#  Written by Natalia García San Martín (ngarcia1@us.es)

regional_brainmap_representation_borders <- function(data,title,sup_lim,inf_lim,midd_p,position){
  
  library(dplyr)
  library(ggplot2)
  library(ggseg)
  
  zones <- c( 'bankssts',
              'caudal anterior cingulate',
              'caudal middle frontal',
              'cuneus',
              'entorhinal',
              'fusiform',
              'inferior parietal',
              'inferior temporal',
              'isthmus cingulate',
              'lateral occipital',
              'lateral orbitofrontal',
              'lingual',
              'medial orbitofrontal',
              'middle temporal',
              'parahippocampal',
              'paracentral',
              'pars opercularis',
              'pars orbitalis',
              'pars triangularis',
              'pericalcarine',
              'postcentral',
              'posterior cingulate',
              'precentral',
              'precuneus',
              'rostral anterior cingulate',
              'rostral middle frontal',
              'superior frontal',
              'superior parietal',
              'superior temporal',
              'supramarginal',
              'frontal pole',
              'temporal pole',
              'transverse temporal',
              'insula')
  
  if(nrow(data) == 34){
    someData <- tibble(
      region = zones, 
      values = data$values,
      significant = data$sig,
      groups = c(rep(title, nrow(data)))
    )
    
  }else if (nrow(data) == 68){
    someData <- tibble(
      label = c(paste0('lh_',gsub(' ','',zones)),paste0('rh_',gsub(' ','',zones))),
      values = data$values,
      significant = data$sig,
      groups = c(rep(title, nrow(data)))
    )
  }
  
  if (any(someData$significant) && any(!someData$significant)){
    borders <- c("grey", "black")
  }else if (all(someData$significant)){
    borders <- "black"
  }else{
    borders <- "grey"
  }
  
  # Brain map configuration
  someData %>%
    group_by(groups) %>%
    ggplot() + scale_fill_gradient2(
      
      #low = "blue",
      #mid = "white",
      #high = "red",
      
      #low = "#003366",
      #mid = "white",
      #high = "#B71001",
      
      low = "#0e4d92",
      mid = "white",
      high = "#B71001",
      
      #low = "#104E8B",
      #mid = "white",
      #high = "#EE2C2C",
      
      # low = "white",
      # mid = "turquoise4",
      # high = "#005654",
      
      # low = "white",
      # mid = "#FF97CB",
      # high = "#E60073",
      midpoint = midd_p,
      limits=c(inf_lim,sup_lim),
      space = "Lab",
      na.value = "grey50",
      # guide = guide_colorbar(title = "age (years)"),
      guide = guide_colorbar(
        title = if (position == 'vertical') 'z values' else NULL,
        direction = position,
        barwidth = if (position == 'horizontal') 10 else NULL,
        barheight = if (position == 'vertical') 8 else NULL,
        
        label.theme = element_text(color = "black"),
        title.theme = element_text(color = "black")),
      # guide = "colourbar",
      aesthetics = "fill"
    ) +
    geom_brain(
      mapping = aes(fill = values, col = significant,alpha = significant, size = significant),
      atlas = dk, 
      position = position_brain(hemi ~ side),
      show.legend = TRUE) +
    scale_colour_manual(values=borders) +
    # scale_colour_manual(values=ifelse(any(someData$significant) && any(!someData$significant), c("grey", "black"), ifelse(all(someData$significant), "black", "grey"))) +
    # scale_colour_manual(values=c("grey", "black")) +
    # scale_colour_manual(values=c( "black")) +
    scale_alpha_manual(values= c(0.6,1)) +
    scale_size_manual(values= c(0.3,0.6)) + 
    facet_wrap(~groups)+theme_brain2(plot.background = "white")
  
}

### PLOT LME RESULTS ON THE REGIONAL BRAINMAP

## BL CENTILS 82 comparisons

# Call the function to generate the plot
plot <- regional_brainmap_representation_borders(results_list_BL_centils_cor[[var]], var, 10, -10, 0, "vertical") # FDR corrected for 68 comparisons

# Render the plot
print(plot)

# Save the plot to a specific folder as a PNG file
ggsave(paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF/BL_CENTILS_FDR_82_comparisons.png"), plot)

## TIME 82 comparisons

# Call the function to generate the plot
plot <- regional_brainmap_representation_borders(results_list_time_cor[[var]], var, 15, -15, 0, "vertical") # FDR corrected for 82 comparisons

# Render the plot
print(plot)

# Save the plot to a specific folder as a PNG file
ggsave(paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF/TIME_FDR_82_comparisons.png"), plot)

## CPZ 82 comparisons

# Call the function to generate the plot
plot <- regional_brainmap_representation_borders(results_list_CPZ_cor[[var]], var, 15, -15, 0, "vertical") # FDR corrected for 82 comparisons

# Render the plot
print(plot)

# Save the plot to a specific folder as a PNG file
ggsave(paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF/CPZ_FDR_82_comparisons.png"), plot)


## INTERACTION 82 comparisons

# Call the function to generate the plot
plot <- regional_brainmap_representation_borders(results_list_interaction_cor[[var]], var, 15, -15, 0, "vertical") # FDR corrected for 82 comparisons

# Render the plot
print(plot)

# Save the plot to a specific folder as a PNG file
ggsave(paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF/INTERACTION_FDR_82_comparisons.png"), plot)

# -------------------------------------------------------------------------------------


##### SUBCORTICAL PLOTS #####

### PLOT RESULTS AS AN HORIZONTAL BAR CHART

library(ggplot2)

# Initialize lists to store beta and sig values
beta_sub_BL_centils <- list()
beta_sub_time <- list()
beta_sub_CPZ <- list()
beta_sub_interaction <- list()

sig_sub_BL_centils <- list()
sig_sub_time <- list()
sig_sub_CPZ <- list()
sig_sub_interaction <- list()

# Initialize result_types list
result_types <- list()

for (var in variables) {
  
  beta_sub_BL_centils[[var]] <- c(results_list_BL_centils_sub[[var]]$values)
  beta_sub_time[[var]] <- c(results_list_time_sub[[var]]$values)
  beta_sub_CPZ[[var]] <- c(results_list_CPZ_sub[[var]]$values)
  beta_sub_interaction[[var]] <- c(results_list_interaction_sub[[var]]$values)
  
  sig_sub_BL_centils[[var]] <- c(results_list_BL_centils_sub[[var]]$sig)
  sig_sub_time[[var]] <- c(results_list_time_sub[[var]]$sig)
  sig_sub_CPZ[[var]] <- c(results_list_CPZ_sub[[var]]$sig)
  sig_sub_interaction[[var]] <- c(results_list_interaction_sub[[var]]$sig)
  
  # Define the result types and their respective file names and titles
  result_types <- list(
    BL_centils = list(beta = beta_sub_BL_centils[[var]], sig = sig_sub_BL_centils[[var]], title = "GAF - Centils", file_name = "BL_centils"),
    time = list(beta = beta_sub_time[[var]], sig = sig_sub_time[[var]], title = "GAF - Time", file_name = "time"),
    CPZ = list(beta = beta_sub_CPZ[[var]], sig = sig_sub_CPZ[[var]], title = "GAF - CPZ", file_name = "CPZ"),
    interaction = list(beta = beta_sub_interaction[[var]], sig = sig_sub_interaction[[var]], title = "GAF - Interaction", file_name = "interaction")
  )
  
  for (result_key in names(result_types)) {
    result <- result_types[[result_key]]
    betas <- result$beta
    sig <- result$sig
    
    # Sample data
    data <- data.frame(
      category = c(
        "L Thalamus", "L Caudate", "L Putamen", "L Pallidum", "L Hippocampus", "L Amygdala", "L Accumbens", 
        "R Thalamus", "R Caudate", "R Putamen", "R Pallidum", "R Hippocampus", "R Amygdala", "R Accumbens"
      ),
      value = betas,
      sig = sig
    )
    
    # Determine the x-axis limits and scale_fill_gradient2 limits
    if (result$title == "GAF - CPZ" || result$title == "GAF - Time") {
      x_limits <- c(-15, 15)
      fill_limits <- c(-15, 15)
    } else {
      x_limits <- c(-5, 5)
      fill_limits <- c(-5, 5)
    }
    
    # Define the desired order of categories
    desired_order <- c(
      "R Accumbens", "L Accumbens", "R Amygdala", "L Amygdala",
      "R Hippocampus", "L Hippocampus", "R Pallidum", "L Pallidum",
      "R Putamen", "L Putamen", "R Caudate", "L Caudate",
      "R Thalamus", "L Thalamus"
    )
    
    # Convert category to factor with desired order
    data$category <- factor(data$category, levels = desired_order)
    
    # Create the horizontal bar chart
    plot <- ggplot(data, aes(x = value, y = category)) +
      geom_bar(aes(fill = value), stat = "identity", width = 0.5) +  # Blue-red gradient for fill
      geom_bar(data = subset(data, sig == TRUE), aes(color = sig), fill = NA, stat = "identity", linewidth = 0.75, width = 0.5) +  # Highlight significant bars with thicker borders
      scale_fill_gradient2(low = "#0e4d92", mid = "white", high = "#B71001", midpoint = 0, limits = fill_limits) + 
      scale_color_manual(values = c("black"), guide = "none") +  # Remove legend for significant bars
      theme_minimal() +
      labs(title = result$title, x = NULL, y = NULL, fill = "z values") +
      xlim(x_limits) +  # Set x-axis limits
      theme(
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_blank(),          # Remove x-axis label
        axis.title.y = element_blank()           # Remove y-axis label
      )
    
    # Render the plot
    print(plot)
    
    # Save the plot to a specific folder as a PNG file
    ggsave(paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF/SUBCORTICAL_FDR_82_comparisons/", result$file_name, ".png"), plot, bg = "white")
  }
}
