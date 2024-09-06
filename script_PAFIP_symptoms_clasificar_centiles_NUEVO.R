

### SCRIPT PARA CLASIFICAR ALTOS Y BAJOS CENTILES (SYMPTOMS) 

# ATENCIÓN: para usar este script hay que traer el Workspace guardado en 'RESULTS_LME_PAFIP_symptoms_CONJUNTO.RData',
# o bien ejecutar la primera parte del script 'script_lme_PAFIP_symptoms_CONJUNTO.R'

### SPLIT THE SAMPLE INTO HIGHER AND LOWER WEIGHTED CENTILS 

# Initialize lists to store results
high_BLcentil <- list()
low_BLcentil <- list()

# MEDIAN

# Split into higher and lower groups based on centiles
high_BLcentil <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores[
  X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$lh.GM.superiortemporal.b2 > 0.5, ]

low_BLcentil <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores[
  X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$lh.GM.superiortemporal.b2 <= 0.5, ]

  ### PLOT THE RESULTS

  # SANS total
  
  # Define colors for points with transparency
  point_color_h <- adjustcolor("#FFB100", alpha.f = 0.5)  # Red with 50% transparency
  point_color_l <- adjustcolor("#919999", alpha.f = 0.5)  # Blue with 50% transparency  
  #point_color_h <- adjustcolor("#C00000", alpha.f = 0.5)  # Red with 50% transparency
  #point_color_l <- adjustcolor("#2165AB", alpha.f = 0.5)  # Blue with 50% transparency
  
  # Start capturing plot commands to a PNG file
  png(file = paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/SCATTER_PLOTS/Mediana_", "lh_superiortemporal_SANS_total", ".png"), width = 2500, height = 2000, res = 300)
  
  # Set the margins. Increase the left margin to provide more space for the y-axis label.
  par(mar = c(5, 5, 4, 2))  # Here the left margin (second value) is increased
  
  # Plot symptoms trajectories for high weighted centil values
  plot(high_BLcentil$Time_clinical2, high_BLcentil$SANS.total2,
       xlab = "Time (years)", ylab = "SANS score",
       main = "",
       pch = 16, cex = 1.9, col = point_color_h,
       ylim = c(-2, 7),
       xlim = c(0, 15),
       cex.lab = 2,      # Change font size for axis labels
       cex.main = 2,     # Change font size for main title
       cex.axis = 2)   # Change font size for axis values
  
  # Add regression line for high values
  lm_high <- lm(as.formula(paste("SANS.total2", "~ Time_clinical2")), data = high_BLcentil)
  abline(lm_high, col = "#C00000", lwd = 7)
  #abline(lm_high, col = "#C00000", lwd = 7)
  
  # Add low weighted values to the same plot
  points(low_BLcentil$Time_clinical2,low_BLcentil$SANS.total2,
         pch = 16, cex = 1.9, col = point_color_l)
  
  # Add regression line for low values
  lm_low <- lm(as.formula(paste("SANS.total2", "~ Time_clinical2")), data = low_BLcentil)
  abline(lm_low, col = "#2165AB", lwd = 7)
  #abline(lm_low, col = "#2165AB", lwd = 7)
  
  # Add legend
  legend("topright", legend = c("Centil >0.5", expression("Centil ≤ 0.5")), pch = 16, col = c("#C00000", "#2165AB"), pt.cex = 2, bty = "n", cex = 2)
  #legend("topright", legend = c("Centil >0.5", expression("Centil ≤ 0.5")), pch = 16, col = c("#C00000", "#2165AB"), pt.cex = 2, bty = "n", cex = 2)
  
  # Save the plot and close the device
  dev.off() 

  # SAPS total
  
  # Define colors for points with transparency
  point_color_h <- adjustcolor("#C00000", alpha.f = 0.5)  # Red with 50% transparency
  point_color_l <- adjustcolor("#2165AB", alpha.f = 0.5)  # Blue with 50% transparency
  
  # Start capturing plot commands to a PNG file
  png(file = paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/SCATTER_PLOTS/Mediana_", "lh_superiortemporal_SAPS_total", ".png"), width = 2500, height = 2000, res = 300)
  
  # Set the margins. Increase the left margin to provide more space for the y-axis label.
  par(mar = c(5, 5, 4, 2))  # Here the left margin (second value) is increased
  
  # Plot symptoms trajectories for high weighted centil values
  plot(high_BLcentil$Time_clinical2, high_BLcentil$SAPS.total2,
       xlab = "Time (years)", ylab = "SAPS score",
       main = "",
       pch = 16, cex = 1.9, col = point_color_h,
       ylim = c(-2, 7),
       xlim = c(0, 15),
       cex.lab = 2,      # Change font size for axis labels
       cex.main = 2,     # Change font size for main title
       cex.axis = 2)   # Change font size for axis values
  
  # Add regression line for high values
  lm_high <- lm(as.formula(paste("SAPS.total2", "~ Time_clinical2")), data = high_BLcentil)
  abline(lm_high, col = "#C00000", lwd = 7)
  
  # Add low weighted values to the same plot
  points(low_BLcentil$Time_clinical2,low_BLcentil$SAPS.total2,
         pch = 16, cex = 1.9, col = point_color_l)
  
  # Add regression line for low values
  lm_low <- lm(as.formula(paste("SAPS.total2", "~ Time_clinical2")), data = low_BLcentil)
  abline(lm_low, col = "#2165AB", lwd = 7)
  
  # Add legend
  legend("topright", legend = c("Centil >0.5", expression("Centil ≤ 0.5")), pch = 16, col = c("#C00000", "#2165AB"), pt.cex = 2, bty = "n", cex = 2)
  
  # Save the plot and close the device
  dev.off() 
  
  # QUARTILE
  
  # Initialize lists to store results
  high_BLcentil <- list()
  low_BLcentil <- list()
  
  # Split into higher and lower groups based on centiles
  high_BLcentil <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores[
    X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$lh.GM.superiortemporal.b2 > 0.75, ]
  
  low_BLcentil <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores[
    X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$lh.GM.superiortemporal.b2 < 0.25, ]
  
  ### PLOT THE RESULTS
  
  # SANS total
  
  # Define colors for points with transparency

  point_color_h <- adjustcolor("#ECA000", alpha.f = 0.85)  # Red with 50% transparency
  point_color_l <- adjustcolor("#7F7F7F", alpha.f = 0.65)  # Blue with 50% transparency
  #point_color_h <- adjustcolor("#C00000", alpha.f = 0.5)  # Red with 50% transparency
  #point_color_l <- adjustcolor("#2165AB", alpha.f = 0.5)  # Blue with 50% transparency
  
  # Start capturing plot commands to a PNG file
  png(file = paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/SCATTER_PLOTS/Q1-Q3_", "lh_superiortemporal_SANS_total", ".png"), width = 2500, height = 2000, res = 300)
  
  # Set the margins. Increase the left margin to provide more space for the y-axis label.
  par(mar = c(5, 5, 4, 2))  # Here the left margin (second value) is increased
  
  # Plot symptoms trajectories for high weighted centil values
  plot(high_BLcentil$Time_clinical2, high_BLcentil$SANS.total2,
       xlab = "Time (years)", ylab = "SANS score",
       main = "",
       pch = 16, cex = 1.9, col = point_color_h,
       ylim = c(-2, 7),
       xlim = c(0, 15),
       cex.lab = 2,      # Change font size for axis labels
       cex.main = 2,     # Change font size for main title
       cex.axis = 2)   # Change font size for axis values
  
  # Add regression line for high values
  lm_high <- lm(as.formula(paste("SANS.total2", "~ Time_clinical2")), data = high_BLcentil)
  abline(lm_high, col = "#ECA000", lwd = 10)
  #abline(lm_high, col = "#C00000", lwd = 7)
  
  # Add low weighted values to the same plot
  points(low_BLcentil$Time_clinical2,low_BLcentil$SANS.total2,
         pch = 16, cex = 1.9, col = point_color_l)
  
  # Add regression line for low values
  lm_low <- lm(as.formula(paste("SANS.total2", "~ Time_clinical2")), data = low_BLcentil)
  abline(lm_low, col = "#7F7F7F", lwd = 10)
  #abline(lm_low, col = "#2165AB", lwd = 7)
  
  # Add legend
  legend("topright", legend = c(expression("Centil "[BL] * " > 0.75"), expression("Centil "[BL] * " < 0.25")), pch = 16, col = c("#ECA000", "#7F7F7F"), pt.cex = 2.5, bty = "n", cex = 2)
  #legend("topright", legend = c(expression("Centil "[BL] * " > 0.75"), expression("Centil "[BL] * " < 0.25")), pch = 16, col = c("#C00000", "#2165AB"), pt.cex = 2, bty = "n", cex = 2)
  
  # Save the plot and close the device
  dev.off() 
  
  # SAPS total
  
  # Define colors for points with transparency
  point_color_h <- adjustcolor("#ECA000", alpha.f = 0.85)  # Red with 50% transparency
  point_color_l <- adjustcolor("#7F7F7F", alpha.f = 0.65)  # Blue with 50% transparency
  #point_color_h <- adjustcolor("#C00000", alpha.f = 0.5)  # Red with 50% transparency
  #point_color_l <- adjustcolor("#2165AB", alpha.f = 0.5)  # Blue with 50% transparency
  
  # Start capturing plot commands to a PNG file
  png(file = paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/SCATTER_PLOTS/Q1-Q3_", "lh_superiortemporal_SAPS_total", ".png"), width = 2500, height = 2000, res = 300)
  
  # Set the margins. Increase the left margin to provide more space for the y-axis label.
  par(mar = c(5, 5, 4, 2))  # Here the left margin (second value) is increased
  
  # Plot symptoms trajectories for high weighted centil values
  plot(high_BLcentil$Time_clinical2, high_BLcentil$SAPS.total2,
       xlab = "Time (years)", ylab = "SAPS score",
       main = "",
       pch = 16, cex = 1.9, col = point_color_h,
       ylim = c(-2, 7),
       xlim = c(0, 15),
       cex.lab = 2,      # Change font size for axis labels
       cex.main = 2,     # Change font size for main title
       cex.axis = 2)   # Change font size for axis values
  
  # Add regression line for high values
  lm_high <- lm(as.formula(paste("SAPS.total2", "~ Time_clinical2")), data = high_BLcentil)
  abline(lm_high, col = "#ECA000", lwd = 10)
  #abline(lm_high, col = "#C00000", lwd = 7)
  
  # Add low weighted values to the same plot
  points(low_BLcentil$Time_clinical2,low_BLcentil$SAPS.total2,
         pch = 16, cex = 1.9, col = point_color_l)
  
  # Add regression line for low values
  lm_low <- lm(as.formula(paste("SAPS.total2", "~ Time_clinical2")), data = low_BLcentil)
  abline(lm_low, col = "#7F7F7F", lwd = 10)
  #abline(lm_low, col = "#2165AB", lwd = 7)
  
  # Add legend
  legend("topright", legend = c(expression("Centil "[BL] * " > 0.75"), expression("Centil "[BL] * " < 0.25")), pch = 16, col = c("#ECA000", "#7F7F7F"), pt.cex = 2.5, bty = "n", cex = 2)
  #legend("topright", legend = c(expression("Centil "[BL] * " > 0.75"), expression("Centil "[BL] * " < 0.25")), pch = 16, col = c("#C00000", "#2165AB"), pt.cex = 2, bty = "n", cex = 2)
  
  # Save the plot and close the device
  dev.off() 
  
  # DECILE
  
  # Split into higher and lower groups based on centiles
  high_BLcentil <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores[
    X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$lh.GM.superiortemporal.b2 > 0.9, ]
  
  low_BLcentil <- X_Centiles_FEP_MRI_baseline_symptoms_raw_scores[
    X_Centiles_FEP_MRI_baseline_symptoms_raw_scores$lh.GM.superiortemporal.b2 < 0.1, ]
  
  ### PLOT THE RESULTS
  
  # SANS total
  
  # Define colors for points with transparency
  point_color_h <- adjustcolor("#C00000", alpha.f = 0.5)  # Red with 50% transparency
  point_color_l <- adjustcolor("#2165AB", alpha.f = 0.5)  # Blue with 50% transparency
  
  # Start capturing plot commands to a PNG file
  png(file = paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/SCATTER_PLOTS/D1-D9_", "lh_superiortemporal_SANS_total", ".png"), width = 2500, height = 2000, res = 300)
  
  # Set the margins. Increase the left margin to provide more space for the y-axis label.
  par(mar = c(5, 5, 4, 2))  # Here the left margin (second value) is increased
  
  # Plot symptoms trajectories for high weighted centil values
  plot(high_BLcentil$Time_clinical2, high_BLcentil$SANS.total2,
       xlab = "Time (years)", ylab = "SANS score",
       main = "",
       pch = 16, cex = 1.9, col = point_color_h,
       ylim = c(-2, 7),
       xlim = c(0, 15),
       cex.lab = 2,      # Change font size for axis labels
       cex.main = 2,     # Change font size for main title
       cex.axis = 2)   # Change font size for axis values
  
  # Add regression line for high values
  lm_high <- lm(as.formula(paste("SANS.total2", "~ Time_clinical2")), data = high_BLcentil)
  abline(lm_high, col = "#C00000", lwd = 7)
  
  # Add low weighted values to the same plot
  points(low_BLcentil$Time_clinical2,low_BLcentil$SANS.total2,
         pch = 16, cex = 1.9, col = point_color_l)
  
  # Add regression line for low values
  lm_low <- lm(as.formula(paste("SANS.total2", "~ Time_clinical2")), data = low_BLcentil)
  abline(lm_low, col = "#2165AB", lwd = 7)
  
  # Add legend
  legend("topright", legend = c(expression("Centil "[BL] * " > 0.9"), expression("Centil "[BL] * " < 0.1")), pch = 16, col = c("#C00000", "#2165AB"), pt.cex = 2, bty = "n", cex = 2)
  
  # Save the plot and close the device
  dev.off() 
  
  # SAPS total
  
  # Define colors for points with transparency
  point_color_h <- adjustcolor("#C00000", alpha.f = 0.5)  # Red with 50% transparency
  point_color_l <- adjustcolor("#2165AB", alpha.f = 0.5)  # Blue with 50% transparency
  
  # Start capturing plot commands to a PNG file
  png(file = paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/SCATTER_PLOTS/D1-D9_", "lh_superiortemporal_SAPS_total", ".png"), width = 2500, height = 2000, res = 300)
  
  # Set the margins. Increase the left margin to provide more space for the y-axis label.
  par(mar = c(5, 5, 4, 2))  # Here the left margin (second value) is increased
  
  # Plot symptoms trajectories for high weighted centil values
  plot(high_BLcentil$Time_clinical2, high_BLcentil$SAPS.total2,
       xlab = "Time (years)", ylab = "SAPS score",
       main = "",
       pch = 16, cex = 1.9, col = point_color_h,
       ylim = c(-2, 7),
       xlim = c(0, 15),
       cex.lab = 2,      # Change font size for axis labels
       cex.main = 2,     # Change font size for main title
       cex.axis = 2)   # Change font size for axis values
  
  # Add regression line for high values
  lm_high <- lm(as.formula(paste("SAPS.total2", "~ Time_clinical2")), data = high_BLcentil)
  abline(lm_high, col = "#C00000", lwd = 7)
  
  # Add low weighted values to the same plot
  points(low_BLcentil$Time_clinical2,low_BLcentil$SAPS.total2,
         pch = 16, cex = 1.9, col = point_color_l)
  
  # Add regression line for low values
  lm_low <- lm(as.formula(paste("SAPS.total2", "~ Time_clinical2")), data = low_BLcentil)
  abline(lm_low, col = "#2165AB", lwd = 7)
  
  # Add legend
  legend("topright", legend = c(expression("Centil "[BL] * " > 0.9"), expression("Centil "[BL] * " < 0.1")), pch = 16, col = c("#C00000", "#2165AB"), pt.cex = 2, bty = "n", cex = 2)
  
  # Save the plot and close the device
  dev.off() 
  
  