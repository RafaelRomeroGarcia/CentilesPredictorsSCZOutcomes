
### HISTOGRAMS

## SANS/SAPS

# Load necessary package
library(ggplot2)

# Read the CSV file into a dataframe (modify the path and separator if needed)
df <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_symptoms_raw_scores_2.csv')

# Define a vector of variables you want to analyze
variables <- c(
  "SANS.affect.flatt", "SANS.alogia", "SANS.avolition", "SANS.anhedonia",
  "SANS.attention", "SANS.total", "SAPS.hallucinations", "SAPS.delusions",
  "SAPS.bizarre.behav", "SAPS.formal.thought", "SAPS.total"
)

# Loop through each variable
for (var in variables) {
  
  # Create the plot and assign it to an object
  plot <- ggplot(df, aes(x = .data[[var]])) + # Use .data[[var]] for tidy evaluation
    geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
    #ggtitle(var) + # Set the title to the column name
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 32), # Increase x-axis title size
      axis.title.y = element_text(size = 32),  # Increase y-axis title size
      axis.text.x = element_text(size = 24),  # Increase x-axis labels size
      axis.text.y = element_text(size = 24)   # Increase y-axis labels size
    )
  
  # Save the plot to a specific folder as a TIFF file
  ggsave(
    filename = paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_histograms/", var, ".png"),
    plot = plot, # Use the plot object here
    width = 8, 
    height = 6,
    dpi = 300 # Adjust resolution if needed
  )
}

## GAF

# Load necessary package
library(ggplot2)

# Read the CSV file into a dataframe (modify the path and separator if needed)
df <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_GAF_raw_scores_2.csv')

# Define a vector of variables you want to analyze
variables <- c(
  "GAF"
)

# Loop through each variable
for (var in variables) {
  
  # Create the plot and assign it to an object
  plot <- ggplot(df, aes(x = .data[[var]])) + # Use .data[[var]] for tidy evaluation
    geom_histogram(binwidth = 20, fill = "skyblue", color = "black") +
    #ggtitle(var) + # Set the title to the column name
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 32), # Increase x-axis title size
      axis.title.y = element_text(size = 32),  # Increase y-axis title size
      axis.text.x = element_text(size = 24),  # Increase x-axis labels size
      axis.text.y = element_text(size = 24)   # Increase y-axis labels size
    )
  
  # Save the plot to a specific folder as a TIFF file
  ggsave(
    filename = paste0("/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_histograms/", var, ".png"),
    plot = plot, # Use the plot object here
    width = 8, 
    height = 6,
    dpi = 300 # Adjust resolution if needed
  )
}
