
### ESTADISTICA PARA DIFERENCIAR ENTRE LA MUESTRA FINAL Y LOS PARTICIPANTES QUE ABANDONARON SEGUIMIENTO TRAS 1º EVALUACION

### SYMPTOM SAMPLE

## TOTAL CENTILES

# Datasets
A <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/centiles_CorticalMeasuresENIGMA_GrayAvg_completo_NUEVO.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx
B <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_symptoms_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Remove rows from A where 'participant' is in B
A_filtered <- A[!A$participant %in% B$participant, ]

A_baseline <- A_filtered[A_filtered$session == 1, ]
B_baseline <- B[B$Clinical.Assessment == 0, ]

# Perform the t-test
t_test_result_cortical <- t.test(A_baseline$centiles_GMV, B_baseline$centiles_GMV)
t_test_result_subcortical <- t.test(A_baseline$centiles_sGMV, B_baseline$centiles_sGMV)

# Print the t-test result
print(t_test_result_cortical)
print(t_test_result_subcortical)

## SAPS/SANS total

# Datasets
A <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Centiles_FEP_MRI_baseline_symtoms.csv',sep=';') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx
B <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_symptoms_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Remove rows from A where 'participant' is in B
A_filtered <- A[!A$SubjID %in% B$SubjID, ]

A_baseline <- A_filtered[A_filtered$Clinical.Assessment == 0, ]
B_baseline <- B[B$Clinical.Assessment == 0, ]

# Perform the wilcox test (non parametric)
wilcox_test_result_SANS <- wilcox.test(A_baseline$SANS.total, B_baseline$SANS.total)
wilcox_test_result_SAPS <- wilcox.test(A_baseline$SAPS.total, B_baseline$SAPS.total)

# Calculate mean values
mean_SANS_A <- mean(A_baseline$SANS.total, na.rm = TRUE)
mean_SANS_B <- mean(B_baseline$SANS.total, na.rm = TRUE)

mean_SAPS_A <- mean(A_baseline$SAPS.total, na.rm = TRUE)
mean_SAPS_B <- mean(B_baseline$SAPS.total, na.rm = TRUE)

# Print the wilcoxon-test result
print(wilcox_test_result_SANS)
cat("Mean SANS for A:", mean_SANS_A, "\n")
cat("Mean SANS for B:", mean_SANS_B, "\n")

print(wilcox_test_result_SAPS)
cat("Mean SAPS for A:", mean_SAPS_A, "\n")
cat("Mean SAPS for B:", mean_SAPS_B, "\n")

### FUNCTIONING SAMPLE

## TOTAL CENTILES

# Datasets
A <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/centiles_CorticalMeasuresENIGMA_GrayAvg_completo_NUEVO.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx
B <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_GAF_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Remove rows from A where 'participant' is in B
A_filtered <- A[!A$participant %in% B$participant, ]

A_baseline <- A_filtered[A_filtered$session == 1, ]
B_baseline <- B[B$Clinical.Assessment == 0, ]

# Perform the t-test
t_test_result_cortical <- t.test(A_baseline$centiles_GMV, B_baseline$Centiles_GMV)
t_test_result_subcortical <- t.test(A_baseline$centiles_sGMV, B_baseline$Centiles_sGMV)

# Print the t-test result
print(t_test_result_cortical)
print(t_test_result_subcortical)

## GAF

# Datasets
A <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Centiles_FEP_MRI_baseline_GAF.csv',sep=';') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx
B <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_GAF_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Remove rows from A where 'participant' is in B
A_filtered <- A[!A$SubjID %in% B$SubjID, ]

A_baseline <- A_filtered[A_filtered$Clinical.Assessment == 0, ]
B_baseline <- B[B$Clinical.Assessment == 0, ]

# Perform the wilcox test (non parametric)
wilcox_test_result_GAF <- wilcox.test(A_baseline$GAF_0, B_baseline$GAF)

# Calculate mean values
mean_GAF_A <- mean(A_baseline$GAF_0, na.rm = TRUE)
mean_GAF_B <- mean(B_baseline$GAF, na.rm = TRUE)

# Print the wilcoxon-test result
print(wilcox_test_result_GAF)
cat("Mean GAF for A:", mean_GAF_A, "\n")
cat("Mean GAF for B:", mean_GAF_B, "\n")

