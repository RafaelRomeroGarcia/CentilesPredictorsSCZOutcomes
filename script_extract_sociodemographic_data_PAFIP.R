
### SANS/SAPS

# Datasets
A <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Covariates_complete copia 2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx
B <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_symptoms_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Keep only rows in A where the first column matches the first column in B
A_filtered <- A[A[,1] %in% B[,1], ]

df_baseline <- A_filtered[A_filtered$Assessment == 1, ]
df_year_1 <- A_filtered[A_filtered$Assessment == 2, ]
df_year_1 <- df_year_1[!is.nan(df_year_1$SANS_Total), ]
df_year_3 <- A_filtered[A_filtered$Assessment == 3, ]
df_year_3 <- df_year_3[!is.nan(df_year_3$SANS_Total), ]
df_year_10 <- A_filtered[A_filtered$Assessment == 10, ]
df_year_10 <- df_year_10[!is.nan(df_year_10$SANS_Total), ]

# BASELINE

# Calculate mean and standard deviation for 'Age_inclusion'
mean_Age_inclusion <- mean(df_baseline$Age_inclusion, na.rm = TRUE)
sd_Age_inclusion <- sd(df_baseline$Age_inclusion, na.rm = TRUE)

# Calculate number of males and females
number_of_females <- sum(df_baseline$Sex, na.rm = TRUE)
total_individuals <- sum(!is.na(df_baseline$Sex))
number_of_males <- total_individuals - number_of_females
# Calculate the percentage of females and males
percentage_females <- (number_of_females / total_individuals) * 100
percentage_males <- (number_of_males / total_individuals) * 100

# Calculate family history of psychosis
history_of_psychosis <- sum(df_baseline$Family_History_of_Psychosis, na.rm = TRUE)
total_individuals <- sum(!is.na(df_baseline$Family_History_of_Psychosis))
# Calculate the percentage of patients with family history of psychosis
percentage_history_psychosis <- (history_of_psychosis / total_individuals) * 100

# Calculate low socioeconomic status
low_socioeconomic_status <- sum(df_baseline$Socio_Economic_Status == 'Low', na.rm = TRUE)
total_individuals <- sum(!is.na(df_baseline$Socio_Economic_Status))
# Calculate the percentage of patients with family history of psychosis
percentage_low_socioeconomic_status <- (low_socioeconomic_status / total_individuals) * 100

# Calculate diagnosis_6m
schizophrenia_6m <- sum(df_baseline$Diagnosis_6m == 'schizophrenia', na.rm = TRUE)
schizophreniform_6m <- sum(df_baseline$Diagnosis_6m == 'schizophreniform', na.rm = TRUE)
schizoaffective_6m <- sum(df_baseline$Diagnosis_6m == 'schizoaffective_disorder', na.rm = TRUE)
brief_psychosis_6m <- sum(df_baseline$Diagnosis_6m == 'brief_psychotic_disorder', na.rm = TRUE)
unspecified_6m <- sum(df_baseline$Diagnosis_6m == 'unspecified_psychotic_disorder', na.rm = TRUE)
total_individuals <- sum(!is.na(df_baseline$Diagnosis_6m))
# Calculate the percentage of patients with family history of psychosis
percentage_schizophrenia <- (schizophrenia_6m / total_individuals) * 100
percentage_schizophreniform <- (schizophreniform_6m / total_individuals) * 100
percentage_schizoaffective <- (schizoaffective_6m / total_individuals) * 100
percentage_brief_psychosis <- (brief_psychosis_6m / total_individuals) * 100
percentage_unspecified <- (unspecified_6m / total_individuals) * 100

# Calculate mean and standard deviation for 'Years_of_Education'
mean_Years_of_Education <- mean(df_baseline$Years_of_Education, na.rm = TRUE)
sd_Years_of_Education <- sd(df_baseline$Years_of_Education, na.rm = TRUE)

# Calculate mean and standard deviation for 'Duration_of_untreated_psychosis'
mean_Duration_of_untreated_psychosis <- mean(df_baseline$Duration_of_untreated_psychosis, na.rm = TRUE)
sd_Duration_of_untreated_psychosis <- sd(df_baseline$Duration_of_untreated_psychosis, na.rm = TRUE)

# Calculate mean and standard deviation for 'CPZ_equivalent'
mean_CPZ_equivalent <- mean(df_baseline$CPZ_equivalent, na.rm = TRUE)
sd_CPZ_equivalent <- sd(df_baseline$CPZ_equivalent, na.rm = TRUE)

# Calculate mean and standard deviation for 'SANS_Total'
mean_SANS_Total <- mean(df_baseline$SANS_Total, na.rm = TRUE)
sd_SANS_Total <- sd(df_baseline$SANS_Total, na.rm = TRUE)

# Calculate mean and standard deviation for 'SAPS_Total'
mean_SAPS_Total <- mean(df_baseline$SAPS_Total, na.rm = TRUE)
sd_SAPS_Total <- sd(df_baseline$SAPS_Total, na.rm = TRUE)

# Calculate mean and standard deviation for 'SANS_Global_Rating_of_Affective_Flattening'
mean_SANS_Global_Rating_of_Affective_Flattening <- mean(df_baseline$SANS_Global_Rating_of_Affective_Flattening, na.rm = TRUE)
sd_SANS_Global_Rating_of_Affective_Flattening <- sd(df_baseline$SANS_Global_Rating_of_Affective_Flattening, na.rm = TRUE)

# Calculate mean and standard deviation for 'SANS_Global_Rating_of_Alogia'
mean_SANS_Global_Rating_of_Alogia <- mean(df_baseline$SANS_Global_Rating_of_Alogia, na.rm = TRUE)
sd_SANS_Global_Rating_of_Alogia <- sd(df_baseline$SANS_Global_Rating_of_Alogia, na.rm = TRUE)

# Calculate mean and standard deviation for 'SANS_Global_Rating_of_Avolition_Apathy'
mean_SANS_Global_Rating_of_Avolition_Apathy <- mean(df_baseline$SANS_Global_Rating_of_Avolition_Apathy, na.rm = TRUE)
sd_SANS_Global_Rating_of_Avolition_Apathy <- sd(df_baseline$SANS_Global_Rating_of_Avolition_Apathy, na.rm = TRUE)

# Calculate mean and standard deviation for 'SANS_Global_Rating_of_Anhedonia_Asociality'
mean_SANS_Global_Rating_of_Anhedonia_Asociality <- mean(df_baseline$SANS_Global_Rating_of_Anhedonia_Asociality, na.rm = TRUE)
sd_SANS_Global_Rating_of_Anhedonia_Asociality <- sd(df_baseline$SANS_Global_Rating_of_Anhedonia_Asociality, na.rm = TRUE)

# Calculate mean and standard deviation for 'SANS_Global_Rating_of_Attention'
mean_SANS_Global_Rating_of_Attention <- mean(df_baseline$SANS_Global_Rating_of_Attention, na.rm = TRUE)
sd_SANS_Global_Rating_of_Attention <- sd(df_baseline$SANS_Global_Rating_of_Attention, na.rm = TRUE)

# Calculate mean and standard deviation for 'SAPS_Global_rating_of_hallucinations'
mean_SAPS_Global_rating_of_hallucinations <- mean(df_baseline$SAPS_Global_rating_of_hallucinations, na.rm = TRUE)
sd_SAPS_Global_rating_of_hallucinations <- sd(df_baseline$SAPS_Global_rating_of_hallucinations, na.rm = TRUE)

# Calculate mean and standard deviation for 'SAPS_Global_rating_of_delusions'
mean_SAPS_Global_rating_of_delusions <- mean(df_baseline$SAPS_Global_rating_of_delusions, na.rm = TRUE)
sd_SAPS_Global_rating_of_delusions <- sd(df_baseline$SAPS_Global_rating_of_delusions, na.rm = TRUE)

# Calculate mean and standard deviation for 'SAPS_Global_rating_of_bizzare_behavior'
mean_SAPS_Global_rating_of_bizzare_behavior <- mean(df_baseline$SAPS_Global_rating_of_bizzare_behavior, na.rm = TRUE)
sd_SAPS_Global_rating_of_bizzare_behavior <- sd(df_baseline$SAPS_Global_rating_of_bizzare_behavior, na.rm = TRUE)

# Calculate mean and standard deviation for 'SAPS_Global_rating_of_positive_formal_thought_disorder'
mean_SAPS_Global_rating_of_positive_formal_thought_disorder <- mean(df_baseline$SAPS_Global_rating_of_positive_formal_thought_disorder, na.rm = TRUE)
sd_SAPS_Global_rating_of_positive_formal_thought_disorder <- sd(df_baseline$SAPS_Global_rating_of_positive_formal_thought_disorder, na.rm = TRUE)

# Create a dataframe with the results
results_df <- data.frame(
  Metric = c("Mean Age of inclusion", "SD Age of inclusion", 
             "Number of Males", "Percentage of Males", "Number of Females", "Percentage of Females",
             "History of Psychosis", "Percentage with Family History of Psychosis",
             "Low Socioeconomic Status", "Percentage with Low Socioeconomic Status",
             "Schizophrenia 6m", "Percentage of Schizophrenia 6m",
             "Schizophreniform 6m", "Percentage of Schizophreniform 6m",
             "Schizoaffective 6m", "Percentage of Schizoaffective 6m",
             "Brief Psychosis 6m", "Percentage of Brief Psychosis 6m",
             "Unspecified 6m", "Percentage of Unspecified 6m",
             "Mean Years of Education", "SD Years of Education",
             "Mean Duration of Untreated Psychosis", "SD Duration of Untreated Psychosis",
             "Mean CPZ Equivalent", "SD CPZ Equivalent",
             "Mean SANS Total", "SD SANS Total",
             "Mean SAPS Total", "SD SAPS Total",
             "Mean SANS Global Rating of Affective Flattening", "SD SANS Global Rating of Affective Flattening",
             "Mean SANS Global Rating of Alogia", "SD SANS Global Rating of Alogia",
             "Mean SANS Global Rating of Avolition Apathy", "SD SANS Global Rating of Avolition Apathy",
             "Mean SANS Global Rating of Anhedonia Asociality", "SD SANS Global Rating of Anhedonia Asociality",
             "Mean SANS Global Rating of Attention", "SD SANS Global Rating of Attention",
             "Mean SAPS Global Rating of Hallucinations", "SD SAPS Global Rating of Hallucinations",
             "Mean SAPS Global Rating of Delusions", "SD SAPS Global Rating of Delusions",
             "Mean SAPS Global Rating of Bizzare Behavior", "SD SAPS Global Rating of Bizzare Behavior",
             "Mean SAPS Global Rating of Positive Formal Thought Disorder", "SD SAPS Global Rating of Positive Formal Thought Disorder"),
  Value = c(mean_Age_inclusion, sd_Age_inclusion, 
            number_of_males, percentage_males, number_of_females, percentage_females,
            history_of_psychosis, percentage_history_psychosis,
            low_socioeconomic_status, percentage_low_socioeconomic_status,
            schizophrenia_6m, percentage_schizophrenia,
            schizophreniform_6m, percentage_schizophreniform,
            schizoaffective_6m, percentage_schizoaffective,
            brief_psychosis_6m, percentage_brief_psychosis,
            unspecified_6m, percentage_unspecified,
            mean_Years_of_Education, sd_Years_of_Education,
            mean_Duration_of_untreated_psychosis, sd_Duration_of_untreated_psychosis,
            mean_CPZ_equivalent, sd_CPZ_equivalent,
            mean_SANS_Total, sd_SANS_Total,
            mean_SAPS_Total, sd_SAPS_Total,
            mean_SANS_Global_Rating_of_Affective_Flattening, sd_SANS_Global_Rating_of_Affective_Flattening,
            mean_SANS_Global_Rating_of_Alogia, sd_SANS_Global_Rating_of_Alogia,
            mean_SANS_Global_Rating_of_Avolition_Apathy, sd_SANS_Global_Rating_of_Avolition_Apathy,
            mean_SANS_Global_Rating_of_Anhedonia_Asociality, sd_SANS_Global_Rating_of_Anhedonia_Asociality,
            mean_SANS_Global_Rating_of_Attention, sd_SANS_Global_Rating_of_Attention,
            mean_SAPS_Global_rating_of_hallucinations, sd_SAPS_Global_rating_of_hallucinations,
            mean_SAPS_Global_rating_of_delusions, sd_SAPS_Global_rating_of_delusions,
            mean_SAPS_Global_rating_of_bizzare_behavior, sd_SAPS_Global_rating_of_bizzare_behavior,
            mean_SAPS_Global_rating_of_positive_formal_thought_disorder, sd_SAPS_Global_rating_of_positive_formal_thought_disorder)
)

# Save the dataframe to a CSV file
write.csv(results_df, file = "/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/datos_sociodemograficos_baseline_clinicos.csv", row.names = FALSE)

# YEAR 1

# Calculate mean and standard deviation for 'Age_inclusion'
mean_Age_inclusion_1 <- mean(df_year_1$Age_inclusion, na.rm = TRUE)
sd_Age_inclusion_1 <- sd(df_year_1$Age_inclusion, na.rm = TRUE)

# Calculate metrics for df_year_1
number_of_females_1 <- sum(df_year_1$Sex, na.rm = TRUE)
total_individuals_1 <- sum(!is.na(df_year_1$Sex))
number_of_males_1 <- total_individuals_1 - number_of_females_1
percentage_females_1 <- (number_of_females_1 / total_individuals_1) * 100
percentage_males_1 <- (number_of_males_1 / total_individuals_1) * 100

history_of_psychosis_1 <- sum(df_year_1$Family_History_of_Psychosis, na.rm = TRUE)
total_individuals_1 <- sum(!is.na(df_year_1$Family_History_of_Psychosis))
percentage_history_psychosis_1 <- (history_of_psychosis_1 / total_individuals_1) * 100

low_socioeconomic_status_1 <- sum(df_year_1$Socio_Economic_Status == 'Low', na.rm = TRUE)
total_individuals_1 <- sum(!is.na(df_year_1$Socio_Economic_Status))
percentage_low_socioeconomic_status_1 <- (low_socioeconomic_status_1 / total_individuals_1) * 100

schizophrenia_6m_1 <- sum(df_year_1$Diagnosis_6m == 'schizophrenia', na.rm = TRUE)
schizophreniform_6m_1 <- sum(df_year_1$Diagnosis_6m == 'schizophreniform', na.rm = TRUE)
schizoaffective_6m_1 <- sum(df_year_1$Diagnosis_6m == 'schizoaffective_disorder', na.rm = TRUE)
brief_psychosis_6m_1 <- sum(df_year_1$Diagnosis_6m == 'brief_psychotic_disorder', na.rm = TRUE)
unspecified_6m_1 <- sum(df_year_1$Diagnosis_6m == 'unspecified_psychotic_disorder', na.rm = TRUE)
total_individuals_1 <- sum(!is.na(df_year_1$Diagnosis_6m))
percentage_schizophrenia_1 <- (schizophrenia_6m_1 / total_individuals_1) * 100
percentage_schizophreniform_1 <- (schizophreniform_6m_1 / total_individuals_1) * 100
percentage_schizoaffective_1 <- (schizoaffective_6m_1 / total_individuals_1) * 100
percentage_brief_psychosis_1 <- (brief_psychosis_6m_1 / total_individuals_1) * 100
percentage_unspecified_1 <- (unspecified_6m_1 / total_individuals_1) * 100

mean_Years_of_Education_1 <- mean(df_year_1$Years_of_Education, na.rm = TRUE)
sd_Years_of_Education_1 <- sd(df_year_1$Years_of_Education, na.rm = TRUE)
mean_Duration_of_untreated_psychosis_1 <- mean(df_year_1$Duration_of_untreated_psychosis, na.rm = TRUE)
sd_Duration_of_untreated_psychosis_1 <- sd(df_year_1$Duration_of_untreated_psychosis, na.rm = TRUE)
mean_CPZ_equivalent_1 <- mean(df_year_1$CPZ_equivalent, na.rm = TRUE)
sd_CPZ_equivalent_1 <- sd(df_year_1$CPZ_equivalent, na.rm = TRUE)

mean_SANS_Total_1 <- mean(df_year_1$SANS_Total, na.rm = TRUE)
sd_SANS_Total_1 <- sd(df_year_1$SANS_Total, na.rm = TRUE)
mean_SAPS_Total_1 <- mean(df_year_1$SAPS_Total, na.rm = TRUE)
sd_SAPS_Total_1 <- sd(df_year_1$SAPS_Total, na.rm = TRUE)
mean_SANS_Global_Rating_of_Affective_Flattening_1 <- mean(df_year_1$SANS_Global_Rating_of_Affective_Flattening, na.rm = TRUE)
sd_SANS_Global_Rating_of_Affective_Flattening_1 <- sd(df_year_1$SANS_Global_Rating_of_Affective_Flattening, na.rm = TRUE)
mean_SANS_Global_Rating_of_Alogia_1 <- mean(df_year_1$SANS_Global_Rating_of_Alogia, na.rm = TRUE)
sd_SANS_Global_Rating_of_Alogia_1 <- sd(df_year_1$SANS_Global_Rating_of_Alogia, na.rm = TRUE)
mean_SANS_Global_Rating_of_Avolition_Apathy_1 <- mean(df_year_1$SANS_Global_Rating_of_Avolition_Apathy, na.rm = TRUE)
sd_SANS_Global_Rating_of_Avolition_Apathy_1 <- sd(df_year_1$SANS_Global_Rating_of_Avolition_Apathy, na.rm = TRUE)
mean_SANS_Global_Rating_of_Anhedonia_Asociality_1 <- mean(df_year_1$SANS_Global_Rating_of_Anhedonia_Asociality, na.rm = TRUE)
sd_SANS_Global_Rating_of_Anhedonia_Asociality_1 <- sd(df_year_1$SANS_Global_Rating_of_Anhedonia_Asociality, na.rm = TRUE)
mean_SANS_Global_Rating_of_Attention_1 <- mean(df_year_1$SANS_Global_Rating_of_Attention, na.rm = TRUE)
sd_SANS_Global_Rating_of_Attention_1 <- sd(df_year_1$SANS_Global_Rating_of_Attention, na.rm = TRUE)
mean_SAPS_Global_rating_of_hallucinations_1 <- mean(df_year_1$SAPS_Global_rating_of_hallucinations, na.rm = TRUE)
sd_SAPS_Global_rating_of_hallucinations_1 <- sd(df_year_1$SAPS_Global_rating_of_hallucinations, na.rm = TRUE)
mean_SAPS_Global_rating_of_delusions_1 <- mean(df_year_1$SAPS_Global_rating_of_delusions, na.rm = TRUE)
sd_SAPS_Global_rating_of_delusions_1 <- sd(df_year_1$SAPS_Global_rating_of_delusions, na.rm = TRUE)
mean_SAPS_Global_rating_of_bizzare_behavior_1 <- mean(df_year_1$SAPS_Global_rating_of_bizzare_behavior, na.rm = TRUE)
sd_SAPS_Global_rating_of_bizzare_behavior_1 <- sd(df_year_1$SAPS_Global_rating_of_bizzare_behavior, na.rm = TRUE)
mean_SAPS_Global_rating_of_positive_formal_thought_disorder_1 <- mean(df_year_1$SAPS_Global_rating_of_positive_formal_thought_disorder, na.rm = TRUE)
sd_SAPS_Global_rating_of_positive_formal_thought_disorder_1 <- sd(df_year_1$SAPS_Global_rating_of_positive_formal_thought_disorder, na.rm = TRUE)

# Create a dataframe with the results for year 1
results_df_year_1 <- data.frame(
  Metric = c("Mean Age of inclusion", "SD Age of inclusion", 
             "Number of Males", "Percentage of Males", "Number of Females", "Percentage of Females",
             "History of Psychosis", "Percentage with Family History of Psychosis",
             "Low Socioeconomic Status", "Percentage with Low Socioeconomic Status",
             "Schizophrenia 6m", "Percentage of Schizophrenia 6m",
             "Schizophreniform 6m", "Percentage of Schizophreniform 6m",
             "Schizoaffective 6m", "Percentage of Schizoaffective 6m",
             "Brief Psychosis 6m", "Percentage of Brief Psychosis 6m",
             "Unspecified 6m", "Percentage of Unspecified 6m",
             "Mean Years of Education", "SD Years of Education",
             "Mean Duration of Untreated Psychosis", "SD Duration of Untreated Psychosis",
             "Mean CPZ Equivalent", "SD CPZ Equivalent",
             "Mean SANS Total", "SD SANS Total",
             "Mean SAPS Total", "SD SAPS Total",
             "Mean SANS Global Rating of Affective Flattening", "SD SANS Global Rating of Affective Flattening",
             "Mean SANS Global Rating of Alogia", "SD SANS Global Rating of Alogia",
             "Mean SANS Global Rating of Avolition Apathy", "SD SANS Global Rating of Avolition Apathy",
             "Mean SANS Global Rating of Anhedonia Asociality", "SD SANS Global Rating of Anhedonia Asociality",
             "Mean SANS Global Rating of Attention", "SD SANS Global Rating of Attention",
             "Mean SAPS Global Rating of Hallucinations", "SD SAPS Global Rating of Hallucinations",
             "Mean SAPS Global Rating of Delusions", "SD SAPS Global Rating of Delusions",
             "Mean SAPS Global Rating of Bizzare Behavior", "SD SAPS Global Rating of Bizzare Behavior",
             "Mean SAPS Global Rating of Positive Formal Thought Disorder", "SD SAPS Global Rating of Positive Formal Thought Disorder"
  ),
  Value = c(mean_Age_inclusion_1, sd_Age_inclusion_1, 
            number_of_males_1, percentage_males_1, number_of_females_1, percentage_females_1,
            history_of_psychosis_1, percentage_history_psychosis_1,
            low_socioeconomic_status_1, percentage_low_socioeconomic_status_1,
            schizophrenia_6m_1, percentage_schizophrenia_1,
            schizophreniform_6m_1, percentage_schizophreniform_1,
            schizoaffective_6m_1, percentage_schizoaffective_1,
            brief_psychosis_6m_1, percentage_brief_psychosis_1,
            unspecified_6m_1, percentage_unspecified_1,
            mean_Years_of_Education_1, sd_Years_of_Education_1,
            mean_Duration_of_untreated_psychosis_1, sd_Duration_of_untreated_psychosis_1,
            mean_CPZ_equivalent_1, sd_CPZ_equivalent_1,
            mean_SANS_Total_1, sd_SANS_Total_1,
            mean_SAPS_Total_1, sd_SAPS_Total_1,
            mean_SANS_Global_Rating_of_Affective_Flattening_1, sd_SANS_Global_Rating_of_Affective_Flattening_1,
            mean_SANS_Global_Rating_of_Alogia_1, sd_SANS_Global_Rating_of_Alogia_1,
            mean_SANS_Global_Rating_of_Avolition_Apathy_1, sd_SANS_Global_Rating_of_Avolition_Apathy_1,
            mean_SANS_Global_Rating_of_Anhedonia_Asociality_1, sd_SANS_Global_Rating_of_Anhedonia_Asociality_1,
            mean_SANS_Global_Rating_of_Attention_1, sd_SANS_Global_Rating_of_Attention_1,
            mean_SAPS_Global_rating_of_hallucinations_1, sd_SAPS_Global_rating_of_hallucinations_1,
            mean_SAPS_Global_rating_of_delusions_1, sd_SAPS_Global_rating_of_delusions_1,
            mean_SAPS_Global_rating_of_bizzare_behavior_1, sd_SAPS_Global_rating_of_bizzare_behavior_1,
            mean_SAPS_Global_rating_of_positive_formal_thought_disorder_1, sd_SAPS_Global_rating_of_positive_formal_thought_disorder_1
  )
)

# Save the dataframe for year 1 to a CSV file
write.csv(results_df_year_1, file = "/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/datos_sociodemograficos_año_1_clinicos.csv", row.names = FALSE)

# YEAR 3

# Calculate mean and standard deviation for 'Age_inclusion'
mean_Age_inclusion_3 <- mean(df_year_3$Age_inclusion, na.rm = TRUE)
sd_Age_inclusion_3 <- sd(df_year_3$Age_inclusion, na.rm = TRUE)

# Calculate metrics for df_year_3
number_of_females_3 <- sum(df_year_3$Sex, na.rm = TRUE)
total_individuals_3 <- sum(!is.na(df_year_3$Sex))
number_of_males_3 <- total_individuals_3 - number_of_females_3
percentage_females_3 <- (number_of_females_3 / total_individuals_3) * 100
percentage_males_3 <- (number_of_males_3 / total_individuals_3) * 100

history_of_psychosis_3 <- sum(df_year_3$Family_History_of_Psychosis, na.rm = TRUE)
total_individuals_3 <- sum(!is.na(df_year_3$Family_History_of_Psychosis))
percentage_history_psychosis_3 <- (history_of_psychosis_3 / total_individuals_3) * 100

low_socioeconomic_status_3 <- sum(df_year_3$Socio_Economic_Status == 'Low', na.rm = TRUE)
total_individuals_3 <- sum(!is.na(df_year_3$Socio_Economic_Status))
percentage_low_socioeconomic_status_3 <- (low_socioeconomic_status_3 / total_individuals_3) * 100

schizophrenia_3y_3 <- sum(df_year_3$Diagnosis_3y == 'schizophrenia', na.rm = TRUE)
schizophreniform_3y_3 <- sum(df_year_3$Diagnosis_3y == 'schizophreniform', na.rm = TRUE)
schizoaffective_3y_3 <- sum(df_year_3$Diagnosis_3y == 'schizoaffective_disorder', na.rm = TRUE)
brief_psychosis_3y_3 <- sum(df_year_3$Diagnosis_3y == 'brief_psychotic_disorder', na.rm = TRUE)
unspecified_3y_3 <- sum(df_year_3$Diagnosis_3y == 'unspecified_psychotic_disorder', na.rm = TRUE)
total_individuals_3 <- sum(!is.na(df_year_3$Diagnosis_3y))
percentage_schizophrenia_3 <- (schizophrenia_3y_3 / total_individuals_3) * 100
percentage_schizophreniform_3 <- (schizophreniform_3y_3 / total_individuals_3) * 100
percentage_schizoaffective_3 <- (schizoaffective_3y_3 / total_individuals_3) * 100
percentage_brief_psychosis_3 <- (brief_psychosis_3y_3 / total_individuals_3) * 100
percentage_unspecified_3 <- (unspecified_3y_3 / total_individuals_3) * 100

mean_Years_of_Education_3 <- mean(df_year_3$Years_of_Education, na.rm = TRUE)
sd_Years_of_Education_3 <- sd(df_year_3$Years_of_Education, na.rm = TRUE)
mean_Duration_of_untreated_psychosis_3 <- mean(df_year_3$Duration_of_untreated_psychosis, na.rm = TRUE)
sd_Duration_of_untreated_psychosis_3 <- sd(df_year_3$Duration_of_untreated_psychosis, na.rm = TRUE)
mean_CPZ_equivalent_3 <- mean(df_year_3$CPZ_equivalent, na.rm = TRUE)
sd_CPZ_equivalent_3 <- sd(df_year_3$CPZ_equivalent, na.rm = TRUE)

mean_SANS_Total_3 <- mean(df_year_3$SANS_Total, na.rm = TRUE)
sd_SANS_Total_3 <- sd(df_year_3$SANS_Total, na.rm = TRUE)
mean_SAPS_Total_3 <- mean(df_year_3$SAPS_Total, na.rm = TRUE)
sd_SAPS_Total_3 <- sd(df_year_3$SAPS_Total, na.rm = TRUE)
mean_SANS_Global_Rating_of_Affective_Flattening_3 <- mean(df_year_3$SANS_Global_Rating_of_Affective_Flattening, na.rm = TRUE)
sd_SANS_Global_Rating_of_Affective_Flattening_3 <- sd(df_year_3$SANS_Global_Rating_of_Affective_Flattening, na.rm = TRUE)
mean_SANS_Global_Rating_of_Alogia_3 <- mean(df_year_3$SANS_Global_Rating_of_Alogia, na.rm = TRUE)
sd_SANS_Global_Rating_of_Alogia_3 <- sd(df_year_3$SANS_Global_Rating_of_Alogia, na.rm = TRUE)
mean_SANS_Global_Rating_of_Avolition_Apathy_3 <- mean(df_year_3$SANS_Global_Rating_of_Avolition_Apathy, na.rm = TRUE)
sd_SANS_Global_Rating_of_Avolition_Apathy_3 <- sd(df_year_3$SANS_Global_Rating_of_Avolition_Apathy, na.rm = TRUE)
mean_SANS_Global_Rating_of_Anhedonia_Asociality_3 <- mean(df_year_3$SANS_Global_Rating_of_Anhedonia_Asociality, na.rm = TRUE)
sd_SANS_Global_Rating_of_Anhedonia_Asociality_3 <- sd(df_year_3$SANS_Global_Rating_of_Anhedonia_Asociality, na.rm = TRUE)
mean_SANS_Global_Rating_of_Attention_3 <- mean(df_year_3$SANS_Global_Rating_of_Attention, na.rm = TRUE)
sd_SANS_Global_Rating_of_Attention_3 <- sd(df_year_3$SANS_Global_Rating_of_Attention, na.rm = TRUE)
mean_SAPS_Global_rating_of_hallucinations_3 <- mean(df_year_3$SAPS_Global_rating_of_hallucinations, na.rm = TRUE)
sd_SAPS_Global_rating_of_hallucinations_3 <- sd(df_year_3$SAPS_Global_rating_of_hallucinations, na.rm = TRUE)
mean_SAPS_Global_rating_of_delusions_3 <- mean(df_year_3$SAPS_Global_rating_of_delusions, na.rm = TRUE)
sd_SAPS_Global_rating_of_delusions_3 <- sd(df_year_3$SAPS_Global_rating_of_delusions, na.rm = TRUE)
mean_SAPS_Global_rating_of_bizzare_behavior_3 <- mean(df_year_3$SAPS_Global_rating_of_bizzare_behavior, na.rm = TRUE)
sd_SAPS_Global_rating_of_bizzare_behavior_3 <- sd(df_year_3$SAPS_Global_rating_of_bizzare_behavior, na.rm = TRUE)
mean_SAPS_Global_rating_of_positive_formal_thought_disorder_3 <- mean(df_year_3$SAPS_Global_rating_of_positive_formal_thought_disorder, na.rm = TRUE)
sd_SAPS_Global_rating_of_positive_formal_thought_disorder_3 <- sd(df_year_3$SAPS_Global_rating_of_positive_formal_thought_disorder, na.rm = TRUE)


# Create a dataframe with the results for year 3
results_df_year_3 <- data.frame(
  Metric = c("Mean Age of inclusion", "SD Age of inclusion", 
             "Number of Males", "Percentage of Males", "Number of Females", "Percentage of Females",
             "History of Psychosis", "Percentage with Family History of Psychosis",
             "Low Socioeconomic Status", "Percentage with Low Socioeconomic Status",
             "Schizophrenia 3y", "Percentage of Schizophrenia 3y",
             "Schizophreniform 3y", "Percentage of Schizophreniform 3y",
             "Schizoaffective 3y", "Percentage of Schizoaffective 3y",
             "Brief Psychosis 3y", "Percentage of Brief Psychosis 3y",
             "Unspecified 3y", "Percentage of Unspecified 3y",
             "Mean Years of Education", "SD Years of Education",
             "Mean Duration of Untreated Psychosis", "SD Duration of Untreated Psychosis",
             "Mean CPZ Equivalent", "SD CPZ Equivalent",
             "Mean SANS Total", "SD SANS Total",
             "Mean SAPS Total", "SD SAPS Total",
             "Mean SANS Global Rating of Affective Flattening", "SD SANS Global Rating of Affective Flattening",
             "Mean SANS Global Rating of Alogia", "SD SANS Global Rating of Alogia",
             "Mean SANS Global Rating of Avolition Apathy", "SD SANS Global Rating of Avolition Apathy",
             "Mean SANS Global Rating of Anhedonia Asociality", "SD SANS Global Rating of Anhedonia Asociality",
             "Mean SANS Global Rating of Attention", "SD SANS Global Rating of Attention",
             "Mean SAPS Global Rating of Hallucinations", "SD SAPS Global Rating of Hallucinations",
             "Mean SAPS Global Rating of Delusions", "SD SAPS Global Rating of Delusions",
             "Mean SAPS Global Rating of Bizzare Behavior", "SD SAPS Global Rating of Bizzare Behavior",
             "Mean SAPS Global Rating of Positive Formal Thought Disorder", "SD SAPS Global Rating of Positive Formal Thought Disorder"
  ),
  Value = c(mean_Age_inclusion_3, sd_Age_inclusion_3, 
            number_of_males_3, percentage_males_3, number_of_females_3, percentage_females_3,
            history_of_psychosis_3, percentage_history_psychosis_3,
            low_socioeconomic_status_3, percentage_low_socioeconomic_status_3,
            schizophrenia_3y_3, percentage_schizophrenia_3,
            schizophreniform_3y_3, percentage_schizophreniform_3,
            schizoaffective_3y_3, percentage_schizoaffective_3,
            brief_psychosis_3y_3, percentage_brief_psychosis_3,
            unspecified_3y_3, percentage_unspecified_3,
            mean_Years_of_Education_3, sd_Years_of_Education_3,
            mean_Duration_of_untreated_psychosis_3, sd_Duration_of_untreated_psychosis_3,
            mean_CPZ_equivalent_3, sd_CPZ_equivalent_3,
            mean_SANS_Total_3, sd_SANS_Total_3,
            mean_SAPS_Total_3, sd_SAPS_Total_3,
            mean_SANS_Global_Rating_of_Affective_Flattening_3, sd_SANS_Global_Rating_of_Affective_Flattening_3,
            mean_SANS_Global_Rating_of_Alogia_3, sd_SANS_Global_Rating_of_Alogia_3,
            mean_SANS_Global_Rating_of_Avolition_Apathy_3, sd_SANS_Global_Rating_of_Avolition_Apathy_3,
            mean_SANS_Global_Rating_of_Anhedonia_Asociality_3, sd_SANS_Global_Rating_of_Anhedonia_Asociality_3,
            mean_SANS_Global_Rating_of_Attention_3, sd_SANS_Global_Rating_of_Attention_3,
            mean_SAPS_Global_rating_of_hallucinations_3, sd_SAPS_Global_rating_of_hallucinations_3,
            mean_SAPS_Global_rating_of_delusions_3, sd_SAPS_Global_rating_of_delusions_3,
            mean_SAPS_Global_rating_of_bizzare_behavior_3, sd_SAPS_Global_rating_of_bizzare_behavior_3,
            mean_SAPS_Global_rating_of_positive_formal_thought_disorder_3, sd_SAPS_Global_rating_of_positive_formal_thought_disorder_3
  )
)

# Save the dataframe for year 3 to a CSV file
write.csv(results_df_year_3, file = "/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/datos_sociodemograficos_año_3_clinicos.csv", row.names = FALSE)

# YEAR 10

# Calculate mean and standard deviation for 'Age_inclusion'
mean_Age_inclusion_10 <- mean(df_year_10$Age_inclusion, na.rm = TRUE)
sd_Age_inclusion_10 <- sd(df_year_10$Age_inclusion, na.rm = TRUE)

# Calculate metrics for df_year_10
number_of_females_10 <- sum(df_year_10$Sex, na.rm = TRUE)
total_individuals_10 <- sum(!is.na(df_year_10$Sex))
number_of_males_10 <- total_individuals_10 - number_of_females_10
percentage_females_10 <- (number_of_females_10 / total_individuals_10) * 100
percentage_males_10 <- (number_of_males_10 / total_individuals_10) * 100

history_of_psychosis_10 <- sum(df_year_10$Family_History_of_Psychosis, na.rm = TRUE)
total_individuals_10 <- sum(!is.na(df_year_10$Family_History_of_Psychosis))
percentage_history_psychosis_10 <- (history_of_psychosis_10 / total_individuals_10) * 100

low_socioeconomic_status_10 <- sum(df_year_10$Socio_Economic_Status == 'Low', na.rm = TRUE)
total_individuals_10 <- sum(!is.na(df_year_10$Socio_Economic_Status))
percentage_low_socioeconomic_status_10 <- (low_socioeconomic_status_10 / total_individuals_10) * 100

schizophrenia_10y <- sum(df_year_10$Diagnosis_10y == 'schizophrenia', na.rm = TRUE)
schizophreniform_10y <- sum(df_year_10$Diagnosis_10y == 'schizophreniform', na.rm = TRUE)
schizoaffective_10y <- sum(df_year_10$Diagnosis_10y == 'schizoaffective_disorder', na.rm = TRUE)
brief_psychosis_10y <- sum(df_year_10$Diagnosis_10y == 'brief_psychotic_disorder', na.rm = TRUE)
unspecified_10y <- sum(df_year_10$Diagnosis_10y == 'unspecified_psychotic_disorder', na.rm = TRUE)
total_individuals_10 <- sum(!is.na(df_year_10$Diagnosis_10y))
percentage_schizophrenia_10y <- (schizophrenia_10y / total_individuals_10) * 100
percentage_schizophreniform_10y <- (schizophreniform_10y / total_individuals_10) * 100
percentage_schizoaffective_10y <- (schizoaffective_10y / total_individuals_10) * 100
percentage_brief_psychosis_10y <- (brief_psychosis_10y / total_individuals_10) * 100
percentage_unspecified_10y <- (unspecified_10y / total_individuals_10) * 100

mean_Years_of_Education_10 <- mean(df_year_10$Years_of_Education, na.rm = TRUE)
sd_Years_of_Education_10 <- sd(df_year_10$Years_of_Education, na.rm = TRUE)
mean_Duration_of_untreated_psychosis_10 <- mean(df_year_10$Duration_of_untreated_psychosis, na.rm = TRUE)
sd_Duration_of_untreated_psychosis_10 <- sd(df_year_10$Duration_of_untreated_psychosis, na.rm = TRUE)
mean_CPZ_equivalent_10 <- mean(df_year_10$CPZ_equivalent, na.rm = TRUE)
sd_CPZ_equivalent_10 <- sd(df_year_10$CPZ_equivalent, na.rm = TRUE)

mean_SANS_Total_10 <- mean(df_year_10$SANS_Total, na.rm = TRUE)
sd_SANS_Total_10 <- sd(df_year_10$SANS_Total, na.rm = TRUE)
mean_SAPS_Total_10 <- mean(df_year_10$SAPS_Total, na.rm = TRUE)
sd_SAPS_Total_10 <- sd(df_year_10$SAPS_Total, na.rm = TRUE)
mean_SANS_Global_Rating_of_Affective_Flattening_10 <- mean(df_year_10$SANS_Global_Rating_of_Affective_Flattening, na.rm = TRUE)
sd_SANS_Global_Rating_of_Affective_Flattening_10 <- sd(df_year_10$SANS_Global_Rating_of_Affective_Flattening, na.rm = TRUE)
mean_SANS_Global_Rating_of_Alogia_10 <- mean(df_year_10$SANS_Global_Rating_of_Alogia, na.rm = TRUE)
sd_SANS_Global_Rating_of_Alogia_10 <- sd(df_year_10$SANS_Global_Rating_of_Alogia, na.rm = TRUE)
mean_SANS_Global_Rating_of_Avolition_Apathy_10 <- mean(df_year_10$SANS_Global_Rating_of_Avolition_Apathy, na.rm = TRUE)
sd_SANS_Global_Rating_of_Avolition_Apathy_10 <- sd(df_year_10$SANS_Global_Rating_of_Avolition_Apathy, na.rm = TRUE)
mean_SANS_Global_Rating_of_Anhedonia_Asociality_10 <- mean(df_year_10$SANS_Global_Rating_of_Anhedonia_Asociality, na.rm = TRUE)
sd_SANS_Global_Rating_of_Anhedonia_Asociality_10 <- sd(df_year_10$SANS_Global_Rating_of_Anhedonia_Asociality, na.rm = TRUE)
mean_SANS_Global_Rating_of_Attention_10 <- mean(df_year_10$SANS_Global_Rating_of_Attention, na.rm = TRUE)
sd_SANS_Global_Rating_of_Attention_10 <- sd(df_year_10$SANS_Global_Rating_of_Attention, na.rm = TRUE)
mean_SAPS_Global_rating_of_hallucinations_10 <- mean(df_year_10$SAPS_Global_rating_of_hallucinations, na.rm = TRUE)
sd_SAPS_Global_rating_of_hallucinations_10 <- sd(df_year_10$SAPS_Global_rating_of_hallucinations, na.rm = TRUE)
mean_SAPS_Global_rating_of_delusions_10 <- mean(df_year_10$SAPS_Global_rating_of_delusions, na.rm = TRUE)
sd_SAPS_Global_rating_of_delusions_10 <- sd(df_year_10$SAPS_Global_rating_of_delusions, na.rm = TRUE)
mean_SAPS_Global_rating_of_bizzare_behavior_10 <- mean(df_year_10$SAPS_Global_rating_of_bizzare_behavior, na.rm = TRUE)
sd_SAPS_Global_rating_of_bizzare_behavior_10 <- sd(df_year_10$SAPS_Global_rating_of_bizzare_behavior, na.rm = TRUE)
mean_SAPS_Global_rating_of_positive_formal_thought_disorder_10 <- mean(df_year_10$SAPS_Global_rating_of_positive_formal_thought_disorder, na.rm = TRUE)
sd_SAPS_Global_rating_of_positive_formal_thought_disorder_10 <- sd(df_year_10$SAPS_Global_rating_of_positive_formal_thought_disorder, na.rm = TRUE)

# Create a dataframe with the results for year 10
results_df_year_10 <- data.frame(
  Metric = c("Mean Age of inclusion", "SD Age of inclusion", 
             "Number of Males", "Percentage of Males", "Number of Females", "Percentage of Females",
             "History of Psychosis", "Percentage with Family History of Psychosis",
             "Low Socioeconomic Status", "Percentage with Low Socioeconomic Status",
             "Schizophrenia 10y", "Percentage of Schizophrenia 10y",
             "Schizophreniform 10y", "Percentage of Schizophreniform 10y",
             "Schizoaffective 10y", "Percentage of Schizoaffective 10y",
             "Brief Psychosis 10y", "Percentage of Brief Psychosis 10y",
             "Unspecified 10y", "Percentage of Unspecified 10y",
             "Mean Years of Education", "SD Years of Education",
             "Mean Duration of Untreated Psychosis", "SD Duration of Untreated Psychosis",
             "Mean CPZ Equivalent", "SD CPZ Equivalent",
             "Mean SANS Total", "SD SANS Total",
             "Mean SAPS Total", "SD SAPS Total",
             "Mean SANS Global Rating of Affective Flattening", "SD SANS Global Rating of Affective Flattening",
             "Mean SANS Global Rating of Alogia", "SD SANS Global Rating of Alogia",
             "Mean SANS Global Rating of Avolition Apathy", "SD SANS Global Rating of Avolition Apathy",
             "Mean SANS Global Rating of Anhedonia Asociality", "SD SANS Global Rating of Anhedonia Asociality",
             "Mean SANS Global Rating of Attention", "SD SANS Global Rating of Attention",
             "Mean SAPS Global Rating of Hallucinations", "SD SAPS Global Rating of Hallucinations",
             "Mean SAPS Global Rating of Delusions", "SD SAPS Global Rating of Delusions",
             "Mean SAPS Global Rating of Bizzare Behavior", "SD SAPS Global Rating of Bizzare Behavior",
             "Mean SAPS Global Rating of Positive Formal Thought Disorder", "SD SAPS Global Rating of Positive Formal Thought Disorder"
  ),
  Value = c(mean_Age_inclusion_10, sd_Age_inclusion_10, 
            number_of_males_10, percentage_males_10, number_of_females_10, percentage_females_10,
            history_of_psychosis_10, percentage_history_psychosis_10,
            low_socioeconomic_status_10, percentage_low_socioeconomic_status_10,
            schizophrenia_10y, percentage_schizophrenia_10y,
            schizophreniform_10y, percentage_schizophreniform_10y,
            schizoaffective_10y, percentage_schizoaffective_10y,
            brief_psychosis_10y, percentage_brief_psychosis_10y,
            unspecified_10y, percentage_unspecified_10y,
            mean_Years_of_Education_10, sd_Years_of_Education_10,
            mean_Duration_of_untreated_psychosis_10, sd_Duration_of_untreated_psychosis_10,
            mean_CPZ_equivalent_10, sd_CPZ_equivalent_10,
            mean_SANS_Total_10, sd_SANS_Total_10,
            mean_SAPS_Total_10, sd_SAPS_Total_10,
            mean_SANS_Global_Rating_of_Affective_Flattening_10, sd_SANS_Global_Rating_of_Affective_Flattening_10,
            mean_SANS_Global_Rating_of_Alogia_10, sd_SANS_Global_Rating_of_Alogia_10,
            mean_SANS_Global_Rating_of_Avolition_Apathy_10, sd_SANS_Global_Rating_of_Avolition_Apathy_10,
            mean_SANS_Global_Rating_of_Anhedonia_Asociality_10, sd_SANS_Global_Rating_of_Anhedonia_Asociality_10,
            mean_SANS_Global_Rating_of_Attention_10, sd_SANS_Global_Rating_of_Attention_10,
            mean_SAPS_Global_rating_of_hallucinations_10, sd_SAPS_Global_rating_of_hallucinations_10,
            mean_SAPS_Global_rating_of_delusions_10, sd_SAPS_Global_rating_of_delusions_10,
            mean_SAPS_Global_rating_of_bizzare_behavior_10, sd_SAPS_Global_rating_of_bizzare_behavior_10,
            mean_SAPS_Global_rating_of_positive_formal_thought_disorder_10, sd_SAPS_Global_rating_of_positive_formal_thought_disorder_10
            
  )
)

# Save the dataframe for year 10 to a CSV file
write.csv(results_df_year_10, file = "/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/datos_sociodemograficos_año_10_clinicos.csv", row.names = FALSE)

### GAF

# Datasets
A <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Covariates_complete copia 2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx
B <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_GAF_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Keep only rows in A where the first column matches the first column in B
A_filtered <- A[A[,1] %in% B[,1], ]

df_baseline <- A_filtered[A_filtered$Assessment == 1, ]
df_year_1 <- A_filtered[A_filtered$Assessment == 2, ]
df_year_1 <- df_year_1[!is.nan(df_year_1$Global_Assessment_Functioning), ]
df_year_3 <- A_filtered[A_filtered$Assessment == 3, ]
df_year_3 <- df_year_3[!is.nan(df_year_3$Global_Assessment_Functioning), ]
df_year_10 <- A_filtered[A_filtered$Assessment == 10, ]
df_year_10 <- df_year_10[!is.nan(df_year_10$Global_Assessment_Functioning), ]

# BASELINE

# Calculate mean and standard deviation for 'Age_inclusion'
mean_Age_inclusion <- mean(df_baseline$Age_inclusion, na.rm = TRUE)
sd_Age_inclusion <- sd(df_baseline$Age_inclusion, na.rm = TRUE)

# Calculate number of males and females
number_of_females <- sum(df_baseline$Sex, na.rm = TRUE)
total_individuals <- sum(!is.na(df_baseline$Sex))
number_of_males <- total_individuals - number_of_females
# Calculate the percentage of females and males
percentage_females <- (number_of_females / total_individuals) * 100
percentage_males <- (number_of_males / total_individuals) * 100

# Calculate family history of psychosis
history_of_psychosis <- sum(df_baseline$Family_History_of_Psychosis, na.rm = TRUE)
total_individuals <- sum(!is.na(df_baseline$Family_History_of_Psychosis))
# Calculate the percentage of patients with family history of psychosis
percentage_history_psychosis <- (history_of_psychosis / total_individuals) * 100

# Calculate low socioeconomic status
low_socioeconomic_status <- sum(df_baseline$Socio_Economic_Status == 'Low', na.rm = TRUE)
total_individuals <- sum(!is.na(df_baseline$Socio_Economic_Status))
# Calculate the percentage of patients with family history of psychosis
percentage_low_socioeconomic_status <- (low_socioeconomic_status / total_individuals) * 100

# Calculate diagnosis_6m
schizophrenia_6m <- sum(df_baseline$Diagnosis_6m == 'schizophrenia', na.rm = TRUE)
schizophreniform_6m <- sum(df_baseline$Diagnosis_6m == 'schizophreniform', na.rm = TRUE)
schizoaffective_6m <- sum(df_baseline$Diagnosis_6m == 'schizoaffective_disorder', na.rm = TRUE)
brief_psychosis_6m <- sum(df_baseline$Diagnosis_6m == 'brief_psychotic_disorder', na.rm = TRUE)
unspecified_6m <- sum(df_baseline$Diagnosis_6m == 'unspecified_psychotic_disorder', na.rm = TRUE)
total_individuals <- sum(!is.na(df_baseline$Diagnosis_6m))
# Calculate the percentage of patients with family history of psychosis
percentage_schizophrenia <- (schizophrenia_6m / total_individuals) * 100
percentage_schizophreniform <- (schizophreniform_6m / total_individuals) * 100
percentage_schizoaffective <- (schizoaffective_6m / total_individuals) * 100
percentage_brief_psychosis <- (brief_psychosis_6m / total_individuals) * 100
percentage_unspecified <- (unspecified_6m / total_individuals) * 100

# Calculate mean and standard deviation for 'Years_of_Education'
mean_Years_of_Education <- mean(df_baseline$Years_of_Education, na.rm = TRUE)
sd_Years_of_Education <- sd(df_baseline$Years_of_Education, na.rm = TRUE)

# Calculate mean and standard deviation for 'Duration_of_untreated_psychosis'
mean_Duration_of_untreated_psychosis <- mean(df_baseline$Duration_of_untreated_psychosis, na.rm = TRUE)
sd_Duration_of_untreated_psychosis <- sd(df_baseline$Duration_of_untreated_psychosis, na.rm = TRUE)

# Calculate mean and standard deviation for 'CPZ_equivalent'
mean_CPZ_equivalent <- mean(df_baseline$CPZ_equivalent, na.rm = TRUE)
sd_CPZ_equivalent <- sd(df_baseline$CPZ_equivalent, na.rm = TRUE)

# Calculate mean and standard deviation for 'SANS_Total'
mean_GAF <- mean(df_baseline$Global_Assessment_Functioning, na.rm = TRUE)
sd_GAF <- sd(df_baseline$Global_Assessment_Functioning, na.rm = TRUE)

# Create a dataframe with the results
results_df <- data.frame(
  Metric = c("Mean Age of inclusion", "SD Age of inclusion", 
             "Number of Males", "Percentage of Males", "Number of Females", "Percentage of Females",
             "History of Psychosis", "Percentage with Family History of Psychosis",
             "Low Socioeconomic Status", "Percentage with Low Socioeconomic Status",
             "Schizophrenia 6m", "Percentage of Schizophrenia 6m",
             "Schizophreniform 6m", "Percentage of Schizophreniform 6m",
             "Schizoaffective 6m", "Percentage of Schizoaffective 6m",
             "Brief Psychosis 6m", "Percentage of Brief Psychosis 6m",
             "Unspecified 6m", "Percentage of Unspecified 6m",
             "Mean Years of Education", "SD Years of Education",
             "Mean Duration of Untreated Psychosis", "SD Duration of Untreated Psychosis",
             "Mean CPZ Equivalent", "SD CPZ Equivalent",
             "Mean GAF", "SD GAF"),
  Value = c(mean_Age_inclusion, sd_Age_inclusion, 
            number_of_males, percentage_males, number_of_females, percentage_females,
            history_of_psychosis, percentage_history_psychosis,
            low_socioeconomic_status, percentage_low_socioeconomic_status,
            schizophrenia_6m, percentage_schizophrenia,
            schizophreniform_6m, percentage_schizophreniform,
            schizoaffective_6m, percentage_schizoaffective,
            brief_psychosis_6m, percentage_brief_psychosis,
            unspecified_6m, percentage_unspecified,
            mean_Years_of_Education, sd_Years_of_Education,
            mean_Duration_of_untreated_psychosis, sd_Duration_of_untreated_psychosis,
            mean_CPZ_equivalent, sd_CPZ_equivalent,
            mean_GAF, sd_GAF
  )
)

# Save the dataframe to a CSV file
write.csv(results_df, file = "/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/datos_sociodemograficos_baseline_funcional.csv", row.names = FALSE)

# YEAR 1

# Calculate mean and standard deviation for 'Age_inclusion'
mean_Age_inclusion_1 <- mean(df_year_1$Age_inclusion, na.rm = TRUE)
sd_Age_inclusion_1 <- sd(df_year_1$Age_inclusion, na.rm = TRUE)

# Calculate metrics for df_year_1
number_of_females_1 <- sum(df_year_1$Sex, na.rm = TRUE)
total_individuals_1 <- sum(!is.na(df_year_1$Sex))
number_of_males_1 <- total_individuals_1 - number_of_females_1
percentage_females_1 <- (number_of_females_1 / total_individuals_1) * 100
percentage_males_1 <- (number_of_males_1 / total_individuals_1) * 100

history_of_psychosis_1 <- sum(df_year_1$Family_History_of_Psychosis, na.rm = TRUE)
total_individuals_1 <- sum(!is.na(df_year_1$Family_History_of_Psychosis))
percentage_history_psychosis_1 <- (history_of_psychosis_1 / total_individuals_1) * 100

low_socioeconomic_status_1 <- sum(df_year_1$Socio_Economic_Status == 'Low', na.rm = TRUE)
total_individuals_1 <- sum(!is.na(df_year_1$Socio_Economic_Status))
percentage_low_socioeconomic_status_1 <- (low_socioeconomic_status_1 / total_individuals_1) * 100

schizophrenia_6m_1 <- sum(df_year_1$Diagnosis_6m == 'schizophrenia', na.rm = TRUE)
schizophreniform_6m_1 <- sum(df_year_1$Diagnosis_6m == 'schizophreniform', na.rm = TRUE)
schizoaffective_6m_1 <- sum(df_year_1$Diagnosis_6m == 'schizoaffective_disorder', na.rm = TRUE)
brief_psychosis_6m_1 <- sum(df_year_1$Diagnosis_6m == 'brief_psychotic_disorder', na.rm = TRUE)
unspecified_6m_1 <- sum(df_year_1$Diagnosis_6m == 'unspecified_psychotic_disorder', na.rm = TRUE)
total_individuals_1 <- sum(!is.na(df_year_1$Diagnosis_6m))
percentage_schizophrenia_1 <- (schizophrenia_6m_1 / total_individuals_1) * 100
percentage_schizophreniform_1 <- (schizophreniform_6m_1 / total_individuals_1) * 100
percentage_schizoaffective_1 <- (schizoaffective_6m_1 / total_individuals_1) * 100
percentage_brief_psychosis_1 <- (brief_psychosis_6m_1 / total_individuals_1) * 100
percentage_unspecified_1 <- (unspecified_6m_1 / total_individuals_1) * 100

mean_Years_of_Education_1 <- mean(df_year_1$Years_of_Education, na.rm = TRUE)
sd_Years_of_Education_1 <- sd(df_year_1$Years_of_Education, na.rm = TRUE)
mean_Duration_of_untreated_psychosis_1 <- mean(df_year_1$Duration_of_untreated_psychosis, na.rm = TRUE)
sd_Duration_of_untreated_psychosis_1 <- sd(df_year_1$Duration_of_untreated_psychosis, na.rm = TRUE)
mean_CPZ_equivalent_1 <- mean(df_year_1$CPZ_equivalent, na.rm = TRUE)
sd_CPZ_equivalent_1 <- sd(df_year_1$CPZ_equivalent, na.rm = TRUE)

mean_GAF_1 <- mean(df_year_1$Global_Assessment_Functioning, na.rm = TRUE)
sd_GAF_1 <- sd(df_year_1$Global_Assessment_Functioning, na.rm = TRUE)


# Create a dataframe with the results for year 1
results_df_year_1 <- data.frame(
  Metric = c("Mean Age of inclusion", "SD Age of inclusion", 
             "Number of Males", "Percentage of Males", "Number of Females", "Percentage of Females",
             "History of Psychosis", "Percentage with Family History of Psychosis",
             "Low Socioeconomic Status", "Percentage with Low Socioeconomic Status",
             "Schizophrenia 6m", "Percentage of Schizophrenia 6m",
             "Schizophreniform 6m", "Percentage of Schizophreniform 6m",
             "Schizoaffective 6m", "Percentage of Schizoaffective 6m",
             "Brief Psychosis 6m", "Percentage of Brief Psychosis 6m",
             "Unspecified 6m", "Percentage of Unspecified 6m",
             "Mean Years of Education", "SD Years of Education",
             "Mean Duration of Untreated Psychosis", "SD Duration of Untreated Psychosis",
             "Mean CPZ Equivalent", "SD CPZ Equivalent",
             "Mean GAF", "SD GAF"
  ),
  Value = c(mean_Age_inclusion_1, sd_Age_inclusion_1, 
            number_of_males_1, percentage_males_1, number_of_females_1, percentage_females_1,
            history_of_psychosis_1, percentage_history_psychosis_1,
            low_socioeconomic_status_1, percentage_low_socioeconomic_status_1,
            schizophrenia_6m_1, percentage_schizophrenia_1,
            schizophreniform_6m_1, percentage_schizophreniform_1,
            schizoaffective_6m_1, percentage_schizoaffective_1,
            brief_psychosis_6m_1, percentage_brief_psychosis_1,
            unspecified_6m_1, percentage_unspecified_1,
            mean_Years_of_Education_1, sd_Years_of_Education_1,
            mean_Duration_of_untreated_psychosis_1, sd_Duration_of_untreated_psychosis_1,
            mean_CPZ_equivalent_1, sd_CPZ_equivalent_1,
            mean_GAF_1, sd_GAF_1
  )
)

# Save the dataframe for year 1 to a CSV file
write.csv(results_df_year_1, file = "/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/datos_sociodemograficos_año_1_funcional.csv", row.names = FALSE)

# YEAR 3

# Calculate mean and standard deviation for 'Age_inclusion'
mean_Age_inclusion_3 <- mean(df_year_3$Age_inclusion, na.rm = TRUE)
sd_Age_inclusion_3 <- sd(df_year_3$Age_inclusion, na.rm = TRUE)

# Calculate metrics for df_year_3
number_of_females_3 <- sum(df_year_3$Sex, na.rm = TRUE)
total_individuals_3 <- sum(!is.na(df_year_3$Sex))
number_of_males_3 <- total_individuals_3 - number_of_females_3
percentage_females_3 <- (number_of_females_3 / total_individuals_3) * 100
percentage_males_3 <- (number_of_males_3 / total_individuals_3) * 100

history_of_psychosis_3 <- sum(df_year_3$Family_History_of_Psychosis, na.rm = TRUE)
total_individuals_3 <- sum(!is.na(df_year_3$Family_History_of_Psychosis))
percentage_history_psychosis_3 <- (history_of_psychosis_3 / total_individuals_3) * 100

low_socioeconomic_status_3 <- sum(df_year_3$Socio_Economic_Status == 'Low', na.rm = TRUE)
total_individuals_3 <- sum(!is.na(df_year_3$Socio_Economic_Status))
percentage_low_socioeconomic_status_3 <- (low_socioeconomic_status_3 / total_individuals_3) * 100

schizophrenia_3y_3 <- sum(df_year_3$Diagnosis_3y == 'schizophrenia', na.rm = TRUE)
schizophreniform_3y_3 <- sum(df_year_3$Diagnosis_3y == 'schizophreniform', na.rm = TRUE)
schizoaffective_3y_3 <- sum(df_year_3$Diagnosis_3y == 'schizoaffective_disorder', na.rm = TRUE)
brief_psychosis_3y_3 <- sum(df_year_3$Diagnosis_3y == 'brief_psychotic_disorder', na.rm = TRUE)
unspecified_3y_3 <- sum(df_year_3$Diagnosis_3y == 'unspecified_psychotic_disorder', na.rm = TRUE)
total_individuals_3 <- sum(!is.na(df_year_3$Diagnosis_3y))
percentage_schizophrenia_3 <- (schizophrenia_3y_3 / total_individuals_3) * 100
percentage_schizophreniform_3 <- (schizophreniform_3y_3 / total_individuals_3) * 100
percentage_schizoaffective_3 <- (schizoaffective_3y_3 / total_individuals_3) * 100
percentage_brief_psychosis_3 <- (brief_psychosis_3y_3 / total_individuals_3) * 100
percentage_unspecified_3 <- (unspecified_3y_3 / total_individuals_3) * 100

mean_Years_of_Education_3 <- mean(df_year_3$Years_of_Education, na.rm = TRUE)
sd_Years_of_Education_3 <- sd(df_year_3$Years_of_Education, na.rm = TRUE)
mean_Duration_of_untreated_psychosis_3 <- mean(df_year_3$Duration_of_untreated_psychosis, na.rm = TRUE)
sd_Duration_of_untreated_psychosis_3 <- sd(df_year_3$Duration_of_untreated_psychosis, na.rm = TRUE)
mean_CPZ_equivalent_3 <- mean(df_year_3$CPZ_equivalent, na.rm = TRUE)
sd_CPZ_equivalent_3 <- sd(df_year_3$CPZ_equivalent, na.rm = TRUE)

mean_GAF_3 <- mean(df_year_3$Global_Assessment_Functioning, na.rm = TRUE)
sd_GAF_3 <- sd(df_year_3$Global_Assessment_Functioning, na.rm = TRUE)

# Create a dataframe with the results for year 3
results_df_year_3 <- data.frame(
  Metric = c("Mean Age of inclusion", "SD Age of inclusion", 
             "Number of Males", "Percentage of Males", "Number of Females", "Percentage of Females",
             "History of Psychosis", "Percentage with Family History of Psychosis",
             "Low Socioeconomic Status", "Percentage with Low Socioeconomic Status",
             "Schizophrenia 3y", "Percentage of Schizophrenia 3y",
             "Schizophreniform 3y", "Percentage of Schizophreniform 3y",
             "Schizoaffective 3y", "Percentage of Schizoaffective 3y",
             "Brief Psychosis 3y", "Percentage of Brief Psychosis 3y",
             "Unspecified 3y", "Percentage of Unspecified 3y",
             "Mean Years of Education", "SD Years of Education",
             "Mean Duration of Untreated Psychosis", "SD Duration of Untreated Psychosis",
             "Mean CPZ Equivalent", "SD CPZ Equivalent",
             "Mean GAF", "SD GAF"
  ),
  Value = c(mean_Age_inclusion_3, sd_Age_inclusion_3, 
            number_of_males_3, percentage_males_3, number_of_females_3, percentage_females_3,
            history_of_psychosis_3, percentage_history_psychosis_3,
            low_socioeconomic_status_3, percentage_low_socioeconomic_status_3,
            schizophrenia_3y_3, percentage_schizophrenia_3,
            schizophreniform_3y_3, percentage_schizophreniform_3,
            schizoaffective_3y_3, percentage_schizoaffective_3,
            brief_psychosis_3y_3, percentage_brief_psychosis_3,
            unspecified_3y_3, percentage_unspecified_3,
            mean_Years_of_Education_3, sd_Years_of_Education_3,
            mean_Duration_of_untreated_psychosis_3, sd_Duration_of_untreated_psychosis_3,
            mean_CPZ_equivalent_3, sd_CPZ_equivalent_3,
            mean_GAF_3, sd_GAF_3
  )
)

# Save the dataframe for year 3 to a CSV file
write.csv(results_df_year_3, file = "/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/datos_sociodemograficos_año_3_funcional.csv", row.names = FALSE)

# YEAR 10

# Calculate mean and standard deviation for 'Age_inclusion'
mean_Age_inclusion_10 <- mean(df_year_10$Age_inclusion, na.rm = TRUE)
sd_Age_inclusion_10 <- sd(df_year_10$Age_inclusion, na.rm = TRUE)

# Calculate metrics for df_year_10
number_of_females_10 <- sum(df_year_10$Sex, na.rm = TRUE)
total_individuals_10 <- sum(!is.na(df_year_10$Sex))
number_of_males_10 <- total_individuals_10 - number_of_females_10
percentage_females_10 <- (number_of_females_10 / total_individuals_10) * 100
percentage_males_10 <- (number_of_males_10 / total_individuals_10) * 100

history_of_psychosis_10 <- sum(df_year_10$Family_History_of_Psychosis, na.rm = TRUE)
total_individuals_10 <- sum(!is.na(df_year_10$Family_History_of_Psychosis))
percentage_history_psychosis_10 <- (history_of_psychosis_10 / total_individuals_10) * 100

low_socioeconomic_status_10 <- sum(df_year_10$Socio_Economic_Status == 'Low', na.rm = TRUE)
total_individuals_10 <- sum(!is.na(df_year_10$Socio_Economic_Status))
percentage_low_socioeconomic_status_10 <- (low_socioeconomic_status_10 / total_individuals_10) * 100

schizophrenia_10y <- sum(df_year_10$Diagnosis_10y == 'schizophrenia', na.rm = TRUE)
schizophreniform_10y <- sum(df_year_10$Diagnosis_10y == 'schizophreniform', na.rm = TRUE)
schizoaffective_10y <- sum(df_year_10$Diagnosis_10y == 'schizoaffective_disorder', na.rm = TRUE)
brief_psychosis_10y <- sum(df_year_10$Diagnosis_10y == 'brief_psychotic_disorder', na.rm = TRUE)
unspecified_10y <- sum(df_year_10$Diagnosis_10y == 'unspecified_psychotic_disorder', na.rm = TRUE)
total_individuals_10 <- sum(!is.na(df_year_10$Diagnosis_10y))
percentage_schizophrenia_10y <- (schizophrenia_10y / total_individuals_10) * 100
percentage_schizophreniform_10y <- (schizophreniform_10y / total_individuals_10) * 100
percentage_schizoaffective_10y <- (schizoaffective_10y / total_individuals_10) * 100
percentage_brief_psychosis_10y <- (brief_psychosis_10y / total_individuals_10) * 100
percentage_unspecified_10y <- (unspecified_10y / total_individuals_10) * 100

mean_Years_of_Education_10 <- mean(df_year_10$Years_of_Education, na.rm = TRUE)
sd_Years_of_Education_10 <- sd(df_year_10$Years_of_Education, na.rm = TRUE)
mean_Duration_of_untreated_psychosis_10 <- mean(df_year_10$Duration_of_untreated_psychosis, na.rm = TRUE)
sd_Duration_of_untreated_psychosis_10 <- sd(df_year_10$Duration_of_untreated_psychosis, na.rm = TRUE)
mean_CPZ_equivalent_10 <- mean(df_year_10$CPZ_equivalent, na.rm = TRUE)
sd_CPZ_equivalent_10 <- sd(df_year_10$CPZ_equivalent, na.rm = TRUE)

mean_GAF_10 <- mean(df_year_10$Global_Assessment_Functioning, na.rm = TRUE)
sd_GAF_10 <- sd(df_year_10$Global_Assessment_Functioning, na.rm = TRUE)

# Create a dataframe with the results for year 10
results_df_year_10 <- data.frame(
  Metric = c("Mean Age of inclusion", "SD Age of inclusion", 
             "Number of Males", "Percentage of Males", "Number of Females", "Percentage of Females",
             "History of Psychosis", "Percentage with Family History of Psychosis",
             "Low Socioeconomic Status", "Percentage with Low Socioeconomic Status",
             "Schizophrenia 10y", "Percentage of Schizophrenia 10y",
             "Schizophreniform 10y", "Percentage of Schizophreniform 10y",
             "Schizoaffective 10y", "Percentage of Schizoaffective 10y",
             "Brief Psychosis 10y", "Percentage of Brief Psychosis 10y",
             "Unspecified 10y", "Percentage of Unspecified 10y",
             "Mean Years of Education", "SD Years of Education",
             "Mean Duration of Untreated Psychosis", "SD Duration of Untreated Psychosis",
             "Mean CPZ Equivalent", "SD CPZ Equivalent",
             "Mean GAF", "SD GAF"
  ),
  Value = c(mean_Age_inclusion_10, sd_Age_inclusion_10, 
            number_of_males_10, percentage_males_10, number_of_females_10, percentage_females_10,
            history_of_psychosis_10, percentage_history_psychosis_10,
            low_socioeconomic_status_10, percentage_low_socioeconomic_status_10,
            schizophrenia_10y, percentage_schizophrenia_10y,
            schizophreniform_10y, percentage_schizophreniform_10y,
            schizoaffective_10y, percentage_schizoaffective_10y,
            brief_psychosis_10y, percentage_brief_psychosis_10y,
            unspecified_10y, percentage_unspecified_10y,
            mean_Years_of_Education_10, sd_Years_of_Education_10,
            mean_Duration_of_untreated_psychosis_10, sd_Duration_of_untreated_psychosis_10,
            mean_CPZ_equivalent_10, sd_CPZ_equivalent_10,
            mean_GAF_10, sd_GAF_10
  )
)

# Save the dataframe for year 10 to a CSV file
write.csv(results_df_year_10, file = "/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/datos_sociodemograficos_año_10_funcional.csv", row.names = FALSE)

# -------------------------------------------------------------------------------------------------

### EXTRAER MEDIANA DE NUMERO DE EVAUACIONES LONGITUDINALES

### SANS/SAPS

# Datasets
A <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Covariates_complete copia 2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx
B <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_symptoms_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Keep only rows in A where the first column matches the first column in B
A_filtered <- A[A[,1] %in% B[,1], ]

# Removing rows with NaN values
df <- A_filtered[!is.nan(A_filtered$SAPS_Total), ]

# Load dplyr package
library(dplyr)

# Count the number of assessments per subject
assessment_counts <- df %>%
  group_by(Subject) %>%
  summarise(assessments = n())

# Calculate median, min, and max of assessments
median_assessments <- median(assessment_counts$assessments)
min_assessments <- min(assessment_counts$assessments)
max_assessments <- max(assessment_counts$assessments)

# Print the results
cat("Median number of assessments per subject:", median_assessments, "\n")
cat("Minimum number of assessments per subject:", min_assessments, "\n")
cat("Maximum number of assessments per subject:", max_assessments, "\n")

### GAF

A <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Covariates_complete copia 2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx
B <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_GAF_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

# Keep only rows in A where the first column matches the first column in B
A_filtered <- A[A[,1] %in% B[,1], ]

# Removing rows with NaN values
df <- A_filtered[!is.nan(A_filtered$Global_Assessment_Functioning), ]

# Load dplyr package
library(dplyr)

# Count the number of assessments per subject
assessment_counts <- df %>%
  group_by(Subject) %>%
  summarise(assessments = n())

# Calculate median, min, and max of assessments
median_assessments <- median(assessment_counts$assessments)
min_assessments <- min(assessment_counts$assessments)
max_assessments <- max(assessment_counts$assessments)

# Print the results
cat("Median number of assessments per subject:", median_assessments, "\n")
cat("Minimum number of assessments per subject:", min_assessments, "\n")
cat("Maximum number of assessments per subject:", max_assessments, "\n")

### EXTRAER MEDIANA DE TIEMPO (EN AÑOS) EN EL QUE SE REALIZARON LAS EVAUACIONES LONGITUDINALES

### SANS/SAPS

# Datasets
A_filtered <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_symptoms_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

df_baseline <- A_filtered[A_filtered$Clinical.Assessment == 0, ]
df_year_1 <- A_filtered[A_filtered$Clinical.Assessment == 1, ]
df_year_1 <- df_year_1[!is.nan(df_year_1$SANS.total), ]
df_year_3 <- A_filtered[A_filtered$Clinical.Assessment == 3, ]
df_year_3 <- df_year_3[!is.nan(df_year_3$SANS.total), ]
df_year_10 <- A_filtered[A_filtered$Clinical.Assessment == 10, ]
df_year_10 <- df_year_10[!is.nan(df_year_10$SANS.total), ]

# For Baseline
median_assessment_baseline <- median(df_baseline$Time_clinical)
min_assessment_baseline <- min(df_baseline$Time_clinical)
max_assessment_baseline <- max(df_baseline$Time_clinical)

# Print the results
cat("Median years assessment baseline:", median_assessment_baseline, "\n")
cat("Minimum years assessment baseline:", min_assessment_baseline, "\n")
cat("Maximum years assessment baseline:", max_assessment_baseline, "\n")

# For Year 1
median_assessment_year_1 <- median(df_year_1$Time_clinical)
min_assessment_year_1 <- min(df_year_1$Time_clinical)
max_assessment_year_1 <- max(df_year_1$Time_clinical)

# Print the results for Year 1
cat("Median years assessment year 1:", median_assessment_year_1, "\n")
cat("Minimum years assessment year 1:", min_assessment_year_1, "\n")
cat("Maximum years assessment year 1:", max_assessment_year_1, "\n\n")

# For Year 3
median_assessment_year_3 <- median(df_year_3$Time_clinical)
min_assessment_year_3 <- min(df_year_3$Time_clinical)
max_assessment_year_3 <- max(df_year_3$Time_clinical)

# Print the results for Year 3
cat("Median years assessment year 3:", median_assessment_year_3, "\n")
cat("Minimum years assessment year 3:", min_assessment_year_3, "\n")
cat("Maximum years assessment year 3:", max_assessment_year_3, "\n\n")

# For Year 10
median_assessment_year_10 <- median(df_year_10$Time_clinical)
min_assessment_year_10 <- min(df_year_10$Time_clinical)
max_assessment_year_10 <- max(df_year_10$Time_clinical)

# Print the results for Year 10
cat("Median years assessment year 10:", median_assessment_year_10, "\n")
cat("Minimum years assessment year 10:", min_assessment_year_10, "\n")
cat("Maximum years assessment year 10:", max_assessment_year_10, "\n")

### GAF

# Datasets
A_filtered <- read.csv('/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/****Centiles_FEP_MRI_baseline_GAF_raw_scores_2.csv') # añadir (,sep=';') al final si no separa bien los datos en columnas, como cuando se importa de .xlsx

df_baseline <- A_filtered[A_filtered$Clinical.Assessment == 0, ]
df_year_1 <- A_filtered[A_filtered$Clinical.Assessment == 1, ]
df_year_1 <- df_year_1[!is.nan(df_year_1$GAF), ]
df_year_3 <- A_filtered[A_filtered$Clinical.Assessment == 3, ]
df_year_3 <- df_year_3[!is.nan(df_year_3$GAF), ]
df_year_10 <- A_filtered[A_filtered$Clinical.Assessment == 10, ]
df_year_10 <- df_year_10[!is.nan(df_year_10$GAF), ]

# For Baseline
median_assessment_baseline <- median(df_baseline$Time_clinical)
min_assessment_baseline <- min(df_baseline$Time_clinical)
max_assessment_baseline <- max(df_baseline$Time_clinical)

# Print the results
cat("Median years assessment baseline:", median_assessment_baseline, "\n")
cat("Minimum years assessment baseline:", min_assessment_baseline, "\n")
cat("Maximum years assessment baseline:", max_assessment_baseline, "\n")

# For Year 1
median_assessment_year_1 <- median(df_year_1$Time_clinical)
min_assessment_year_1 <- min(df_year_1$Time_clinical)
max_assessment_year_1 <- max(df_year_1$Time_clinical)

# Print the results for Year 1
cat("Median years assessment year 1:", median_assessment_year_1, "\n")
cat("Minimum years assessment year 1:", min_assessment_year_1, "\n")
cat("Maximum years assessment year 1:", max_assessment_year_1, "\n\n")

# For Year 3
median_assessment_year_3 <- median(df_year_3$Time_clinical)
min_assessment_year_3 <- min(df_year_3$Time_clinical)
max_assessment_year_3 <- max(df_year_3$Time_clinical)

# Print the results for Year 3
cat("Median years assessment year 3:", median_assessment_year_3, "\n")
cat("Minimum years assessment year 3:", min_assessment_year_3, "\n")
cat("Maximum years assessment year 3:", max_assessment_year_3, "\n\n")

# For Year 10
median_assessment_year_10 <- median(df_year_10$Time_clinical)
min_assessment_year_10 <- min(df_year_10$Time_clinical)
max_assessment_year_10 <- max(df_year_10$Time_clinical)

# Print the results for Year 10
cat("Median years assessment year 10:", median_assessment_year_10, "\n")
cat("Minimum years assessment year 10:", min_assessment_year_10, "\n")
cat("Maximum years assessment year 10:", max_assessment_year_10, "\n")

