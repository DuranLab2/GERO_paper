set.seed(1111)
options(max.print=3000)
library(readxl)
library(lavaan)
library(semPlot)
library(writexl)
library(dplyr)
library(glmnet)
library(reshape2)
library(ggplot2)
library("MatchIt")



data <- read.csv("merged_filtered_data_CN_AD_FTD_bio_Cognitive_domains_CN_AD.csv")
data$diagnosis_binary <- ifelse(data$diagnosis == "AD", 1, 
                                ifelse(data$diagnosis == "CN", 0, NA))


match <- matchit(diagnosis_binary  ~ age + education + sex, data = data, method = "nearest", caliper = 0.30)

summary(match)

# Evaluar el balance
plot(match)



library(cobalt)

bal.tab(match, un = TRUE)

matched_data <- match.data(match)


# Prueba de Shapiro-Wilk para 'age'
shapiro_age <- shapiro.test(matched_data$age)
print(paste("Shapiro-Wilk p-value for age:", shapiro_age$p.value))

# Prueba de Shapiro-Wilk para 'education'
shapiro_education <- shapiro.test(matched_data$education)
print(paste("Shapiro-Wilk p-value for education:", shapiro_education$p.value))


# Prueba Chi-cuadrado para 'sex'
p_chisq <- chisq.test(table(matched_data$sex, matched_data$diagnosis_binary))$p.value
print(paste("Chi-squared test (sex):", p_chisq))

# Prueba t para 'age'
p_t_age <- t.test(age ~ diagnosis_binary, data = matched_data)$p.value
print(paste("t-test (age):", p_t_age))

# Prueba t para 'education'
p_t_education <- t.test(education ~ diagnosis_binary, data = matched_data)$p.value
print(paste("t-test (education):", p_t_education))

# Prueba no paramétrica de Wilcoxon para 'age'
p_wilcox_age <- wilcox.test(age ~ diagnosis_binary, data = matched_data)$p.value
print(paste("Wilcoxon test (age):", p_wilcox_age))

# Prueba no paramétrica de Wilcoxon para 'education'
p_wilcox_education <- wilcox.test(education ~ diagnosis_binary, data = matched_data)$p.value
print(paste("Wilcoxon test (education):", p_wilcox_education))


library(dplyr)

# Contar filas por nivel
factor_counts_df <- matched_data %>%
  group_by(diagnosis) %>%
  summarise(Count = n())

# Mostrar el resultado
print(factor_counts_df)


write.csv(matched_data, "merged_filtered_data_CN_AD_FTD_bio_Cognitive_domains_CN_AD_matched.csv", row.names = FALSE)


round(cor(data$NfL, data$GLOBAL.COGNITION),2)
round(cor(data$NfL, data$age),2)
