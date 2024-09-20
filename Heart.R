install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
install.packages("fmsb")

library(ggplot2)
library(corrplot)
library(dplyr)
library(fmsb)

heart_data <- read.csv("E:/heart.csv", header = TRUE, sep = ",")
head(heart_data)
str(heart_data)

target_var <- 'target'
pearson_cor <- cor(heart_data, method = "pearson", use = "complete.obs")
pearson_target_cor <- pearson_cor[, target_var]
pearson_target_cor_df <- data.frame(Variable = names(pearson_target_cor), Correlation = pearson_target_cor)

print(pearson_target_cor_df)

positive_correlations <- pearson_target_cor_df %>%
  filter(Correlation > 0.2)

negative_correlations <- pearson_target_cor_df %>%
  filter(Correlation < -0.2)

zero_correlations <- pearson_target_cor_df %>%
  filter(Correlation >= -0.2 & Correlation <= 0.2)

ggplot(positive_correlations, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_point(color = "red", size = 3) +
  coord_flip() +
  labs(title = "Positive Correlations with Target", x = "Variables", y = "Correlation Coefficient (r)") +
  theme_minimal()

ggplot(negative_correlations, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_point(color = "green", size = 3) +
  coord_flip() +
  labs(title = "Negative Correlations with Target", x = "Variables", y = "Correlation Coefficient (r)") +
  theme_minimal()

ggplot(zero_correlations, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_point(color = "black", size = 3) +
  coord_flip() +
  labs(title = "Zero Correlations with Target", x = "Variables", y = "Correlation Coefficient (r)") +
  theme_minimal()

ggplot(pearson_target_cor_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Pearson Correlation with Target", x = "Variables", y = "Correlation Coefficient (r)") +
  theme_minimal()

pearson_filtered_columns <- names(pearson_target_cor[abs(pearson_target_cor) > 0.2])
pearson_filtered_data <- heart_data[, pearson_filtered_columns]
pearson_filtered_cor <- cor(pearson_filtered_data, method = "pearson", use = "complete.obs")
corrplot(pearson_filtered_cor, method = "color", tl.cex = 0.8, number.cex = 0.7)

heart_data_factor <- heart_data %>%
  mutate(
    age = as.factor(age),
    sex = as.factor(sex),
    cp = as.factor(cp),
    trestbps = as.factor(trestbps),
    chol = as.factor(chol),
    fbs = as.factor(fbs),
    restecg = as.factor(restecg),
    thalach = as.factor(thalach),
    exang = as.factor(exang),
    slope = as.factor(slope),
    ca = as.factor(ca),
    thal = as.factor(thal),
    target = as.factor(target)
  )
str(heart_data_factor)
chi_squared_results <- data.frame(Variable = character(), p_value = numeric(), stringsAsFactors = FALSE)

for (col in colnames(heart_data_factor)) {
  if (col != target_var) {
    contingency_table <- table(heart_data_factor[[col]], heart_data_factor[[target_var]])
    test_result <- chisq.test(contingency_table, simulate.p.value = TRUE, B = 2000)
    chi_squared_results <- rbind(chi_squared_results, data.frame(Variable = col, p_value = test_result$p.value))
  }
}
print(chi_squared_results)

significant_vars <- chi_squared_results %>% filter(p_value < 0.05) %>% select(Variable)

ggplot(chi_squared_results, aes(x = reorder(Variable, p_value), y = p_value)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() +
  labs(title = "Chi-Squared Test p-values (Monte Carlo)", x = "Variables", y = "p-value") +
  theme_minimal()

chi_squared_filtered_data <- heart_data[, c(significant_vars$Variable, target_var)]
print(chi_squared_filtered_data)

ggplot(heart_data, aes(x = age, y = chol, color = as.factor(target))) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs Cholesterol", x = "Age", y = "Cholesterol", color = "Heart Disease") +
  theme_minimal()

ggplot(heart_data, aes(x = as.factor(target), y = chol, fill = as.factor(target))) +
  geom_violin() +
  labs(title = "Violin Plot of Cholesterol Levels by Heart Disease Status", x = "Heart Disease", y = "Cholesterol") +
  theme_minimal()

ggplot(heart_data, aes(x = age, y = thalach, group = 1)) +
  geom_line(color = "blue") +
  labs(title = "Line Graph of Max Heart Rate Over Age", x = "Age", y = "Max Heart Rate") +
  theme_minimal()
