install.packages(c("dplyr", "ggplot2", "corrplot", "fmsb"))
library(ggplot2)
library(corrplot)
library(dplyr)
library(fmsb)

heart_data <- read.csv("E:/heart.csv", header = TRUE, sep = ",")
head(heart_data)
str(heart_data)

heart_data$sex <- factor(heart_data$sex, 
                         levels = c(1, 0), 
                         labels = c("male", "female"))

heart_data$cp <- factor(heart_data$cp, 
                        levels = c(0, 1, 2, 3), 
                        labels = c("Typical angina", "Atypical angina", "Non-anginal pain", "Asymptomatic"))

heart_data$fbs <- factor(heart_data$fbs, 
                         levels = c(1, 0), 
                         labels = c("true", "false"))

heart_data$restecg <- factor(heart_data$restecg, 
                             levels = c(0, 1, 2), 
                             labels = c("Normal", "ST-T abnormality", "Left ventricular hypertrophy"))

heart_data$exang <- factor(heart_data$exang, 
                           levels = c(1, 0), 
                           labels = c("yes", "no"))

heart_data$slope <- factor(heart_data$slope, 
                           levels = c(0, 1, 2), 
                           labels = c("Upsloping", "Flat", "Downsloping"))

heart_data$thal <- factor(heart_data$thal, 
                          levels = c(1, 2, 3), 
                          labels = c("Normal", "Fixed defect", "Reversible defect"))

heart_data$target <- factor(heart_data$target, 
                            levels = c(1, 0), 
                            labels = c("disease present", "no disease"))

missing_values <- colSums(is.na(heart_data))
print(missing_values)

heart_data <- na.omit(heart_data)

str(heart_data)
summary(heart_data)

mean_age <- mean(heart_data$age)
median_age <- median(heart_data$age)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_age <- get_mode(heart_data$age)

age_density <- density(heart_data$age)

ggplot(heart_data, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1.2) +
  geom_vline(aes(xintercept = mean_age), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median_age), color = "green", linetype = "dashed") +
  geom_vline(aes(xintercept = mode_age), color = "blue", linetype = "dashed") +
  labs(title = "Age Distribution with Gaussian Overlay",
       x = "Age", y = "Density",size = 6, fontface = "bold") +
  annotate("text", x = max(heart_data$age) * 0.9, y = max(age_density$y) * 1.3, 
           label = paste("Mean =", round(mean_age, 2)), color = "red", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$age) * 0.9, y = max(age_density$y) * 1.2, 
           label = paste("Median =", round(median_age, 2)), color = "green", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$age) * 0.9, y = max(age_density$y) * 1.1, 
           label = paste("Mode =", mode_age), color = "blue", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$age) * 0.5, y = max(age_density$y) * 1.5, 
           label = "Negative Skewness", color = "orange", size = 5, fontface = "bold") +
  theme_minimal()

mean_trestbps <- mean(heart_data$trestbps)
median_trestbps <- median(heart_data$trestbps)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_trestbps <- get_mode(heart_data$trestbps)

trestbps_density <- density(heart_data$trestbps)
ggplot(heart_data, aes(x = trestbps)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightpink", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1.2) +
  geom_vline(aes(xintercept = mean_trestbps), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median_trestbps), color = "darkgreen", linetype = "dashed") +
  geom_vline(aes(xintercept = mode_trestbps), color = "blue", linetype = "dashed") +
  labs(title = "Resting Blood Pressure Distribution with Gaussian Overlay",
       x = "Resting Blood Pressure (mm Hg)", y = "Density") +
  annotate("text", x = max(heart_data$trestbps) * 0.9, y = max(trestbps_density$y) * 1.3, 
           label = paste("Mode =", mode_trestbps), color = "blue", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$trestbps) * 0.9, y = max(trestbps_density$y) * 1.2, 
           label = paste("Median =", round(median_trestbps, 2)), color = "darkgreen", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$trestbps) * 0.9, y = max(trestbps_density$y) * 1.1, 
           label = paste("Mean =", round(mean_trestbps, 2)), color = "red", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$trestbps) * 0.5, y = max(trestbps_density$y) * 1.5, 
           label = "Positive Skewness", color = "orange", size = 5, fontface = "bold") +
  theme_minimal()

mean_thalach <- mean(heart_data$thalach)
median_thalach <- median(heart_data$thalach)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_thalach <- get_mode(heart_data$thalach)

thalach_density <- density(heart_data$thalach)
ggplot(heart_data, aes(x = thalach)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "purple", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1.2) +
  geom_vline(aes(xintercept = mean_thalach), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median_thalach), color = "green", linetype = "dashed") +
  geom_vline(aes(xintercept = mode_thalach), color = "blue", linetype = "dashed") +
  labs(title = "Maximum Heart Rate Distribution with Gaussian Overlay",
       x = "Heart Rate", y = "Density") +
  annotate("text", x = max(heart_data$thalach) * 0.9, y = max(thalach_density$y) * 1.3, 
           label = paste("Mean =", round(mean_thalach, 2)), color = "red", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$thalach) * 0.9, y = max(thalach_density$y) * 1.2, 
           label = paste("Median =", round(median_thalach, 2)), color = "green", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$thalach) * 0.9, y = max(thalach_density$y) * 1.1, 
           label = paste("Mode =", mode_thalach), color = "blue", size = 4, fontface = "bold") +
  annotate("text", x = max(heart_data$thalach) * 0.5, y = max(thalach_density$y) * 1.5, 
           label = "Negative Skewness", color = "orange", size = 5, fontface = "bold") +
  theme_minimal()

gender_distribution <- as.data.frame(table(heart_data$sex))
colnames(gender_distribution) <- c("Gender", "Frequency")
ggplot(gender_distribution, aes(x = Gender, y = Frequency, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  labs(title = "Gender Distribution", x = "Gender", y = "Frequency") +
  geom_text(aes(label = Frequency), position = position_dodge(0.9), vjust = -0.5, size = 4) +
  theme_minimal()

cp_distribution <- as.data.frame(table(heart_data$cp))
colnames(cp_distribution) <- c("Chest_Pain_Type", "Frequency")
ggplot(cp_distribution, aes(x = Chest_Pain_Type, y = Frequency, fill = Chest_Pain_Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("lightgreen", "lightblue", "lightcoral", "lightgoldenrod")) +
  labs(title = "Chest Pain Type Distribution", x = "Chest Pain Type", y = "Frequency") +
  geom_text(aes(label = Frequency), position = position_dodge(0.9), vjust = -0.5, size = 4) +
  theme_minimal()

target_distribution <- as.data.frame(table(heart_data$target))
colnames(target_distribution) <- c("Heart_Disease", "Frequency")
ggplot(target_distribution, aes(x = Heart_Disease, y = Frequency, fill = Heart_Disease)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("lightcoral", "lightgreen")) +
  labs(title = "Heart Disease Presence", 
       x = "Heart Disease (1 = Yes, 0 = No)", 
       y = "Frequency") +
  geom_text(aes(label = Frequency), 
            position = position_dodge(0.9), vjust = -0.5, size = 4) + theme_minimal()

boxplot(heart_data$trestbps, main = "Boxplot of Resting Blood Pressure", ylab = "Resting Blood Pressure (mm Hg)", col = "lightgreen", border = "black")
boxplot(heart_data$chol, main = "Boxplot of Serum Cholesterol", ylab = "Cholesterol (mg/dl)", col = "lightpink", border = "black")
boxplot(heart_data$thalach, main = "Boxplot of Maximum Heart Rate", ylab = "Heart Rate", col = "skyblue", border = "black")

ggplot(heart_data, aes(x = age, y = chol, color = target)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Age vs Cholesterol (Heart Disease Presence)",
       x = "Age", y = "Cholesterol") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

ggplot(heart_data, aes(x = age, y = thalach, color = sex)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(title = "Maximum Heart Rate vs Age (Gender)",
       x = "Age", y = "Maximum Heart Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

ggplot(heart_data, aes(x = chol, y = trestbps, color = cp)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Cholesterol vs Resting Blood Pressure (Chest Pain Type)",
       x = "Cholesterol", y = "Resting Blood Pressure") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

ggplot(heart_data, aes(x = as.factor(target), y = chol, fill = as.factor(target))) +
  geom_violin(trim = FALSE, color = "black") + 
  geom_boxplot(width = 0.1, color = "black", fill = "black", alpha = 0.5, outlier.shape = NA) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2, color = "white", fill = "white") +
  scale_fill_manual(values = c("pink", "lightblue")) + 
  labs(title = "Distribution of Cholesterol by Heart Disease Presence",
       x = "Heart Disease Presence", y = "Cholesterol",
       fill = "Heart Disease Presence") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(heart_data, aes(x = as.factor(sex), y = thalach, fill = as.factor(sex))) +
  geom_violin(trim = FALSE, color = "black") + 
  geom_boxplot(width = 0.1, color = "black", fill = "black", alpha = 0.5, outlier.shape = NA) + 
  stat_summary(fun = median, geom = "point", shape = 21, size = 2, color = "white", fill = "white") + 
  scale_fill_manual(values = c("coral", "lightgoldenrod")) + 
  labs(title = "Distribution of Maximum Heart Rate by Gender",
       x = "Gender", y = "Maximum Heart Rate",
       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(heart_data, aes(x = as.factor(cp), y = trestbps, fill = as.factor(cp))) +
  geom_violin(trim = FALSE, color = "black") +
  geom_boxplot(width = 0.1, color = "black", fill = "black", alpha = 0.5, outlier.shape = NA) + 
  stat_summary(fun = median, geom = "point", shape = 21, size = 2, color = "white", fill = "white") + 
  scale_fill_manual(values = c("lightgreen", "purple", "lightblue", "orange")) + 
  labs(title = "Distribution of Resting Blood Pressure by Chest Pain Type",
       x = "Chest Pain Type", y = "Resting Blood Pressure",
       fill = "Chest Pain Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

data_values <- rbind(
  max_min = data.frame(age = c(80, 20), chol = c(600, 100), trestbps = c(200, 80), thalach = c(220, 70)),
  Disease = c(60, 200, 120, 180),No_Disease = c(40, 150, 90, 160)
)
colors_border <- c("pink", "lightgreen")
colors_in <- c(rgb(1, 0.75, 0.8, 0.3), rgb(0.6, 1, 0.6, 0.3))

radarchart(data_values, axistype = 1, pcol = colors_border, pfcol = colors_in,
           plwd = 2, cglcol = "grey", cglty = 1, axislabcol = "black",
           caxislabels = seq(0, 80, 20), cglwd = 0.8, vlcex = 0.8)
legend(x = 0.8, y = -0.15, legend = c("Disease", "No Disease"), 
       bty = "n", pch = 20, col = colors_border, text.col = "black", cex = 0.8, pt.cex = 2)

ggplot(heart_data, aes(x = chol, y = trestbps, color = target)) +
  geom_line(stat = "summary", fun = mean, aes(group = target), size = 1) +
  geom_point(stat = "summary", fun = mean, size = 3, alpha = 0.7) +
  labs(title = "Cholesterol vs. Resting Blood Pressure",
       x = "Cholesterol",
       y = "Resting Blood Pressure") +
  scale_color_manual(values = c("blue", "orange")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

ggplot(heart_data, aes(x = thalach, y = oldpeak, color = target)) +
  geom_line(stat = "summary", fun = mean, aes(group = target), size = 1) +
  geom_point(stat = "summary", fun = mean, size = 3, alpha = 0.7) +
  labs(title = "Maximum Heart Rate vs. Oldpeak",
       x = "Maximum Heart Rate",
       y = "Oldpeak (ST Depression)") +
  scale_color_manual(values = c("green", "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

ggplot(heart_data, aes(x = chol, y = thalach, color = target)) +
  geom_line(stat = "summary", fun = mean, aes(group = target), size = 1) +
  geom_point(stat = "summary", fun = mean, size = 3, alpha = 0.7) +
  labs(title = "Cholesterol vs. Maximum Heart Rate",
       x = "Cholesterol",
       y = "Maximum Heart Rate") +
  scale_color_manual(values = c("purple", "yellow")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
