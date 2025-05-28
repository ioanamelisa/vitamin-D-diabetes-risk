# ---- Vitamin D and Diabetes Risk: Reproduction Project ----
# Simulated based on Pittas et al. (2023)

library(epiR)

set.seed(2025)
n <- 1000

# Diabetes incidence rates from the study
vit_d_rate <- 0.091
placebo_rate <- 0.107

vit_d <- rbinom(n, 1, vit_d_rate)  # 1 = developed diabetes, 0 = no diabetes
placebo <- rbinom(n, 1, placebo_rate)

group <- c(rep("Vitamin D", n), rep("Placebo", n))
diabetes <- c(vit_d, placebo)

data <- data.frame(Group = factor(group), Diabetes = diabetes)

table_data <- table(data$Group, data$Diabetes)
colnames(table_data) <- c("No Diabetes", "Diabetes")
print("Contingency Table:")
print(table_data)

# Chi-square test
chisq_result <- chisq.test(table_data)
print("Chi-square Test Result:")
print(chisq_result)

# Relative Risk and 95% Confidence Interval using epi.2by2
epi_table <- matrix(c(sum(vit_d), n - sum(vit_d),
                      sum(placebo), n - sum(placebo)),
                    nrow = 2, byrow = TRUE)

rownames(epi_table) <- c("Vitamin D", "Placebo")
colnames(epi_table) <- c("Diabetes", "No Diabetes")

epi_result <- epi.2by2(epi_table, method = "cohort.count", conf.level = 0.95)
print("Relative Risk and 95% CI:")
print(epi_result)

# Logistic Regression
data$Diabetes <- as.factor(data$Diabetes)
log_model <- glm(Diabetes ~ Group, data = data, family = binomial)
summary(log_model)

# Get predicted probabilities for each group
pred_probs <- aggregate(as.numeric(as.character(data$Diabetes)), 
                        by = list(data$Group), 
                        FUN = mean)
names(pred_probs) <- c("Group", "DiabetesRate")
print(pred_probs)

counts <- table(data$Group, data$Diabetes)[, "1"]
barplot_heights <- as.numeric(counts)

bp <- barplot(barplot_heights,
              beside = TRUE,
              col = c("green", "red"),
              names.arg = c("Vitamin D", "Placebo"),
              ylim = c(0, max(barplot_heights) + 10),
              ylab = "Number of Cases",
              main = "Diabetes Cases by Group")

percent_labels <- paste0(round((barplot_heights / n) * 100, 1), "%")
text(x = bp, y = barplot_heights + 5, labels = percent_labels, col = "blue")

postscript("incidence_eps_figure.eps", horizontal=FALSE, onefile=FALSE,
           paper="special", width=6, height=4)
bp <- barplot(barplot_heights,
              beside = TRUE,
              col = c("green", "red"),
              names.arg = c("Vitamin D", "Placebo"),
              ylim = c(0, max(barplot_heights) + 10),
              ylab = "Number of Cases",
              main = "Diabetes Cases by Group")
text(x = bp, y = barplot_heights + 5, labels = percent_labels, col = "blue")
dev.off()
