
# =====================================================
# MINI PROJECT: STUDENT PERFORMANCE ANALYSIS USING R
# MCA Data Science
# =====================================================

# 1. Load the required package
library(ggplot2)

# 2. Generate sample student data
set.seed(123)

n <- 100

students <- data.frame(
  Student_ID = 1:n,
  Study_Hours = round(runif(n, 1, 8), 1),
  Attendance = round(runif(n, 50, 100), 1),
  Previous_Marks = round(runif(n, 35, 90), 1)
)

# Generate final marks with random variation
students$Final_Marks <- round(
  0.35 * students$Previous_Marks +
    3.0 * students$Study_Hours +
    0.20 * students$Attendance +
    rnorm(n, mean = 0, sd = 7)
)

# Keep marks within 0-100
students$Final_Marks <- pmax(
  0, pmin(100, students$Final_Marks)
)

# 3. View and inspect the dataset
print(head(students))
print(dim(students))
str(students)
summary(students)

# 4. Check missing values
print(colSums(is.na(students)))

# 5. Descriptive statistics
variables <- students[, c(
  "Study_Hours", "Attendance",
  "Previous_Marks", "Final_Marks"
)]

statistics <- data.frame(
  Variable = names(variables),
  Mean = sapply(variables, mean),
  Median = sapply(variables, median),
  Standard_Deviation = sapply(variables, sd),
  Variance = sapply(variables, var),
  Minimum = sapply(variables, min),
  Maximum = sapply(variables, max)
)

print(cbind(
  Variable = statistics$Variable,
  round(statistics[, -1], 2)
))

# 6. Correlation analysis
correlation_matrix <- cor(variables)
print(round(correlation_matrix, 2))

# Test the relationship between study hours
# and final marks
correlation_test <- cor.test(
  students$Study_Hours,
  students$Final_Marks,
  method = "pearson"
)

print(correlation_test)

# 7. Hypothesis testing
# H0: Mean final marks = 50
# H1: Mean final marks != 50

mean_test <- t.test(
  students$Final_Marks,
  mu = 50
)

print(mean_test)

# 8. Create performance categories
students$Performance <- cut(
  students$Final_Marks,
  breaks = c(-Inf, 40, 60, 75, Inf),
  labels = c("Low", "Average", "Good", "Excellent")
)

print(table(students$Performance))

# 9. Visualizations

# Histogram of final marks
print(
  ggplot(students, aes(x = Final_Marks)) +
    geom_histogram(
      bins = 10,
      fill = "steelblue",
      color = "white"
    ) +
    labs(
      title = "Distribution of Final Marks",
      x = "Final Marks",
      y = "Number of Students"
    ) +
    theme_minimal()
)

# Study hours vs final marks
print(
  ggplot(students, aes(
    x = Study_Hours, y = Final_Marks
  )) +
    geom_point(color = "darkblue") +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = "Study Hours vs Final Marks",
      x = "Study Hours per Day",
      y = "Final Marks"
    ) +
    theme_minimal()
)

# Attendance vs final marks
print(
  ggplot(students, aes(
    x = Attendance, y = Final_Marks
  )) +
    geom_point(color = "darkgreen") +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = "Attendance vs Final Marks",
      x = "Attendance (%)",
      y = "Final Marks"
    ) +
    theme_minimal()
)

# Performance category bar chart
print(
  ggplot(students, aes(x = Performance)) +
    geom_bar(fill = "orange") +
    labs(
      title = "Student Performance Categories",
      x = "Performance Category",
      y = "Number of Students"
    ) +
    theme_minimal()
)

# Box plot of final marks
print(
  ggplot(students, aes(
    y = Final_Marks
  )) +
    geom_boxplot(fill = "lightblue") +
    labs(
      title = "Spread of Final Marks",
      y = "Final Marks"
    ) +
    theme_minimal()
)

# 10. Linear regression model
model <- lm(
  Final_Marks ~ Study_Hours +
    Attendance + Previous_Marks,
  data = students
)

print(summary(model))

# 11. Export results
write.csv(
  students,
  "student_performance_data.csv",
  row.names = FALSE
)

write.csv(
  statistics,
  "descriptive_statistics.csv",
  row.names = FALSE
)

capture.output(
  summary(model),
  file = "regression_results.txt"
)

# 12. Display final confirmation
cat("Student Performance Analysis completed.\n")
cat("Dataset and statistical results exported.\n")
