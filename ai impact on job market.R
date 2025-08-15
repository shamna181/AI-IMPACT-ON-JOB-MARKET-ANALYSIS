# install package
install.packages("dplyr")
library(dplyr)


# load data set
df <- read.csv("C:\\Users\\USER\\OneDrive\\Documents\\final project\\ai_job_trends_dataset.csv")
View(df)

head(df)
str(df)
summary(df)


# T-Test (Compare two group means)
# Test whether Median Salary differs significantly between High vs Low Gender Diversity groups,

# Create groups based on Gender Diversity median
med <- median(df$Gender.Diversity...., na.rm = TRUE)
group <- ifelse(df$Gender.Diversity.... > med, "High", "Low")
# Run t-test
result <- t.test(df$Median.Salary..USD. ~ group)
print(result)
# Print simplified interpretation
if(result$p.value < 0.05) {
  print("The difference is statistically significant( p < 0.05\n")
} else {
  print("The difference is not statistically significant (p > 0.05\n")
}



# F-Test (Compare variances of two groups)
#  Do high-risk and low-risk job groups have different variance in salary?
# Create a RiskGroup column based on Automation Risk threshold
df$RiskGroup <- ifelse(df$`Automation.Risk....` > 70, "High Risk", "Low/Moderate Risk")
# Now run the F-test
result2<-var.test(`Median.Salary..USD.` ~ RiskGroup, data = df)
print(result2)
if(result2$p.value < 0.05) {
  print("The difference is statistically significant( p < 0.05\n")
} else {
  print("The difference is not statistically significant (p > 0.05\n")
}



# Z-Test (Compare mean of one group to a known value)
# Is the average Automation Risk (%) significantly different from 50?
install.packages("BSDA")
library(BSDA)
result3<-z.test(df$`Automation.Risk....`, mu = 50, sigma.x = sd(df$`Automation.Risk....`, na.rm=TRUE))
print(result3)
if(result3$p.value < 0.05) {
  print("The difference is statistically significant( p < 0.05\n")
} else {
  print("The difference is not statistically significant (p > 0.05\n")
}



# ANOVA (Compare means across 3 or more groups)
# checks if the mean number of job openings (2024) differs significantly across different levels of required education.

anova_result <- aov(`Job.Openings..2024.` ~ `Required.Education`, data = df)
summary_result <- summary(anova_result)
pval <- summary_result[[1]][["Pr(>F)"]][1]

if (pval < 0.05) {
  print("significant: difference in job openings across different education levels in 2024")
} else {
  print("Not significant")
}
# There is a statistically significant difference in job openings across different education levels in 2024.


 
# Chi-Square Test
# checks whether there is a significant association between the type of industry and the education level required.
chisq_result1 <- chisq.test(table(df$Industry, df$Required.Education))
print(chisq_result1)
if(chisq_result1$p.value < 0.05) {
  print("The difference is statistically significant( p < 0.05\n")
} else {
  print("The difference is not statistically significant (p > 0.05\n")
}
#  There is a significant relationship between Industry and Required Education


