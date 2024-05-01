library(dplyr)
library(MASS)
student <- read.csv("student.csv")

# cleaning the student data set
students <- student[, c(-1, -3)]
students <- students[which(student$What.is.your.course. == "Engineering" |
                             student$What.is.your.course. == "BIT" |
                             student$What.is.your.course. == "BCS" |
                             student$What.is.your.course. == "Biotechnology"), ]

#Change colnames
colnames(students)[1] = "gender"
colnames(students)[2] = "major"
colnames(students)[3] = "year"
colnames(students)[4] = "gpa"
colnames(students)[6] = "depression"
colnames(students)[7] = "anxiety"
colnames(students)[8] = "panic_attack"
colnames(students)[9] = "treatment"

# Delete colnames = year and marital.status
students <- students[, -c(3, 5)] 

# make a new column for yes/no in depression, anxiety, and panic_attack, treatment transform to 0/ 1
students$depression <- ifelse(students$depression == "Yes", 1, 0)
students$anxiety <- ifelse(students$anxiety == "Yes", 1, 0)
students$panic_attack <- ifelse(students$panic_attack == "Yes", 1, 0)
students$treatment <- ifelse(students$treatment == "Yes", 1, 0)

for (i in seq_along(students$gpa)) {
  gpa_str <- students$gpa[i]
  if (gpa_str == "0 - 1.99") {
    students$gpa[i] <- 1
  } else if (gpa_str == "2.00 - 2.49") {
    students$gpa[i] <- 2
  } else if (gpa_str == "2.50 - 2.99") {
    students$gpa[i] <- 3
  } else if (gpa_str == "3.00 - 3.49") {
    students$gpa[i] <- 4
  } else if (gpa_str == "3.50 - 4.00") {
    students$gpa[i] <- 5
  }
}

students$gpa_group <- ifelse(students$gpa < 3, "low", ifelse(students$gpa > 3, "high", "medium"))
gpa_issues <- aggregate(cbind(depression, anxiety, panic_attack) ~ gpa_group, data = students, FUN = sum)
gpa_issues$gpa_group <- factor(gpa_issues$gpa_group, levels = c("low", "medium", "high"))
gpa_issues<- arrange(gpa_issues, gpa_group)
gpa_issues$total_issues <- gpa_issues$depression + gpa_issues$anxiety + gpa_issues$panic_attack

model <- glm(total_issues ~ gpa_group, data = gpa_issues, family = poisson())
summary(model)

#Residual Anaysis
residuals_deviance <- residuals(model, type = "deviance")
plot(fitted(model), residuals_deviance, main = "Deviance Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(0, 0, col = "red")

#Variable stepwise
stepwise_model <- stepAIC(model, direction = "both")
stepwise_model
summary(stepwise_model)

exp(coef(stepwise_model))









