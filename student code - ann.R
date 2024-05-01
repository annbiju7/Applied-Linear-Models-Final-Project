library(dplyr)
library(MASS)

student <- read.csv("C:/ann/spring 2023/stat 4355/project/student.csv")

# ******************************************************************************
# THE FOLLOWING ARE THE QUESTIONS WE ARE TRYING TO ANSWER:

# do both males and females have the same percentage of mental health distress 
# in the industry as in academia?

# test various treatment variable vs mental health in both tech and academia - ANN

# mental health consequence & depression, anxiety, and panic attacks in both 
# datasets - CINDY
# ******************************************************************************

# cleaning the student data set
students <- student[, c(-1, -3, -5, -7)]

clean.students <- students[which(students$What.is.your.course. == "Engineering"|
                            students$What.is.your.course. == "BIT" |
                            students$What.is.your.course. == "BCS" |
                            students$What.is.your.course. == "Biotechnology"), ]

# changing column names to be less wordy
colnames(clean.students)[1] = "gender"
colnames(clean.students)[2] = "major"
colnames(clean.students)[3] = "gpa"
colnames(clean.students)[4] = "depression"
colnames(clean.students)[5] = "anxiety"
colnames(clean.students)[6] = "panic.attack"
colnames(clean.students)[7] = "treatment"

# QUESTION 2 - logistic regression
clean.students$depression <- ifelse(clean.students$depression == "Yes", 1, 0)
clean.students$anxiety <- ifelse(clean.students$anxiety == "Yes", 1, 0)
clean.students$panic.attack <- ifelse(clean.students$panic.attack == "Yes", 1, 0)
clean.students$treatment <- ifelse(clean.students$treatment == "Yes", 1, 0)
View(clean.students)

sapply(clean.students[, 4:7], sd)
xtabs(~treatment + depression + panic.attack + anxiety, data = clean.students)

model <- glm(treatment ~ depression + panic.attack + anxiety, data = clean.students,
             family = "binomial")
summary(model)

# residual analysis
r.dev <- residuals(model, type = "deviance")
fm <- fitted(model)

o.plot <- plot(fm, r.dev, col = c("blue", "red"), pch = c(22, 17), 
               main = "Deviant Residual vs Fitted Residual Values",
     xlab = "Fitted", ylab = "Deviant")
legend("topleft", pch = c(22, 17), c("Fitted", "Deviant"), col = c("blue", "red"))
abline(lm(fm ~ r.dev), col = "black")

# transforming the data
r.dev <- log10(r.dev)

new.plot <- plot(fm, r.dev, col = c("blue", "red"), pch = c(22, 17), 
                 main = "Deviant Residual vs Fitted Residual Values",
                 xlab = "Fitted", ylab = "Deviant")
legend("topleft", pch = c(22, 17), c("Fitted", "Deviant"), col = c("blue", "red"))
abline(lm(fm ~ r.dev), col = "black")

# model selection
step.mod <- stepAIC(model, direction = "both")
summary(step.mod)

# Conclusion:
# Based on the summary output of the stepwise regression model, it appears that 
# getting treatment does have a significant effect on the various of mental 
# health issues experienced by the students. The coefficients for the depression,
# anxiety, and panic attacks are positive and statistically significant.


