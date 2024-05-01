library(readr)
library(dplyr)
library(tidyverse)
library(aod)
library(tibble)
library(MASS)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(car)
library(cowplot)
library(reshape2)
mental_health <- read_csv("mental_health_in_tech.csv")
#View(mental_health)


#CLEANING ______________________________________________________________________
#removing unused data
mental_health <- subset(mental_health, select = -c(Timestamp, state, self_employed, 
                                                   no_employees, comments))

#omit NA's
mental_health <- na.omit(mental_health)


#dim(mental_health)

#GENDER ________________________________________________________________________
#Male
index_M <- which(mental_health$Gender == "Male")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "male")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "m")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "Cis Man")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "maile")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "Cis Male")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "Mal")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "Male (CIS)")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "Make")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "Man")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "msle")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "Mail")
mental_health$Gender[index_M] <- "M"

index_M <- which(mental_health$Gender == "cis male")
mental_health$Gender[index_M] <- "M"
index_M <- which(mental_health$Gender == "Malr")
mental_health$Gender[index_M] <- "M"


#Female
index_F <- which(mental_health$Gender == "Female")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "female")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "f")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "Cis Female")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "Woman")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "woman")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "Femake")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "cis-female/femme")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "Female (cis)")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "femail")
mental_health$Gender[index_F] <- "F"
index_F <- which(mental_health$Gender == "Woman")
mental_health$Gender[index_F] <- "F"


#Other
index_O <- which(mental_health$Gender == "Male-ish")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "something kinda male?")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Trans-female")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "queer/she/they")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "non-binary")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Nah")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "All")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Enby")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "fluid")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Genderqueer")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Androgyne")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Agender")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "male leaning androgynous")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Guy (-ish) ^_^")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Female (trans)")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Neuter")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "queer")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "A little about you")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "p")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "ostensibly male, unsure what that really means")
mental_health$Gender[index_O] <- "O"
index_O <- which(mental_health$Gender == "Trans woman")
mental_health$Gender[index_O] <- "O"

mental_health$Gender <- factor(mental_health$Gender)





#FAMILY HISTORY_________________________________________________________________

index_fh_true <- which(mental_health$family_history =="Yes")
mental_health$family_history[index_fh_true] <- TRUE

index_fh_false <- which(mental_health$family_history =="No")
mental_health$family_history[index_fh_false] <- FALSE 

mental_health$family_history <- as.logical(mental_health$family_history)


#TREATMENT______________________________________________________________________
index_treat_true <- which(mental_health$treatment =="Yes")
mental_health$treatment[index_treat_true] <- TRUE

index_treat_false <- which(mental_health$treatment =="No")
mental_health$treatment[index_treat_false] <- FALSE 

mental_health$treatment <- as.logical(mental_health$treatment)



#WORK_INTERFERE_________________________________________________________________

mental_health$work_interfere <- factor(mental_health$work_interfere,
                                       levels = c("Never", "Rarely", "Sometimes",
                                                  "Often"))


#REMOTE WORK____________________________________________________________________
rm_false <- which(mental_health$remote_work == "No")
mental_health$remote_work[rm_false] <- FALSE
rm_true <- which(mental_health$remote_work == "Yes")
mental_health$remote_work[rm_true] <- TRUE

mental_health$remote_work <- as.logical(mental_health$remote_work)

#TECH COMPANY___________________________________________________________________
tc_false <- which(mental_health$tech_company == "No")
mental_health$tech_company[tc_false] <- FALSE
tc_true <- which(mental_health$tech_company == "Yes")
mental_health$tech_company[tc_true] <- TRUE

mental_health$tech_company <- as.logical(mental_health$tech_company)


#BENEFITS_______________________________________________________________________
mental_health$benefits <- factor(mental_health$benefits,
                                 levels = c("No", "Don't know", "Yes"))

#CARE_OPTIONS___________________________________________________________________
index_NS <- which(mental_health$care_options == "Not sure")
mental_health$care_options[index_NS] <- "Don't know"


mental_health$care_options <- factor(mental_health$care_options,
                                     levels = c("No", "Don't know", "Yes"))


#WELLNESS_PROGRAM_______________________________________________________________
mental_health$wellness_program <- factor(mental_health$wellness_program,
                                         levels = c("No", "Don't know", "Yes"))


#SEEK_HELP______________________________________________________________________
mental_health$seek_help <- factor(mental_health$seek_help,
                                  levels = c("No", "Don't know", "Yes"))


#ANONIMITY______________________________________________________________________
mental_health$anonymity <- factor(mental_health$anonymity,
                                  levels = c("No", "Don't know", "Yes"))

#LEAVE__________________________________________________________________________
mental_health$leave <- factor(mental_health$leave,
                              levels = c("Don't know", "Very difficult", "Somewhat difficult",
                                         "Somewhat easy", "Very easy"))

#MENTAL_HEALTH_DATA_____________________________________________________________
mental_health$mental_health_consequence <- factor(mental_health$mental_health_consequence,
                                                  levels = c("No", "Maybe", "Yes"))

#PHYS_HEALTH_CONS_____________________________________________________________
mental_health$phys_health_consequence <- factor(mental_health$phys_health_consequence,
                                                levels = c("No", "Maybe", "Yes"))

#COWORKERS_____________________________________________________________
mental_health$coworkers <- factor(mental_health$coworkers,
                                  levels = c("No", "Some of them", "Yes"))

#COWORKERS_____________________________________________________________
mental_health$supervisor <- factor(mental_health$supervisor,
                                   levels = c("No", "Some of them", "Yes"))


#MENTAL_HEALTH_INTERVIEW_____________________________________________________________
mental_health$mental_health_interview <- factor(mental_health$mental_health_interview,
                                                levels = c("No", "Maybe", "Yes"))

#PHYS_HEALTH_INTERVIEW_____________________________________________________________
mental_health$phys_health_interview <- factor(mental_health$phys_health_interview,
                                              levels = c("No", "Maybe", "Yes"))

#MENTAL_HEALTH_INTERVIEW_____________________________________________________________
mental_health$mental_vs_physical<- factor(mental_health$mental_vs_physical,
                                          levels = c("No", "Don't know", "Yes"))

#OBSERVED NEGATIVE CONSEQUENCES
unique(mental_health$obs_consequence)
oc_false <- which(mental_health$obs_consequence == "No")
mental_health$obs_consequence[oc_false] <- FALSE
oc_true <- which(mental_health$obs_consequence == "Yes")
mental_health$obs_consequence[oc_true] <- TRUE
mental_health$obs_consequence <- as.logical(mental_health$obs_consequence)


#VARIABLE SELECTION ------------------------------------------------------------
#new data frame with only have valid predictors of mental health 
mental_health_valid <- cbind(mental_health[1:5], mental_health[7:8])
mh_valid_tech <- subset(mental_health_valid, 
                        mental_health_valid$tech_company == TRUE)
#creating logistic regression model 
large_model <- glm(treatment ~ Age + Gender + Country + family_history + 
                     remote_work, data = mh_valid_tech)
summary(large_model)            
# Using stepwise variable selection for model
transformed <- stepAIC(large_model, direction = "both")
summary(transformed)
transformed <- glm(formula = treatment ~ Gender + family_history, 
                   data = mh_valid_tech)

#visualizing probabilities using the transformed model
#VARIABLE SELECTION ------------------------------------------------------------

#VIF FOR MULTICOLLINEARITY -----------------------------------------------------
vif(large_model)
vif(transformed)
#VIF FOR MULTICOLLINEARITY -----------------------------------------------------

#INFLUENCE ANALYSIS ------------------------------------------------------------
summary(influence.measures(large_model))
summary(influence.measures(transformed))
infIndexPlot(transformed, vars = c("hat"), main = "Hat")
#INFLUENCE ANALYSIS ------------------------------------------------------------

#RESIDUAL ANALYSIS -------------------------------------------------------------
#on largest model
large_model_resid <- resid(large_model, type = "pearson")
lmr_standard <- large_model_resid / sd(large_model_resid)
hist(lmr_standard, main = "Standardized Pearson Residuals before Model Selection",
     xlab = "Standardized Pearson Residuals", breaks = 30)
#on best fit model
transformed_resid <- resid(transformed, type = "pearson")
trans_standard <- transformed_resid / sd(transformed_resid)
hist(trans_standard, main = "Standardized Pearson Residuals After Model Selection",
     xlab = "Standardized Pearson Residuals", breaks = 30)
#RESIDUAL ANALYSIS -------------------------------------------------------------

#to get QQ plots before and after model selection 
plot(large_model)
plot(transformed)

#encoding appropriate data to perform transformations
mhvt2 <- mh_valid_tech
mhvt2$Gender <-as.numeric(mhvt2$Gender)
mhvt2$family_history <- as.numeric(mhvt2$family_history)
mhvt2$treatment <- as.numeric(mhvt2$treatment)
mhvt2$tech_company <- as.numeric(mhvt2$tech_company)
mhvt2$remote_work <- as.numeric(mhvt2$remote_work)

#apply square root transformation to best model
trans <- glm(formula = sqrt(treatment) ~ Gender + family_history, 
                   data = mhvt2, 
                   family = binomial(link = "logit"))
plot(trans)
trans_resid_clean <- resid(trans, type = "pearson")
trans_standard_clean <- trans_resid_clean / sd(trans_resid_clean)
hist(trans_standard_clean, main = "Standardized Pearson Residuals After Model Transformation",
     xlab = "Standardized Pearson Residuals", breaks = 30)

#VALIDATION ____________________________________________________________________
{
  mental_health_valid <- cbind(mental_health[1:5], mental_health[7:8])
  mental_health_transf <- subset(mental_health_valid, 
                                 mental_health_valid$tech_company == TRUE)
}

#AGE AS FACTOR__________________________________________________________________
{mental_health_transf <- cbind(mental_health_transf, mental_health_transf$Age)
  
  colnames(mental_health_transf)[8] <- "Age_fact"
  
  index_10 <- which(mental_health_transf$Age_fact < 20)
  mental_health_transf$Age_fact[index_10] <- "Under 20"
  
  index_20 <- which(mental_health_transf$Age_fact < 30 & mental_health_transf$Age_fact >= 20)
  mental_health_transf$Age_fact[index_20] <- "20's"
  
  index_30 <- which(mental_health_transf$Age_fact < 40  & mental_health_transf$Age_fact >= 30)
  mental_health_transf$Age_fact[index_30] <- "30's"
  
  index_40 <- which(mental_health_transf$Age_fact < 50 & mental_health_transf$Age_fact >= 40)
  mental_health_transf$Age_fact[index_40] <- "40's"
  
  index_50 <- which(mental_health_transf$Age_fact < 60 & mental_health_transf$Age_fact >= 50)
  mental_health_transf$Age_fact[index_50] <- "50's"
  
  index_60 <- which(mental_health_transf$Age_fact < 70 & mental_health_transf$Age_fact >= 60)
  mental_health_transf$Age_fact[index_60] <- "60's"
  
  index_70 <- which(mental_health_transf$Age >= 70)
  mental_health_transf$Age_fact[index_70] <- "Over 70"
  
  mental_health_transf$Age_fact <- as.factor(mental_health_transf$Age_fact)
  
  levels(mental_health_transf$Age_fact) <- c("Under 20", "20's", "30's", "40's", "50's", "60's", "0ver 70")
  
}

#VARIABLE COUNT PLOTS___________________________________________________________
{
  wi <- ggplot(data = mental_health) +
    geom_bar(mapping = aes(x = work_interfere) , fill = "lightcyan2", color = "darkslategray3") + labs(x = "Interferes with Work (from original data)") + 
    geom_text(mapping = aes(x = work_interfere, label=..count..), stat = 'count') 
  
  
  fh <- ggplot(data = mental_health_transf) +
    geom_bar(mapping = aes(x = family_history) , fill = "lightcyan2", color = "darkslategray3") + labs(x = "Family History") + 
    geom_text(mapping = aes(x = family_history, label=..count..), stat = 'count') 
  
  gender <- ggplot(data = mental_health_transf) +
    geom_bar(mapping = aes(x = Gender) , fill = "lightcyan2",  color = "darkslategray3" ) + labs(x = "Gender") + 
    geom_text(mapping = aes(x = Gender, label=..count..), stat = 'count') 
  
  rw <- ggplot(data = mental_health_transf) +
    geom_bar(mapping = aes(x = remote_work) , fill = "lightcyan2",  color = "darkslategray3" ) + labs(x = "Remote Work") + 
    geom_text(mapping = aes(x = remote_work, label=..count..), stat = 'count') 
  
  age <- ggplot(data = mental_health_transf) +
    geom_bar(mapping = aes(x = as.factor(Age_fact)) , fill = "lightcyan2",  color = "darkslategray3" ) + labs(x = "Age") + 
    geom_text(mapping = aes(x = as.factor(Age_fact), label=..count..), stat = 'count') 
  
  
  plot_grid(rw, fh, gender, wi, age, nrow = 3)
  
}

#WORK INTERFERE PLOTS___________________________________________________________
{
  lm_workInfft = glm(treatment~work_interfere, data = mental_health, 
                     family = poisson())
  summary(lm_workInfft)
  
  ggplot(data = mental_health) +
    geom_bar(mapping = aes(x = treatment,  fill = work_interfere), 
             position = "fill", color = "white") +
    labs(title = " Treatment and Mental Health Interference with Work",
         x = "Treatment",
         y = "Work Interfere (percentage)",
         fill = "") +
    scale_fill_brewer(palette = "BuGn", direction = 1) + theme_bw() 
}

#FAMILY HISTORY PLOTS___________________________________________________________
{
  lm_fam = glm(treatment~family_history, data = mental_health_transf,
               family = binomial)
  summary(lm_fam)
  
  ggplot(data = mental_health_transf) +
    geom_bar(mapping = aes(x = treatment,  fill = family_history), 
             position = "fill", color = 'white') +
    labs(title = "Treatment and Family History ",
         x = "Treatment",
         y = "Family History (percentage)",
         fill = "") +
    scale_fill_brewer(palette = "BuGn", direction = -1) + theme_bw()
}

#GENDER PLOTS___________________________________________________________________
{
  lm_gen = glm(treatment~family_history, data = mental_health_transf,
               family = binomial)
  summary(lm_gen)
  
  ggplot(data = mental_health) +
    geom_bar(mapping = aes(x = Gender,  fill = treatment), 
             position = "fill", color = 'white') +
    labs(title = "Mental Health by Gender",
         x = "Treatment",
         y = "Gender (percentage)",
         fill = "") +
    scale_fill_brewer(palette = "BuGn", direction = -1) + theme_bw()
}    


