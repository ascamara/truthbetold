library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(stargazer)

#options(contrasts = c("contr.sum", "contr.poly"))

# Redirect text output to a file
sink("output_final.txt")

i<<-0
data <- read_sav("HXC23014 Harvard Poll Data.sav")

covid_questions <- select(data, "Q45","Q46", #Knowledge checks: 1 if true, 2 if false
                          "hQ47r1","hQ47r2","hQ47r3","hQ47r4", #eligibility for randomization on Q47, should all be 1
                          "hQ48r1","hQ48r2","hQ48r3","hQ48r4", #eligibility for randomization on Q48, should all be 1
                          "dQ47","dQ48", #treatment assignment for Q47, Q48
                          "hQ47Imgr1","hQ47Imgr2","hQ47Imgr3","hQ47Imgr4", #whether the respondent saw the img, condl on treatment assgm for Q47
                          "hQ48Imgr1","hQ48Imgr2", "hQ48Imgr3","hQ48Imgr4", #whether the respondent saw the img, condl on treatment assgm for Q48
                          "dQ47Imgr1","dQ47Imgr2", #id of img in each spot, condl on treatment assgm for Q47
                          "dQ48Imgr1","dQ48Imgr2", #id of img in each spot, condl on treatment assgm for Q48
                          "Q47","Q48") #whether the respondent clicked on the first or second image

# "Q38", #AI LIFE BETTER
# "Q41r6", #Social media
# "Q41r7", #mass media
# "Q42r1", #vax
# "Q42r2", #vax
# 
# 
# "Q1", #SEX
# "Q57", #age
# "Q59", #race
# "Q60", #hislat
# "Q61", #pid
# "Q65", #income
#+ controls$Q1 + controls$Q57 + controls$Q59 + controls$Q60 + controls$Q61 + controls$Q65 "classics"
#+ 
controls <- select(data,
                   "Q1", #SEX
                   "Q38", #AI LIFE BETTER
                   "Q41r6", #Social media
                   "Q41r7", #mass media
                   "Q42r1", #vax
                   "Q42r2", #vax
                   "Q49", #cheating
                   "Q57", #age
                   "Q59", #race
                   "Q60", #hislat
                   "Q61", #pid
                   "Q65", #income
                   "D103", #education
                   "QTimeStampQ47",
                   "QTimeStampQ48"
)
# "Q1", #SEX
# "Q57", #age
# "Q59", #race
# "Q60", #hislat
# "Q61", #pid
# "Q65", #income




# controls$Q1 <- relevel(controls$Q1, ref = "desired_reference_level")
# controls$Q59 <- relevel(controls$Q59, ref = "desired_reference_level")
# controls$Q60 <- relevel(controls$Q60, ref = "desired_reference_level")
# controls$Q61 <- relevel(controls$Q61, ref = "desired_reference_level")
# controls$Q65 <- relevel(controls$Q65, ref = "desired_reference_level")
# controls$Q38 <- relevel(controls$Q38, ref = "desired_reference_level")



# "Q38", #AI LIFE BETTER
# "Q41r6", #Social media
# "Q41r7", #mass media



analyse_data <- function(df) {
  # Fit a one-way ANOVA
  one.way <- aov(outcome ~ true_labeled * false_labeled, data = df)
  print(summary(one.way))
  
  # Fit a logistic regression model
  fit <- glm(outcome ~ true_labeled * false_labeled, family = binomial(link = "logit"), data = df)
  print(summary(fit))
  stargazer(fit, type = "latex")
  
  # Aggregate data
  data_agg <- aggregate(outcome ~ true_labeled + false_labeled, df, mean)
  
  # Convert to factor
  data_agg$true_labeled <- as.factor(data_agg$true_labeled)
  data_agg$false_labeled <- as.factor(data_agg$false_labeled)
  
  # Create and display the first plot
  p1 <- ggplot(data_agg, aes(x = true_labeled, y = outcome, color = false_labeled, group = false_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(x = "Fake News Labeled", y = "Pr(Fake News Selected)", color = "True News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  
  
  ggsave(paste0("plot_", i, ".png"), p1)
  print(i)
  i<<-i+1
  
  # Create and display the second plot
  p2 <- ggplot(data_agg, aes(x = false_labeled, y = outcome, color = true_labeled, group = true_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(x = "True News Labeled", y = "Pr(Fake News Selected)", color = "Fake News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  ggsave(paste0("plot_", i, ".png"), p2)
  print(i)
  i<<-i+1
}

analyse_data_mycontrols <- function(df) {
  # Fit a one-way ANOVA
  one.way <- aov(outcome ~ true_labeled * false_labeled + time + know, data = df)
  print(summary(one.way))
  
  # Fit a logistic regression model
  fit <- glm(outcome ~ true_labeled * false_labeled + time + know, family = binomial(link = "logit"), data = df)
  print(summary(fit))
  stargazer(fit, type = "latex")
  # Aggregate data
  data_agg <- aggregate(outcome ~ true_labeled + false_labeled, df, mean)
  data_agg$true_labeled <- as.factor(data_agg$true_labeled)
  data_agg$false_labeled <- as.factor(data_agg$false_labeled)
  # Create and display the first plot
  p1 <- ggplot(data_agg, aes(x = true_labeled, y = outcome, color = false_labeled, group = false_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(x = "Fake News Labeled", y = "Pr(Fake News Selected)", color = "True News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  
  ggsave(paste0("plot_", i, ".png"), p1)
  print(i)
  i<<-i+1
  
  # Create and display the second plot
  p2 <- ggplot(data_agg, aes(x = false_labeled, y = outcome, color = true_labeled, group = true_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(x = "True News Labeled", y = "Pr(Fake News Selected)", color = "Fake News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  ggsave(paste0("plot_", i, ".png"), p2)
  print(i)
  i<<-i+1
}

analyse_data_with_controls_basic <- function(df) {
  # Fit a one-way ANOVA
  one.way <- aov(outcome ~ true_labeled * false_labeled + Q1 + Q57 + Q59 + Q60 + Q61 + Q65, data = df)
  print(summary(one.way))
  
  # Fit a logistic regression model
  fit <- glm(outcome ~ true_labeled * false_labeled + Q1 + Q57 + Q59 + Q60 + Q61 + Q65, family = binomial(link = "logit"), data = df)
  print(summary(fit))
  stargazer(fit, type = "latex")
  
  df$predicted <- predict(fit, type = "response")
  data_agg <- aggregate(outcome ~ true_labeled + false_labeled, df, mean)
  data_agg$true_labeled <- as.factor(data_agg$true_labeled)
  data_agg$false_labeled <- as.factor(data_agg$false_labeled)
  # Create and display the first plot
  p1 <- ggplot(data_agg, aes(x = true_labeled, y = outcome, color = false_labeled, group = false_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    #geom_line(data = data_pred, aes(y = predicted), linetype = "dashed") +
    labs(x = "Fake News Labeled", y = "Pr(Fake News Selected)", color = "True News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  ggsave(paste0("plot_", i, ".png"), p1)
  print(i)
  i<<-i+1
  
  # Create and display the second plot
  p2 <- ggplot(data_agg, aes(x = false_labeled, y = outcome, color = true_labeled, group = true_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    #geom_line(data = data_pred, aes(y = predicted), linetype = "dashed") +
    labs(x = "True News Labeled", y = "Pr(Fake News Selected)", color = "Fake News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  ggsave(paste0("plot_", i, ".png"), p2)
  print(i)
  i<<-i+1
}

analyse_data_with_allcontrols <- function(df) {
  # Fit a one-way ANOVA
  one.way <- aov(outcome ~ true_labeled * false_labeled + Q1 + Q57 + Q59 + Q60 + Q61 + Q65 + time + know, data = df)
  print(summary(one.way))
  
  # Fit a logistic regression model
  fit <- glm(outcome ~ true_labeled * false_labeled + Q1 + Q57 + Q59 + Q60 + Q61 + Q65 + time + know, family = binomial(link = "logit"), data = df)
  print(summary(fit))
  stargazer(fit, type = "latex")
  df$predicted <- predict(fit, type = "response")
  data_pred <- aggregate(predicted ~ true_labeled + false_labeled, df, mean)
  data_agg <- aggregate(outcome ~ true_labeled + false_labeled, df, mean)
  data_agg$true_labeled <- as.factor(data_agg$true_labeled)
  data_agg$false_labeled <- as.factor(data_agg$false_labeled)
  # Create and display the first plot
  p1 <- ggplot(data_agg, aes(x = true_labeled, y = outcome, color = false_labeled, group = false_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    #geom_line(data = data_pred, aes(y = predicted), linetype = "dashed") +
    labs(x = "Fake News Labeled", y = "Pr(Fake News Selected)", color = "True News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  ggsave(paste0("plot_", i, ".png"), p1)
  print(i)
  i<<-i+1
  
  # Create and display the second plot
  p2 <- ggplot(data_agg, aes(x = false_labeled, y = outcome, color = true_labeled, group = true_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    #geom_line(data = data_pred, aes(y = predicted), linetype = "dashed") +
    labs(x = "True News Labeled", y = "Pr(Fake News Selected)", color = "Fake News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  ggsave(paste0("plot_", i, ".png"), p2)
  print(i)
  i<<-i+1
}


# raw analysis
data_R <- data.frame(treatment = covid_questions$dQ47,
                     outcome = covid_questions$Q47-1,
                     true_labeled = covid_questions$dQ47Imgr1 - 1,
                     false_labeled = covid_questions$dQ47Imgr2 - 3)
data_R$outcome <- as.numeric(as.character(data_R$outcome)) # Outcome should be numeric

data_R$Q1 <- controls$Q1
data_R$Q38 <- controls$Q38
data_R$Q41r6 <- controls$Q41r6
data_R$Q41r7 <- controls$Q41r7
data_R$Q42r1 <- controls$Q42r1
data_R$Q42r2 <- controls$Q42r2
data_R$Q49 <- controls$Q49
data_R$Q57 <- controls$Q57
data_R$Q59 <- controls$Q59
data_R$Q60 <- controls$Q60
data_R$Q61 <- controls$Q61
data_R$Q65 <- controls$Q65
data_R$D103 <- controls$D103
data_R$time <- controls$QTimeStampQ47
data_R$know <- covid_questions$Q45-1

data_R$Q1 <- factor(data_R$Q1, levels = c(1, 2, 3), labels = c("Male", "Female", "Other Sex"))
data_R$Q59 <- factor(data_R$Q59, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("White", "Black", "Asian", "AIAN", "MENA", "NHPI", "Other", NA))
data_R$Q60 <- factor(data_R$Q60, levels = c(1, 2, 3), labels = c("Hispanic/Latino", "Not Hispanic/Latino", "NR Hispanic/Latino"))
data_R$Q61 <- factor(data_R$Q61, levels = c(1, 2, 3, 4, 5), labels = c("Democratic", "Republican", "Independent", "Other", "Not Sure"))
data_R$Q65 <- factor(data_R$Q65,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
                     labels = c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999",
                                "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999",
                                "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999",
                                "$100,000 - $119,999", "$120,000 - $149,999", "$150,000 - $199,999",
                                "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999",
                                "$500,000 or more", "Prefer not to say"))
data_R$Q38 <- factor(data_R$Q38, levels = c(1, 2, 3, 4, 5, 6), labels = c("Much Worse off from AI", "Worse off from AI", "Neutral from AI", "Better off from AI", "Much Better off from AI", "Unsure About AI"))

data_R$Q60 <- relevel(data_R$Q60, ref = "Not Hispanic/Latino")
data_R$Q38 <- relevel(data_R$Q38, ref = "Neutral from AI")
data_R$Q61 <- relevel(data_R$Q61, ref = "Independent")

# to ensure 1 is the true, 2 is the false
data_L <- data.frame(treatment  = covid_questions$dQ48,
                     outcome = ifelse(covid_questions$Q48 == 1, 2, ifelse(covid_questions$Q48 == 2, 1, covid_questions$Q48)) - 1 ,
                     true_labeled = covid_questions$dQ48Imgr2 - 3,
                     false_labeled = covid_questions$dQ48Imgr1 - 1)

data_L$Q1 <- controls$Q1
data_L$Q38 <- controls$Q38
data_L$Q41r6 <- controls$Q41r6
data_L$Q41r7 <- controls$Q41r7
data_L$Q42r1 <- controls$Q42r1
data_L$Q42r2 <- controls$Q42r2
data_L$Q49 <- controls$Q49
data_L$Q57 <- controls$Q57
data_L$Q59 <- controls$Q59
data_L$Q60 <- controls$Q60
data_L$Q61 <- controls$Q61
data_L$Q65 <- controls$Q65
data_L$D103 <- controls$D103
data_L$time <- controls$QTimeStampQ48
data_L$know <- covid_questions$Q46-1

data_L$outcome <- as.numeric(as.character(data_L$outcome)) # Outcome should be numeric

data_L$Q1 <- factor(data_L$Q1, levels = c(1, 2, 3), labels = c("Male", "Female", "Other Sex"))
data_L$Q59 <- factor(data_L$Q59, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("White", "Black", "Asian", "AIAN", "MENA", "NHPI", "Other", "NA"))
data_L$Q60 <- factor(data_L$Q60, levels = c(1, 2, 3), labels = c("Hispanic/Latino", "Not Hispanic/Latino", "NR Hispanic/Latino"))
data_L$Q61 <- factor(data_L$Q61, levels = c(1, 2, 3, 4, 5), labels = c("Democratic", "Republican", "Independent", "Other", "Not Sure"))
data_L$Q65 <- factor(data_L$Q65,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
                     labels = c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999",
                                "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999",
                                "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999",
                                "$100,000 - $119,999", "$120,000 - $149,999", "$150,000 - $199,999",
                                "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999",
                                "$500,000 or more", "Prefer not to say"))
data_L$Q38 <- factor(data_L$Q38, levels = c(1, 2, 3, 4, 5, 6), labels = c("Much Worse off from AI", "Worse off from AI", "Neutral from AI", "Better off from AI", "Much Better off from AI", "Unsure About AI"))

data_L$Q60 <- relevel(data_L$Q60, ref = "Not Hispanic/Latino")
data_L$Q38 <- relevel(data_L$Q38, ref = "Neutral from AI")
data_L$Q61 <- relevel(data_L$Q61, ref = "Independent")


strength <- rbind(data_R, data_L)




#strength_controls <- rbind(controls, controls)


print("analyse_data(data_R)")
analyse_data(data_R)
print("analyse_data_mycontrols(data_R, controls$Interest47, controls$Q45)")
analyse_data_mycontrols(data_R)
print("analyse_data_with_controls_basic(data_R, controls)")
analyse_data_with_controls_basic(data_R)
print("analyse_data_with_allcontrols(data_R, controls, controls$Interest47, controls$Q45)")
analyse_data_with_allcontrols(data_R)

print("analyse_data(data_L)")
analyse_data(data_L)
print("analyse_data_mycontrols(data_L, controls$Interest48, controls$Q46)")
analyse_data_mycontrols(data_L)
print("analyse_data_with_controls_basic(data_L, controls)")
analyse_data_with_controls_basic(data_L)
print("analyse_data_with_allcontrols(data_L, controls, controls$Interest48, controls$Q46)")
analyse_data_with_allcontrols(data_L)

print("analyse_data(strength)")
analyse_data(strength)
print("analyse_data_mycontrols(strength,  c(controls$Interest47, controls$Interest48), c(controls$Q45, controls$Q46))")
analyse_data_mycontrols(strength)
print("analyse_data_with_controls_basic(strength, strength_controls)")
analyse_data_with_controls_basic(strength)
print("analyse_data_with_allcontrols(strength, strength_controls, c(controls$Interest47, controls$Interest48), c(controls$Q45, controls$Q46))")
analyse_data_with_allcontrols(strength)
sink()







