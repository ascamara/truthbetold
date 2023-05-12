library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)

# Redirect text output to a file
sink("output_1.txt")

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
controls$Q1 <- factor(controls$Q1, levels = c(1, 2, 3), labels = c("Male", "Female", "Something else"))
# controls$Q57 <- cut(controls$Q57, 
#                          breaks = c(0, 18, 35, 50, 65, Inf), 
#                          labels = c("Under 18", "18-34", "35-49", "50-64", "65 and over"), 
#                          include.lowest = TRUE)
controls$Q59 <- factor(controls$Q59, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("White", "Black", "Asian", "AIAN", "MENA", "NHPI", "Other", "NA"))
controls$Q60 <- factor(controls$Q60, levels = c(1, 2, 3), labels = c("HL", "!HL", "NA"))
controls$Q61 <- factor(controls$Q61, levels = c(1, 2, 3, 4, 5), labels = c("D", "R", "I", "O", "NS"))
controls$Q65 <- factor(controls$Q65,
                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
                       labels = c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999",
                                  "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999",
                                  "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999",
                                  "$100,000 - $119,999", "$120,000 - $149,999", "$150,000 - $199,999",
                                  "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999",
                                  "$500,000 or more", "Prefer not to say"))
controls$Interest47 <- controls$QTimeStampQ47
controls$Interest48 <- controls$QTimeStampQ48

controls$Q38 <- factor(controls$Q61, levels = c(1, 2, 3, 4, 5, 6), labels = c("WW", "W", "-", "B", "BB", "?"))
controls$Q45 <- covid_questions$Q45-1
controls$Q46 <- covid_questions$Q46-1

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
  
  # Aggregate data
  data_agg <- aggregate(outcome ~ true_labeled + false_labeled, df, mean)
  
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

analyse_data_mycontrols <- function(df, time, know) {
  # Fit a one-way ANOVA
  one.way <- aov(outcome ~ true_labeled * false_labeled + time + know, data = df)
  print(summary(one.way))
  
  # Fit a logistic regression model
  fit <- glm(outcome ~ true_labeled * false_labeled + time + know, family = binomial(link = "logit"), data = df)
  print(summary(fit))
  
  # Aggregate data
  data_agg <- aggregate(outcome ~ true_labeled + false_labeled, df, mean)
  
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

analyse_data_with_controls_basic <- function(df, controls) {
  # Fit a one-way ANOVA
  one.way <- aov(outcome ~ true_labeled * false_labeled + controls$Q1 + controls$Q57 + controls$Q59 + controls$Q60 + controls$Q61 + controls$Q65, data = df)
  print(summary(one.way))
  
  # Fit a logistic regression model
  fit <- glm(outcome ~ true_labeled * false_labeled + controls$Q1 + controls$Q57 + controls$Q59 + controls$Q60 + controls$Q61 + controls$Q65, family = binomial(link = "logit"), data = df)
  print(summary(fit))
  
  df$predicted <- predict(fit, type = "response")
  data_pred <- aggregate(predicted ~ true_labeled + false_labeled, df, mean)
  data_agg <- aggregate(outcome ~ true_labeled + false_labeled, df, mean)
  
  # Create and display the first plot
  p1 <- ggplot(data_agg, aes(x = true_labeled, y = outcome, color = false_labeled, group = false_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_line(data = data_pred, aes(y = predicted), linetype = "dashed") +
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
    geom_line(data = data_pred, aes(y = predicted), linetype = "dashed") +
    labs(x = "True News Labeled", y = "Pr(Fake News Selected)", color = "Fake News") +
    scale_color_manual(values = c("blue", "red"), labels = c("Unlabeled", "Labeled")) +
    theme_classic() +
    theme(legend.position = "top")
  ggsave(paste0("plot_", i, ".png"), p2)
  print(i)
  i<<-i+1
}

analyse_data_with_allcontrols <- function(df, controls, time, know) {
  # Fit a one-way ANOVA
  one.way <- aov(outcome ~ true_labeled * false_labeled + controls$Q1 + controls$Q57 + controls$Q59 + controls$Q60 + controls$Q61 + controls$Q65 + time + know, data = df)
  print(summary(one.way))
  
  # Fit a logistic regression model
  fit <- glm(outcome ~ true_labeled * false_labeled + controls$Q1 + controls$Q57 + controls$Q59 + controls$Q60 + controls$Q61 + controls$Q65 + time + know, family = binomial(link = "logit"), data = df)
  print(summary(fit))
  
  df$predicted <- predict(fit, type = "response")
  data_pred <- aggregate(predicted ~ true_labeled + false_labeled, df, mean)
  data_agg <- aggregate(outcome ~ true_labeled + false_labeled, df, mean)
  
  # Create and display the first plot
  p1 <- ggplot(data_agg, aes(x = true_labeled, y = outcome, color = false_labeled, group = false_labeled)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_line(data = data_pred, aes(y = predicted), linetype = "dashed") +
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
    geom_line(data = data_pred, aes(y = predicted), linetype = "dashed") +
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
data_R$true_labeled <- as.factor(data_R$true_labeled)
data_R$false_labeled <- as.factor(data_R$false_labeled)
data_R$outcome <- as.numeric(as.character(data_R$outcome)) # Outcome should be numeric


# to ensure 1 is the true, 2 is the false
data_L <- data.frame(treatment  = covid_questions$dQ48,
                     outcome = ifelse(covid_questions$Q48 == 1, 2, ifelse(covid_questions$Q48 == 2, 1, covid_questions$Q48)) - 1 ,
                     true_labeled = covid_questions$dQ48Imgr2 - 3,
                     false_labeled = covid_questions$dQ48Imgr1 - 1)

data_L$true_labeled <- as.factor(data_L$true_labeled)
data_L$false_labeled <- as.factor(data_L$false_labeled)
data_L$outcome <- as.numeric(as.character(data_L$outcome)) # Outcome should be numeric
strength <- rbind(data_R, data_L)

print("analyse_data(data_R)")
analyse_data(data_R)

print("analyse_data(data_L)")
analyse_data(data_L)

print("analyse_data(strength)")
analyse_data(strength)

print("analyse_data_mycontrols(data_R, controls$Interest47, controls$Q45)")
analyse_data_mycontrols(data_R, controls$Interest47, controls$Q45)

print("analyse_data_mycontrols(data_L, controls$Interest48, controls$Q46)")
analyse_data_mycontrols(data_L, controls$Interest48, controls$Q46)

print("analyse_data_mycontrols(strength,  c(controls$Interest47, controls$Interest48), c(controls$Q45, controls$Q46))")
analyse_data_mycontrols(strength,  c(controls$Interest47, controls$Interest48), c(controls$Q45, controls$Q46))

print("analyse_data_with_controls_basic(data_R, controls)")
analyse_data_with_controls_basic(data_R, controls)

print("analyse_data_with_controls_basic(data_L, controls)")
analyse_data_with_controls_basic(data_L, controls)

strength_controls <- rbind(controls, controls)

print("analyse_data_with_controls_basic(strength, strength_controls)")
analyse_data_with_controls_basic(strength, strength_controls)

print("analyse_data_with_allcontrols(data_R, controls, controls$Interest47, controls$Q45)")
analyse_data_with_allcontrols(data_R, controls, controls$Interest47, controls$Q45)

print("analyse_data_with_allcontrols(data_L, controls, controls$Interest48, controls$Q46)")
analyse_data_with_allcontrols(data_L, controls, controls$Interest48, controls$Q46)

print("analyse_data_with_allcontrols(strength, strength_controls, c(controls$Interest47, controls$Interest48), c(controls$Q45, controls$Q46))")
analyse_data_with_allcontrols(strength, strength_controls, c(controls$Interest47, controls$Interest48), c(controls$Q45, controls$Q46))


sink()







