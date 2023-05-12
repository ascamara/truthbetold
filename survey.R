library(haven)
library(dplyr)
library(ggplot2)

get_results <- function(data_selected){
  covid_questions <- select(data_selected, "Q45","Q46", #Knowledge checks: 1 if true, 2 if false
                            "hQ47r1","hQ47r2","hQ47r3","hQ47r4", #eligibility for randomization on Q47, should all be 1
                            "hQ48r1","hQ48r2","hQ48r3","hQ48r4", #eligibility for randomization on Q48, should all be 1
                            "dQ47","dQ48", #treatment assignment for Q47, Q48
                            "hQ47Imgr1","hQ47Imgr2","hQ47Imgr3","hQ47Imgr4", #whether the respondent saw the img, condl on treatment assgm for Q47
                            "hQ48Imgr1","hQ48Imgr2", "hQ48Imgr3","hQ48Imgr4", #whether the respondent saw the img, condl on treatment assgm for Q48
                            "dQ47Imgr1","dQ47Imgr2", #id of img in each spot, condl on treatment assgm for Q47
                            "dQ48Imgr1","dQ48Imgr2", #id of img in each spot, condl on treatment assgm for Q48
                            "Q47","Q48", "Q57") #whether the respondent clicked on the first or second image
  
  # raw analysis
  treatment_47 <- covid_questions$dQ47
  outcome_47 <- covid_questions$Q47-1
  true_labeled_47 <- covid_questions$dQ47Imgr1 - 1
  false_labeled_47 <- covid_questions$dQ47Imgr2 - 3
  
  treatment_48 <- covid_questions$dQ48
  # to ensure 1 is the true, 2 is the false
  outcome_48 <- ifelse(covid_questions$Q48 == 1, 2, ifelse(covid_questions$Q48 == 2, 1, covid_questions$Q48)) - 1 
  true_labeled_48 <- covid_questions$dQ48Imgr2 - 3
  false_labeled_48 <- covid_questions$dQ48Imgr1 - 1
  
  is_young <- ifelse(covid_questions$Q57 < 35, 1, 0)

  ### for q47
  one.way.47 <- aov(outcome_47 ~ true_labeled_47 * false_labeled_47 + is_young)
  summary(one.way.47)
  
  fit.47 <- glm(outcome_47 ~ true_labeled_47 * false_labeled_47 + is_young, family = binomial(link = "logit"))
  summary(fit.47)
  
  ### plots for q47
  grid <- expand.grid(true_labeled_47 = c(0,1), false_labeled_47 = c(0,1), is_young=c(0,1))
  grid$response <- predict(fit.47, newdata = grid, type = "response")
  
  me_true_lable_right <- ggplot(grid, aes(x = true_labeled_47, y = response)) +
    geom_line(data = subset(grid, false_labeled_47 == 1), aes(color = "labeled"), linewidth = 1) +
    geom_line(data = subset(grid, false_labeled_47 == 0), aes(color = "unlabeled"), linewidth = 1) +
    labs(x = "True News Labeled", y = "Pr(Fake News Selected)", color = "Fake News") +
    scale_color_manual(values = c("red", "blue"), labels = c("Labeled", "Unlabeled")) + 
    ggtitle("Main Effects of Labeling True News as Disputed on a Rightist Post")
  
  me_false_lable_right <- ggplot(grid, aes(x = false_labeled_47, y = response)) +
    geom_line(data = subset(grid, true_labeled_47 == 1), aes(color = "labeled"), linewidth = 1) +
    geom_line(data = subset(grid, true_labeled_47 == 0), aes(color = "unlabeled"), linewidth = 1) +
    labs(x = "Fake News Labeled", y = "Pr(Fake News Selected)", color = "True News") +
    scale_color_manual(values = c("red", "blue"), labels = c("Labeled", "Unlabeled")) + 
    ggtitle("Main Effects of Labeling Fake News as Disputed on a Rightist Post")
  
  ### for q48
  one.way.48 <- aov(outcome_48 ~ true_labeled_48 * false_labeled_48 + is_young)
  summary(one.way.48)
  
  fit.48 <- glm(outcome_48 ~ true_labeled_48 * false_labeled_48 + is_young, family = binomial(link = "logit"))
  summary(fit.48)
  
  
  grid <- expand.grid(true_labeled_48 = c(0,1), false_labeled_48 = c(0,1), is_young=c(0,1))
  grid$response <- predict(fit.48, newdata = grid, type = "response")
  
  me_true_lable_left <- ggplot(grid, aes(x = true_labeled_48, y = response)) +
    geom_line(data = subset(grid, false_labeled_48 == 1), aes(color = "labeled"), linewidth = 1) +
    geom_line(data = subset(grid, false_labeled_48 == 0), aes(color = "unlabeled"), linewidth = 1) +
    labs(x = "True News Labeled", y = "Pr(Fake News Selected)", color = "Fake News") +
    scale_color_manual(values = c("red", "blue"), labels = c("Labeled", "Unlabeled")) + 
    ggtitle("Main Effects of Labeling True News as Disputed on a Leftist Post")
  
  me_false_lable_left <- ggplot(grid, aes(x = false_labeled_48, y = response)) +
    geom_line(data = subset(grid, true_labeled_48 == 1), aes(color = "labeled"), linewidth = 1) +
    geom_line(data = subset(grid, true_labeled_48 == 0), aes(color = "unlabeled"), linewidth = 1) +
    labs(x = "Fake News Labeled", y = "Pr(Fake News Selected)", color = "True News") +
    scale_color_manual(values = c("red", "blue"), labels = c("Labeled", "Unlabeled")) + 
    ggtitle("Main Effects of Labeling Fake News as Disputed on a Leftist Post")
  
  print("ANOVA Rightist")
  print(summary(one.way.47))
  print("GLM Rightist")
  print(summary(fit.47))
  
  print(me_true_lable_right)
  print(me_false_lable_right)
  
  print("ANOVA Leftist")
  print(summary(one.way.48))
  print("GLM Leftist")
  print(summary(fit.48))
  
  print(me_true_lable_left)
  print(me_false_lable_left)
}

get_results(data)

#those who successfully completed the knowledge checks
data_knowledge <- data[data$Q45 == 2 & data$Q46 == 2,]
get_results(data_knowledge)

data_no_knowledge <- data[data$Q45 != 2 | data$Q46 != 2,]
get_results(data_no_knowledge)

#consider only the first knowledge check
data_knowledge <- data[data$Q45 == 2,]
get_results(data_knowledge)

data_no_knowledge <- data[data$Q45 != 2,]
get_results(data_no_knowledge)

#weird...
data_knowledge <- data[data$Q46 == 2,]
get_results(data_knowledge)

data_no_knowledge <- data[data$Q46 != 2,]
get_results(data_no_knowledge)

###trying a few specifications
data_D <- data[data$Q61 == 1,]
get_results(data_D)

data_R <- data[data$Q61 == 2,]
get_results(data_R)

data_I <- data[data$Q61 == 3,]
get_results(data_I)

###final
data_social_media <- data[data$Q41r6 >= 3,]
get_results(data_social_media)

data_social_media <- data[data$Q41r6 == 4,]
get_results(data_social_media)

data_social_media <- data[data$Q41r6 <= 2,]
get_results(data_social_media)

##yee haw
data_vaxxed <- data[data$Q42r2 != 3,]
ru_notvaxxed <- data_vaxxed$Q42r2 - 1
outcome1 <- data_vaxxed$Q45 - 1
outcome2 <- data_vaxxed$Q46 - 1

model1 <- lm(outcome1 ~ ru_notvaxxed,family = binomial(link = "logit"))
summary(model1)


model2 <- lm(outcome2 ~ ru_notvaxxed,family = binomial(link = "logit"))
summary(model2)