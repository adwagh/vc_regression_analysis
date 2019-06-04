### libraries ###
library(readxl)
library(pander)

install.packages("openxlsx")
library("openxlsx")

install.packages("xlsx", dependencies = TRUE)

##### ab test issuance -> activation user conversion ####
AB_Test_Cohorts <- read_xls("Desktop/VC analyses/Output/AB Test Cohorts.xls")
View(AB_Test_Cohorts)
result = t.test(AB_Test_Cohorts$conv_flag ~ AB_Test_Cohorts$design_key)
t.test(AB_Test_Cohorts$conv_flag ~ AB_Test_Cohorts$design_key)
pander(result)

##### ab test issuance -> transaction user activation ####
AB_test_issue_txn <- read_excel("Desktop/VC analyses/Output/AB test issue - txn.xls")
result4 = t.test(AB_test_issue_txn$conv_flag ~ AB_test_issue_txn$design_key)
t.test(AB_test_issue_txn$conv_flag ~ AB_test_issue_txn$design_key)
pander(result4)

##### ab test activation -> transaction user activation ####
AB_test_act_txn <- read_excel("Desktop/VC analyses/Output/AB test act - txn.xls")
result5 = t.test(AB_test_act_txn$conv_flag ~ AB_test_act_txn$design_key)
t.test(AB_test_act_txn$conv_flag ~ AB_test_act_txn$design_key)
pander(result5)

##### ab test activation -> transaction user activation [incl people who did not activate]####
AB_test_act_txn_0 <- read_excel("Desktop/VC analyses/Output/AB test act - txn (incl 0).xls")
result8 = t.test(AB_test_act_txn_0$conv_flag ~ AB_test_act_txn_0$design_key)
t.test(AB_test_act_txn_0$conv_flag ~ AB_test_act_txn_0$design_key)
pander(result8)


##### ab test activation -> transaction user conversion ####
##AB_test_user_activation_to_transaction <- read_excel("Desktop/VC analyses/Output/AB test user activation to transaction.xls")
#result4 = t.test(AB_test_user_activation_to_transaction$user_conv ~ AB_test_user_activation_to_transaction$design_key)
#t.test(AB_test_user_activation_to_transaction$user_conv ~ AB_test_user_activation_to_transaction$design_key)
#pander(result4)

##### ab test transaction count AFT ISSUANCE####
AB_test_transactions <- read_excel("Desktop/VC analyses/Output/AB test num txn.xls")
View(AB_test_transactions)
result3 = t.test(AB_test_transactions$num_transact ~ AB_test_transactions$design_key)
t.test(AB_test_transactions$num_transact ~ AB_test_transactions$design_key)
pander(result3)

result3 = t.test(XXX$num_transact ~ XXX$design_key)
t.test(XXX$num_transact ~ XXX$design_key)
pander(result3)

##### ab test transaction amount AFT ISSUANCE####
c
View(AB_test_paym)
result8 = t.test(AB_test_paym$amount ~ AB_test_paym$design_key)
t.test(AB_test_paym$amount ~ AB_test_paym$design_key)
pander(result8)

##### ab test transaction count AFT ISSUANCE (incl 0)####
AB_test_transactions_incl0 <- read_excel("Desktop/VC analyses/Output/AB test num txn (incl 0).xls")
View(AB_test_transactions_incl0)
result14 = t.test(AB_test_transactions_incl0$num_transact ~ AB_test_transactions_incl0$design_key)
t.test(AB_test_transactions_incl0$num_transact ~ AB_test_transactions_incl0$design_key)
pander(result14)

##### ab test transaction amount AFT ISSUANCE (incl 0)####
AB_test_paym_incl0 <- read_excel("Desktop/VC analyses/Output/AB Test TPV (incl 0).xls")
View(AB_test_paym_incl0)
result15 = t.test(AB_test_paym_incl0$amount ~ AB_test_paym_incl0$design_key)
t.test(AB_test_paym_incl0$amount ~ AB_test_paym_incl0$design_key)
pander(result15)

##### ab test transaction count AFT ACTIVATION####
AB_test_transactions_a <- read_excel("Desktop/VC analyses/Output/AB test num txn aft activation.xls")
View(AB_test_transactions_a)
result6 = t.test(AB_test_transactions_a$num_transact ~ AB_test_transactions_a$design_key)
t.test(AB_test_transactions_a$num_transact ~ AB_test_transactions_a$design_key)
pander(result6)

##### ab test transaction amount AFT ACTIVATION####
AB_test_paym_a <- read_excel("Desktop/VC analyses/Output/AB Test TPV aft activation.xls")
View(AB_test_paym_a)
result7 = t.test(AB_test_paym_a$amount ~ AB_test_paym_a$design_key)
t.test(AB_test_paym_a$amount ~ AB_test_paym_a$design_key)
pander(result7)

##### ab test transaction count AFT ACTIVATION INCL 0####
AB_test_transactions_a_incl0 <- read_excel("Desktop/VC analyses/Output/AB test num txn aft activation (incl 0).xls")
View(AB_test_transactions_a_incl0)
result11 = t.test(AB_test_transactions_a_incl0$num_transact ~ AB_test_transactions_a_incl0$design_key)
t.test(AB_test_transactions_a_incl0$num_transact ~ AB_test_transactions_a_incl0$design_key)
pander(result11)

##### ab test transaction amount AFT ACTIVATION INCL 0####
AB_test_paym_a_incl0 <- read_excel("Desktop/VC analyses/Output/AB test TPV aft activation (incl 0).xls")
View(AB_test_paym_a_incl0)
result12 = t.test(AB_test_paym_a_incl0$amount ~ AB_test_paym_a_incl0$design_key)
t.test(AB_test_paym_a_incl0$amount ~ AB_test_paym_a_incl0$design_key)
pander(result12)


##### SEGMENTED REGRESSION ANALYSIS ###########
install.packages("stargazer")
library(stargazer)
install.packages("plyr")
library(plyr)
require(aod)
require(ggplot2)

reg_data <- read_excel("Desktop/VC analyses/Output/Segmented regression.xls")


myprobit <- glm(conv_flag ~ cohort, family = binomial(link = "probit"), 
                data = reg_data)

## model summary
summary(myprobit)
confint(myprobit)

##### REGRESSION ANALYSIS ON USER ATTRIBUTES W cohorts ###########
# with bank acc as var for all user cohorts#
user_reg_data <- read_excel("~/Desktop/Regression analysis (cohorts) 2018-01-16.xlsx")
View(user_reg_data)
###user_reg_data$acc_age <- factor(user_reg_data$acc_age)
reg_model <- glm(conv_flag ~ num_paym + se + edu_email + acc_age + pwv_use + paym_partners_life + bank_acc,
                 family = binomial(link = "logit"),
                 data=user_reg_data)
summary(reg_model)

exp(coef(reg_model))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(reg_model))

allmean <- data.frame(se=mean(user_reg_data$se),
                      edu_email=1,
                      acc_age=mean(user_reg_data$acc_age),
                      num_paym=mean(user_reg_data$num_paym),
                      paym_partners_life=mean(user_reg_data$paym_partners_life),
                      pwv_use=mean(user_reg_data$pwv_use),
                      bank_acc=mean(user_reg_data$bank_acc))

vector_paym <- c(1:81)
vector_num_paym_part <- c(1:307)

allmean2 <- data.frame(se=mean(user_reg_data$se),
                      edu_email=mean(user_reg_data$edu_email),
                      acc_age=mean(user_reg_data$acc_age),
                      num_paym=vector_paym,
                      paym_partners_life=mean(user_reg_data$paym_partners_life),
                      pwv_use=mean(user_reg_data$pwv_use),
                      bank_acc=mean(user_reg_data$bank_acc))


predict_paym <- predict(reg_model, newdata=allmean, type = 'response')
View(predict_paym)

allmean3 <- data.frame(se=mean(user_reg_data$se),
                       edu_email=mean(user_reg_data$edu_email),
                       acc_age=mean(user_reg_data$acc_age),
                       num_paym=mean(user_reg_data$num_paym),
                       paym_partners_life=vector_num_paym_part,
                       pwv_use=mean(user_reg_data$pwv_use),
                       bank_acc=mean(user_reg_data$bank_acc))


predict_paym_part <- predict(reg_model, newdata=allmean3, type = 'response')
View(predict_paym_part)

##library(ggplot2)
#ggplot(user_reg_data, aes(x=num_paym, y=conv_flag, color=factor(num_paym), group=num_paym)) + geom_point() + 
#  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

# without bank acc  as a regression var#
user_reg_data_bank <- subset(user_reg_data, cohort == 'w_1_A' | cohort == 'w_1_b' | cohort == 'w_1_c' | cohort == 'w_2_a' | cohort == 'w_2_b' | cohort == 'w_2_c')
View(user_reg_data_bank)
reg_model <- glm(conv_flag ~ num_paym + se + edu_email + acc_age + pwv_use + paym_partners_life + cc_funding_source,
                 family = binomial(link = "logit"),
                 data=user_reg_data)
summary(reg_model)
exp(coef(reg_model))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(reg_model))

# predicted prob of conversion for our sample #
allmean <- data.frame(se=mean(user_reg_data_bank$se),
                      edu_email=mean(user_reg_data_bank$edu_email),
                      acc_age=mean(user_reg_data_bank$acc_age),
                      num_paym=mean(user_reg_data_bank$num_paym),
                      paym_partners_life=mean(as.numeric(user_reg_data_bank$paym_partners_life)),
                      pwv_use=mean(user_reg_data_bank$pwv))

View(allmean)

predict(reg_model, newdata=allmean, type = 'response')

allmean <- data.frame(se=mean(user_reg_data_bank$se),
                      edu_email=mean(user_reg_data_bank$edu_email),
                      acc_age=mean(user_reg_data_bank$acc_age),
                      num_paym=2,
                      paym_partners_life=mean(as.numeric(user_reg_data_bank$paym_partners_life)),
                      pwv_use=mean(user_reg_data_bank$pwv_use))

View(allmean)

predict(reg_model, newdata=allmean, type = 'response')

allmean <- data.frame(se=mean(user_reg_data_bank$se),
                      edu_email=mean(user_reg_data_bank$edu_email),
                      acc_age=mean(user_reg_data_bank$acc_age),
                      num_paym=3,
                      paym_partners_life=mean(as.numeric(user_reg_data_bank$paym_partners_life)),
                      pwv_use=mean(user_reg_data_bank$pwv_use))

View(allmean)

predict(reg_model, newdata=allmean, type = 'response')

# issuance conv flag # 
# with bank acc as var for all user cohorts#
reg_model <- glm(conv_flag_iss ~ num_paym + bank_acc + se + edu_email + acc_age + pwv_use + paym_partners_life,
                 family = binomial(link = "logit"),
                 data=user_reg_data)
summary(reg_model)
exp(coef(reg_model))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(reg_model))

# issuance conv flag # 
# without bank acc as var for users with bank cohorts only#
user_reg_data_bank <- subset(user_reg_data, cohort == 'w_1_A' | cohort == 'w_1_b' | cohort == 'w_1_c' | cohort == 'w_2_a' | cohort == 'w_2_b' | cohort == 'w_2_c')
View(user_reg_data_bank)
reg_model <- glm(conv_flag ~ num_paym + se + edu_email + acc_age + pwv_use + paym_partners_life,
                 family = binomial(link = "logit"),
                 data=user_reg_data)
summary(reg_model)
exp(coef(reg_model))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(reg_model))

# accuracy of regression #
install.packages("caret")
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(user_reg_data$conv_flag, p = .67,
                                  list = FALSE,
                                  times = 1)

Train <- user_reg_data[ trainIndex,]
Test  <- user_reg_data[-trainIndex,]

# Logistic Regression Model
reg_model <- glm(conv_flag ~ num_paym + se + edu_email + acc_age + pwv_use + paym_partners_life,
                 family = binomial(link = "logit"),
                 data=Train)

Test$model_prob <- predict(reg_model, Test, type = "response")

fitted.results <- ifelse(Test$model_prob > 0.5,1,0)

misClasificError <- mean(fitted.results != Test$conv_flag)
print(paste('Accuracy',1-misClasificError))

# 98% accuracy #
