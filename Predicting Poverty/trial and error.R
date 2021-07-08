### Loading in Data ###

# This is the general household survey data - 2018 - STATS SA

##################################################
############### INTRO ############################
##################################################

library(tidyverse)
library(caTools)
library(dplyr)
library(gridExtra)
library(kableExtra)
library(rpart.plot)
library(tidyverse)
library(caret)
library(grid)
library(lares)
library(e1071)

# for project, put in an install option for Dawie for lesser known packages like lares or make a note so that he checks that everything is installed.


house <- read.csv("C:/Users/jesic/OneDrive/Desktop/ghs-2018-house-1.0-csv.csv")
house.simple <-  select(house, head_popgrp, head_sex, head_age, Q55Bedr, Q55TotRm, Q57Rent, Q58Val, Q63NrCell, Q814Exp, Q89aGrant, Q821WashM, hholdsz, chld17yr_hh, Q42Msal_hh, totmhinc, econact_hh)

#add column. We are making the pp person income using the total monthly income and minus the child17 years an
house.simple$adults <- house.simple$hholdsz - house.simple$chld17yr_hh
house.simple$income_pp <- house.simple$totmhinc / house.simple$adults

house.simple <- filter(house.simple, Q821WashM <9,Q55Bedr < 99, Q55TotRm < 25, Q814Exp < 11, hholdsz < 15, Q58Val < 9, Q63NrCell < 11, income_pp < 50000, Q821WashM <9)

## Add column
# We will define a household under these
# 1 - extreme poverty - Food poverty line ( <=547)
# 2 - moderate poverty - lower poverty line (<=785)
# 3 - vulnerable - upper bound poverty line (<= 1183)
# 4 - non-vulnerable - the rest of the people (rest)

house.simple$poverty_level <- ifelse(house.simple$income_pp <= 547, 1, ifelse(house.simple$income_pp > 547 & house.simple$income_pp <=785,2,ifelse(house.simple$income_pp > 785 & house.simple$income_pp <=1183,3,4)))

## Need to split data into testing and training data
# here randomly selects 70 % of the rows from the dataset
# 70% is split into the training one, where 30% of same is in the test dataset

##SET SEED HERE
set.seed(555)
dt <- sort(sample(nrow(house.simple), nrow(house.simple)*.7))
train <- house.simple[dt,]
test <- house.simple[-dt,]














#################################################
################## FIGURE 3.2 #################
###############################################

plot1 <- train %>%
    ggplot(aes(as.numeric(poverty_level))) +
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count", show.legend = TRUE) +
    geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count", vjust = -.5, size = 3) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Poverty levels ", subtitle= " Number of household below a given poverty level", caption = "Own calculations using training dataset") +
    ggthemes::theme_economist_white()+
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank()) +
    scale_fill_viridis_d(name="Poverty levels:",
                         breaks=c("1", "2", "3", "4"),
                         labels=c("1 = Food poverty line", "2 = Lower bound poverty line", "3 = Upper bound poverty line", "4 = non-poor"))

plot1



######################

no_dummy_index  <- apply(house, 2, function(x) { !all(x %in% 0:1) })
no_dummy <- house[ , no_dummy_index]
names(no_dummy)
#################################################


#########################################################
############# FIGURE 3.3 ################################
#########################################################
plot_target_by_var <- function(df, variable){
    var <- enquo(variable)
    df %>%
        ggplot(aes(!!var, fill = as.factor(poverty_level), color = as.factor(poverty_level))) +
        geom_density(alpha = 0.1, size = 1, show.legend = FALSE) +
        scale_colour_viridis_d() +
        theme_minimal()
}


p_age <- plot_target_by_var(train, head_age)
p_size <- plot_target_by_var(train, hholdsz)
p_hhexp <- plot_target_by_var(train, Q814Exp)
p_cellphone <- plot_target_by_var(train, Q63NrCell)
p_room <- plot_target_by_var(train, Q55TotRm)
p_val <- plot_target_by_var(train, Q58Val)
#display plots in a grid
grid.arrange(p_age, p_size, p_hhexp, p_cellphone, p_room , p_val, nrow = 2, top = textGrob("Distribution by poverty levels", gp=gpar(fontsize=15)))

# purple - food poverty line
# yellow = not vulnerable



###########################################################
############## FIGURE 3.4 ##################################
###########################################################
#getting rid of the variables that represent the same variable just as monthly household income. Have three variables that basically say the same thing. I want to get rid of them so that we have better information.
a <- train[,-18] # get rid of income_pp
b <- a[,-14] # get rid of monthly salary - check.
corr_var(b, poverty_level, top=10)



#################################################################
# Our poverty_level needs to be a factor.
# this is to ensure that our machine learning models treat this problem as a classification task

train$poverty_level <- as.factor(train$poverty_level)
test$poverty_level <- as.factor(test$poverty_level)


##################################################
# Different classification models using caret packages
# have an imbalanced dataset
# use a default parameter we will try to optimize during training wull be Kappa in order to boost our performance.
# This can be done by adding the metric=Kappa argument to our train call

# For performance and speed improvements, we will use a 10 K-fold cross validation to fit our models
# This can be done using trControl function in caret




# define models to try
models <- c("multinom", "lda", "naive_bayes", "svmLinear", "knn", "rpart", "ranger")
# set CV control for knn, k-folds
control <- trainControl(method = "cv", number = 10, p = .9) # 10 fold, 10%
# fit models
set.seed(1)

train_models <- lapply(models, function(model){
    print(model)
    train(poverty_level ~ ., method = model, data = train, trControl = control, metric = "Kappa")
})
names(train_models) <- models


# extract elapsed training times
elapsed <- sapply(train_models, function(object)
    object$times$everything["elapsed"])
# extract accuracy from CM in one step without creating a separate predictions vector
acc = sapply(train_models, function(x){
    pred = predict(x, newdata = test)
    cm = confusionMatrix(pred, reference = test$poverty_level)
    return(cm[["overall"]]["Accuracy"])
}
)
# extract F1 by class
F1 = sapply(train_models, function(x){
    pred = predict(x, newdata = test)
    cm = confusionMatrix(pred, reference = test$poverty_level)
    return(cm[["byClass"]][ , "F1"])
}
)
# extract macro F1
F1_M = sapply(train_models, function(x){
    pred = predict(x, newdata = test)
    cm = confusionMatrix(pred, reference = test$poverty_level)
    return(mean(cm[["byClass"]][ , "F1"], na.rm = TRUE))
}
)
# extract weighted F1
F1_W <- sapply(train_models, function(x){
    pred = predict(x, newdata = test)
    cm = confusionMatrix(pred, reference = test$poverty_level)
    actual = colSums(cm$table)
    F1 = cm[["byClass"]][ , "F1"]
    return((sum(actual*F1, na.rm = TRUE))/(sum(actual)))
}
)


## PUT ALL OF THE ABOVE IN A NICE TABLE  in LATEX
## Also put in dataframe so that you can display nice graphs.

df <- data.frame(models, elapsed, acc, F1_M, F1_W)

#speed vs accuracy
plot2 <- ggplot(data=df) +
    geom_point(mapping = aes(x= elapsed, y = acc, color = models, size = 10)) +
    geom_text(aes(x= elapsed, y = acc, color = models, label = models), hjust =0.7, vjust= -0.8, nudge_x = 0.1) +
    labs(y = "Accuracy", x = "Speed", title = "Compaing Speed and Accuracy of Different Models", subtitle = "using training model", caption = "Own calculations") +
    ggthemes::theme_economist_white() +
    theme(legend.position = "none")
plot2


#speed vs macroF1
plot3 <- ggplot(data=df) +
    geom_point(mapping = aes(x= elapsed, y = F1_M, color = models, size = 10)) +
    geom_text(aes(x= elapsed, y = F1_M, color = models, label = models), hjust =0.7, vjust= -0.8, nudge_x = 0.1) +
    labs(y = "Macro F1 Score", x = "Speed", title = "Compaing Speed and the Macro-F1 Score of Different Models", subtitle = "using training model", caption = "Own calculations") +
    ggthemes::theme_economist_white() +
    theme(legend.position = "none")
plot3

#speed and macro weighted
plot4 <- ggplot(data=df) +
    geom_point(mapping = aes(x= elapsed, y = F1_W, color = models, size = 10)) +
    geom_text(aes(x= elapsed, y = F1_W, color = models, label = models), hjust =0.7, vjust= -0.8, nudge_x = 0.1) +
    labs(y = "Weighted F1 Score", x = "Speed", title = "Compaing Speed and the Weighted-F1 Score of Different Models", subtitle = "using training model", caption = "Own calculations") +
    ggthemes::theme_economist_white() +
    theme(legend.position = "none")
plot4


### evaluation of Micro scores

F1 %>% kable(col.names = models) %>% kable_styling()


# f2 and f3 scores

F2 <- sapply(train_models, function(x){
    pred = predict(x, newdata = test)
    cm = confusionMatrix(pred, reference = test$poverty_level)
    precision = cm[["byClass"]][ , "Precision"]
    recall = cm[["byClass"]][ , "Recall"]
    return((5*precision*recall) / (4*precision + recall))
}
)
# extract F-Beta by class
F3 <- sapply(train_models, function(x){
    pred = predict(x, newdata = test)
    cm = confusionMatrix(pred, reference = test$poverty_level)
    precision = cm[["byClass"]][ , "Precision"]
    recall = cm[["byClass"]][ , "Recall"]
    return((10*precision*recall) / (9*precision + recall))
}
)

# Class 1: Food poverty

class1 <- rbind(F1[1, ], F2[1, ], F3[1, ])
row.names(class1) <-  c("F1", "F2", "F3")
class1 %>% kable(col.names = models, caption = "F-Beta Scores for the Food Poverty line") %>% kable_styling()

# Class 2: lower bound poverty line
class2 <- rbind(F1[2, ], F2[2, ], F3[2, ])
row.names(class2) <-  c("F1", "F2", "F3")
class2 %>% kable(col.names = models, caption = "F-Beta Scores for the Lower Bound Poverty line") %>% kable_styling()

#Class 3: Upper bound poverty line
class3 <- rbind(F1[3, ], F2[3, ], F3[3, ])
row.names(class3) <-  c("F1", "F2", "F3")
class3 %>% kable(col.names = models, caption = "F-Beta Scores for the Upper Bound Poverty line") %>% kable_styling()

# Class4: non-vulnerable
class4 <- rbind(F1[4, ], F2[4, ], F3[4, ])
row.names(class4) <-  c("F1", "F2", "F3")
class4 %>% kable(col.names = models, caption = "F-Beta Scores for Non-Vulnerable Households") %>% kable_styling()


## DO THIS FOR YOUR BEST PERFORMING MODELS. WE CAN CHECK THIS WITH OUR DECISION TREE!!!

get_imp <- function(modelname){
    imp <- data.frame(varImp(modelname)$importance)
    imp$Variable <- rownames(imp)
    imp <- imp[order(-imp$Overall)[1:5], ] %>% select(Variable, Overall)
    rownames(imp) <- 1:5
    return(imp)
}
imp_multinom <- get_imp(train_models$multinom)
imp_rpart <- get_imp(train_models$rpart)
get_imp <- function(modelname){
    imp <- data.frame(varImp(modelname)$importance)
    imp$Variable <- rownames(imp)
    imp <- imp[order(-imp$Overall)[1:10], ] %>% select(Variable, Overall)
    rownames(imp) <- 1:10
    return(imp)
}
cbind(imp_rpart, imp_multinom) %>% kable(caption = "Most important variables, `rpart` vs. `multinom`") %>% kable_styling()


#TREES - notes from DAWIE - DONT KNOW WHAT I AM DOING
##SIMPLE DECISION TREE
#Doesn't say anything impressive. - Could be nice in descriptive stats to say how I split it.

#library(rpart)
#library(rpart.plot)

#More for descriptive
#fit <- rpart(poverty_level~., data = train, method = 'class')
#rpart.plot(fit, extra = 106)

vip(fit,bar=FALSE, aesthetics = list(fill="mediumvioletred", col="black")) + ggthemes::theme_economist_white() + labs(title = "Variable Importance Plot", subtitle = "all variables included", caption = "Own calculations")
#Shows that three income variables are the most important. I dont like this.

# taking income_pp out
fit1 <- rpart(poverty_level~.-income_pp, data = train, method = 'class')
rpart.plot(fit1, extra = 106)


# Taking all income variables out and adult variable.
fit2 <- rpart(poverty_level~., data = train[,-c(13,14,16,17)], method = 'class')
rpart.plot(fit2, extra = 106)

plotcp(fit2)

library(vip)

#Do either one or two here.
vip(fit2,bar=FALSE, aesthetics = list(fill="lightslateblue", col="black")) + ggthemes::theme_economist_white() + labs(title = "Variable Importance Plot", subtitle = "removed all income variables", caption = "Own calculations")

## RANDOM FOREST

library(randomForest)

model1 <- randomForest(poverty_level~ ., data = train, importance = TRUE)
model1

model2 <- randomForest(poverty_level ~ ., data = train, ntree = 500, mtry = 6, importance = TRUE)
model2

predTrain <- predict(model2, train, type = "class")
table(predTrain, train$poverty_level)

predValid <- predict(model2, test, type = "class")

mean(predValid == test$poverty_level)
table(predValid, test$poverty_level)

#############################################################

# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

set.seed(1234)
# Run the model
rf_default <- train(poverty_level~.,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)

### SEARCG FOR BEST MTRY (11)
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 17))
rf_mtry <- train(poverty_level~.,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)

best_mtry <- rf_mtry$bestTune$mtry

## SEARCH FOR BEST MAXNODES

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(1: 15)) {
    set.seed(1234)
    rf_maxnode <- train(poverty_level~.,
                        data = train,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        nodesize = 14,
                        maxnodes = maxnodes,
                        ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

########################################################################

rf <- randomForest(poverty_level~.,data=train[,-17], ntree=100, proximity=TRUE)
table(predict(rf),train$poverty_level)
plot(rf)
importance(rf)
varImpPlot(rf)

rfpred <- predict(rf, newdata=test[,-17])
table(rfpred,test$poverty_level)
plot(margin(rf,test$poverty_level))

cm <- table(rfpred, test$poverty_level)
accuracy <-(sum(diag(cm)))/sum(cm)

###
set.seed(222)
rf <- randomForest(poverty_level~.,data=train[,-17], proximity=TRUE)
p1 <- predict(rf, train)
confusionMatrix(p1, train$poverty_level)

p2 <- predict(rf, test)
confusionMatrix(p2, test$poverty_level)

plot(rf)

t <- tuneRF(train[,-17], train[,17],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

hist(treesize(rf), main = "No. of Nodes for the Trees", col = "mediumpurple4")
#Variable Importance
varImpPlot(rf, sort = T, n.var = 10,
           main = "Top 10 - Variable Importance")


