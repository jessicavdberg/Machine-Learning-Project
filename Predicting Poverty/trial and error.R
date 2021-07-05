### Loading in Data ###

# This is the general household survey data - 2018 - STATS SA

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

# for project, put in an install option for Dawie for lesser known packages like lares or make a note so that he checks that everything is installed.


house <- read.csv("C:/Users/jesic/OneDrive/Desktop/ghs-2018-house-1.0-csv.csv")
house.simple <-  select(house, head_popgrp, head_sex, head_age, Q55Bedr, Q55TotRm, Q57Rent, Q58Val, Q63NrCell, Q814Exp, Q89aGrant, hholdsz, chld17yr_hh, Q42Msal_hh, totmhinc, econact_hh)

#add column. We are making the pp person income using the total monthly income and minus the child17 years an
house.simple$adults <- house.simple$hholdsz - house.simple$chld17yr_hh
house.simple$income_pp <- house.simple$totmhinc / house.simple$adults

house.simple <- filter(house.simple, Q55Bedr < 99, Q55TotRm < 25, Q814Exp < 11, hholdsz < 15, Q58Val < 9, Q63NrCell < 11, income_pp < 50000, income_pp>0)

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


dt <- sort(sample(nrow(house.simple), nrow(house.simple)*.7))
train <- house.simple[dt,]
test <- house.simple[-dt,]


## Distribution of poverty levels


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
p_adult <- plot_target_by_var(train, adults)
p_income <- plot_target_by_var(train, income_pp)
p_room <- plot_target_by_var(train, Q55TotRm)
p_sex <- plot_target_by_var(train, head_sex)
p_val <- plot_target_by_var(train, Q58Val)
#display plots in a grid
grid.arrange(p_age, p_size, p_hhexp, p_cellphone, p_room , p_val, nrow = 2, top = textGrob("Distribution by poverty levels", gp=gpar(fontsize=15)))

# purple - food poverty line
# yellow = not vulnerable


###########################################################

#getting rid of the variables that represent the same variable just as monthly household income. Have three variables that basically say the same thing. I want to get rid of them so that we have better information.
a <- train[,-17] # get rid of income_pp
b <- a[,-13] # get rid of monthly salary - check.
corr_var(b, poverty_level, top=5)



