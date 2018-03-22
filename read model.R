
# install.packages(c('recipes','lime','DALEX','xgboost','caret'))
# devtools::install_github('thomasp85/lime')
# prereq ------------------------------------------------------------------
require(readxl)
library(recipes)
require(tidyverse)
require(lime)
require(DALEX)
require(xgboost)

dir <- 'data/WA_Fn-UseC_-HR-Employee-Attrition.xlsx'
dir_res <- 'data/data_for_mod.csv'
# functions ---------------------------------------------------------------


# read and preproc file ---------------------------------------------------------------


df <- read_excel(dir) %>% 
  # mutate_if(is.character,as_factor) %>% 
  rename(id = EmployeeNumber) %>% 
  #equal for all employers
  select(-c(EmployeeCount,Over18,StandardHours))
col_con <- c("Age", 'DailyRate', 'HourlyRate','MonthlyIncome','MonthlyRate',
             'TotalWorkingYears','DistanceFromHome', 
             'YearsAtCompany', 'YearsInCurrentRole',	'YearsSinceLastPromotion',	'YearsWithCurrManager')

df_sum <- df %>% 
  gather(parameter,value,-c(id,Attrition)) 

df_sum %>% 
  filter(parameter %in% col_con) %>% 
  spread(parameter,value) %>% 
  mutate_if(is.character,as.numeric) %>% 
  summary()

res <- df_sum %>% 
  filter(!(parameter %in% col_con)) %>% 
  count(parameter, value) 

df_sum %>% 
  filter(!(parameter %in% col_con)) %>% 
  ggplot(aes(value,fill = Attrition)) +
  geom_histogram(stat="count") +
  facet_wrap(c('parameter'),scales = 'free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# prap data for modeling -----------------------------------------------------

recipe_obj <- df %>%
  recipe(formula = ~ .) %>%
  step_rm(id) %>%
  step_zv(all_predictors()) %>%
  step_center(col_con) %>%
  step_scale(col_con) %>%
  # step_string2factor(Attrition) %>%
  prep(data = df)
recipe_obj

df_model <- bake(recipe_obj, newdata = df)

df_model <- df_model %>% 
  mutate_if(is.character,as_factor)
#write data for h2o
write_csv(df_model, dir_res)



# split data --------------------------------------------------------------

smp_size <- floor(0.75 * nrow(df_model))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df_model)), size = smp_size)

train <- df_model[train_ind, ]
test <- df_model[-train_ind, ]

# modeling ----------------------------------------------------------------

#GLM
mod_glm <- caret::train(Attrition ~.,
                        family = 'binomial',
                        data = train,
                        method = 'glm')

#Xgboost
train_mod_matrix <- model.matrix(Attrition ~.-1,
                                 data = mutate(train,Attrition = as.numeric(Attrition) - 1) )
train_xgb <- xgb.DMatrix(data = train_mod_matrix ,
                         label = as.numeric(train$Attrition) - 1 )
param <- list(objective = "binary:logistic",num_parallel_tree = 5,
              max_depth = 2, eta = 1, silent = 1, nthread = 2)
mod_xgb <- xgboost(params = param,data = train_xgb,nrounds = 50)


# explain by LIME ---------------------------------------------------------------

explainer <- lime(
  as.data.frame(train[,-2]),
  model = mod_glm,
  bin_continuous = T,quantile_bins = T)

to_exp <- as.data.frame(test) %>% 
  # filter(OverTime == 'Yes') %>% 
  .[1:5,]

explanation <- lime::explain(
  to_exp[,-2],
  single_explanation = T,
  explainer = explainer,
  n_labels = 2,
  n_features = 5)

# Error: All permutations have no similarity to the original observation.
# Try setting bin_continuous to TRUE and/or increase kernel_size

#Cannot Continue
plot_features(explanation)

plot_explanations(explanation)

tibble::glimpse(explanation)






# Explain using DALEX -----------------------------------------------------------



explainer_xgb <- explain(mod_xgb, 
                         data = train_mod_matrix, 
                         y = train$Attrition, 
                         label = "xgboost")
explainer_xgb

#single variable
col_con

sv_xgb_satisfaction_level  <- single_variable(explainer_xgb, 
                                              variable = "OverTimeYes", 
                                              type = "pdp")
head(sv_xgb_satisfaction_level)
plot(sv_xgb_satisfaction_level)+ theme_bw()





