 
# install.packages(c('recipes','DALEX','xgboost','caret','RColorBrewer'))
# devtools::install_github(c('thomasp85/lime',"pbiecek/breakDown","pbiecek/DALEX"))

# prereq ------------------------------------------------------------------
require(ggthemes)
require(RColorBrewer)
require(readxl)
library(recipes)
require(car)
require(caret)
require(pROC)
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
  theme_pander() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_sum %>% 
  filter(!(parameter %in% col_con)) %>% 
  group_by(parameter,value) %>% 
  count(Attrition) %>% 
  ungroup() %>% 
  group_by(parameter,value) %>% 
  mutate(prob = n/(n[Attrition == 'No'] + n[Attrition == 'Yes'])) %>% 
  ggplot(aes(value,prob *100, fill = Attrition)) +
  geom_col() +
  labs(y = "Percentage") +
  facet_wrap(c('parameter'),scales = 'free') +
  theme_pander() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



df %>% 
  ggplot(aes(y = YearsSinceLastPromotion, x = YearsAtCompany, colour = OverTime)) + 
  geom_jitter(size = 2, alpha = 0.8) + 
  geom_smooth(method = "gam") + 
  facet_wrap(~ Attrition,labeller = 'label_both') + 
  theme_pander() +
  scale_color_brewer(palette = "Paired")




df_sum %>% 
  filter((parameter %in% col_con)) %>% 
  mutate(value = as.numeric(value)) %>% 
  ggplot(aes(value,fill = Attrition)) +
  geom_density(alpha = 0.7) +
  facet_wrap(c('parameter'),scales = 'free') +
  theme_pander() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(GGally)
pp <- df_sum %>% 
  filter((parameter %in% col_con)) %>% 
  mutate(value = as.numeric(value)) %>% 
  spread(parameter,value) %>% 
  select(-id) %>% 
  ggpairs(aes(col = Attrition,alpha = 0.4),
          columns = c(2:12),
          lower = list(continuous = "smooth"), 
          cardinality_threshold = 60 ) +
  theme_pander()
for(i in 1:pp$nrow) {
  for(j in 1:pp$ncol){
    pp[i,j] <- pp[i,j] + 
      scale_fill_brewer(palette = "Paired") +
      scale_color_brewer(palette = "Paired")
  }
}
  
pp

unique(df$JobRole)




# prep data for modeling -----------------------------------------------------

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
#write_csv(df_model, dir_res)

levels(df_model$JobRole) <- c("HlRep", "HR", "LabTech", "Man", "ManDir", "ResDir", "ResSci", "SalEx", "SalRep")
levels(df_model$EducationField) <- c("HR", "LSci", "Mar", "Med", "Other", "TechDir")

# split data --------------------------------------------------------------

smp_size <- floor(0.75 * nrow(df_model))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df_model)), size = smp_size)

train <- df_model[train_ind, ]
test <- df_model[-train_ind, ]

# modeling ----------------------------------------------------------------

#GLM
mod_glm <- train(Attrition ~.,
                        family = 'binomial',
                        data = train,
                        method = 'glm')
summary(mod_glm)

preds <- predict(mod_glm, test)
rocv <- roc(as.numeric(test$Attrition), as.numeric(preds))
rocv$auc

prop.table(table(test$Attrition, preds, dnn = c("Actual", "Predicted")),1)


#Xgboost
train_mod_matrix <- model.matrix(Attrition ~.-1,
                                 data = mutate(train,Attrition = as.numeric(Attrition) - 1) )
train_xgb <- xgb.DMatrix(data = train_mod_matrix ,
                         label = as.numeric(train$Attrition) - 1 )

param <- list(objective = "binary:logistic",num_parallel_tree = 5,
              max_depth = 2, eta = 1, silent = 1, nthread = 2)
mod_xgb <- xgboost(params = param,data = train_xgb,nrounds = 50)
preds <- predict(mod_xgb, xgb.DMatrix(test))
rocv <- roc(as.numeric(test$Attrition), as.numeric(preds))
rocv$auc

prop.table(table(test$Attrition, preds, dnn = c("Actual", "Predicted")),1)

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
                                              variable = "YearsInCurrentRole", 
                                              type = "pdp")
head(sv_xgb_satisfaction_level)
plot(sv_xgb_satisfaction_level)+ theme_bw()





