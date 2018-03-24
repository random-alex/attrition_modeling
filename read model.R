 # install.packages('Ckmeans.1d.dp')
# install.packages(c('recipes','DALEX','xgboost','caret',
#                    'RColorBrewer','ggthemes','pROC','GGally',"DiagrammeR",'Ckmeans.1d.dp'))
# devtools::install_github(c('thomasp85/lime',"pbiecek/breakDown","pbiecek/DALEX"))

# prereq ------------------------------------------------------------------
require(ggthemes)
require(RColorBrewer)
require(readxl)
require(zoo)
library(recipes)
require(car)
require(caret)
require(pROC)
require(tidyverse)
require(lime)
require(DALEX)
require(xgboost)
require(GGally)
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

df_def <- read_excel(dir,2) %>% 
  mutate(group = na.locf(group)) %>% 
  separate('value',c('value_old','value_new'),sep = " '") %>% 
  mutate(value_new = str_replace_all(value_new,"'",''))
col_nes <- unique(df_def$group)

df_sum <- df %>% 
  gather(parameter,value,-c(id,Attrition)) 
df_tmp <- df_sum %>% 
  filter(parameter %in% col_nes) %>% 
  left_join(df_def, by = c("parameter" = "group",'value' = 'value_old')) %>% 
  mutate(value = value_new) %>% 
  select(-value_new)

df_sum <- df_sum %>% 
  filter(!(parameter %in% col_nes)) %>% 
  bind_rows(df_tmp)



df_sum %>% 
  filter(parameter %in% col_con ) %>% 
  spread(parameter,value) %>% 
  .[,-c(1,2)] %>% 
  mutate_if(is.character,as.numeric) %>% 
  summary()

res <- df_sum %>% 
  filter(!(parameter %in% c(col_con))) %>% 
  count(parameter, value) 


# some plots for factor data ----------------------------------------------


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
  labs(y = "Percentage", x = '') +
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
  mutate(prob = n/(n[Attrition == 'No'] + n[Attrition == 'Yes']),
         all = n[Attrition == 'No'] + n[Attrition == 'Yes']) %>% 
  select(-n) %>% 
  spread(Attrition,prob) %>% 
  # filter(Attrition == 'Yes') %>% 
  ungroup() %>% 
  arrange(desc(Yes)) %>% 
  .[1:5,] %>%
  gather(Attrition,prob,c(No,Yes)) %>% 
  mutate(par = as_factor(str_c(parameter,' : \n',value))) %>% 
  ggplot(aes(par,prob,fill = Attrition)) +
  geom_col() +
  geom_label(aes(label = str_c(all * prob )),position = position_stack(vjust = 0.5),size = 7) +
  theme_pander(lp = 'None') +
  labs(y = "Percentage", x = '') +
  scale_fill_brewer(palette = "Paired") 

df %>% 
  filter(JobRole == 'Sales Representative') %>%
  select(-JobRole) %>% 
  gather(parameter,value,-c(id,Attrition)) %>% 
  filter(!(parameter %in% col_con)) %>% 
  # group_by(parameter,value) %>% 
  # count(Attrition) %>% 
  # ungroup() %>% 
  # group_by(parameter,value) %>% 
  ggplot(aes(value,fill = Attrition)) +
  geom_histogram(stat="count") +
  facet_wrap(c('parameter'),scales = 'free') +
  theme_pander() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# some plots for count data -----------------------------------------------

df %>% 
  select(c(Attrition,YearsAtCompany,PercentSalaryHike,MonthlyIncome,OverTime, YearsSinceLastPromotion)) %>% 
  gather(parameter,value,-c(Attrition,YearsAtCompany,OverTime)) %>% 
  ggplot(aes(y = value, x = YearsAtCompany, colour = OverTime)) + 
  geom_jitter(size = 2, alpha = 0.8) + 
  geom_smooth(method = "gam") + 
  facet_wrap(c('parameter','Attrition' ),labeller = 'label_both',scales = 'free',ncol = 2) + 
  theme_pander(boxes = T) +
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


# prep data for modeling -----------------------------------------------------

df_model <- df_sum %>% 
  spread(parameter,value) %>% 
  mutate_at(vars(c(col_con,'PercentSalaryHike','NumCompaniesWorked')),funs(as.numeric))
df_model$Attrition <- as.numeric(as.factor(df_model$Attrition)) - 1

recipe_obj <- df_model %>% 
  recipe(formula = ~ .) %>%
  step_rm(id) %>%
  step_zv(all_predictors()) %>%
  step_center(col_con) %>%
  step_scale(col_con) %>%
  step_dummy(all_nominal()) %>% 
  # step_string2factor(Attrition) %>%
  prep(data = df)
recipe_obj

df_model <- bake(recipe_obj, newdata = df_model)

df_model <- df_model %>% 
  mutate_if(is.character,as_factor)
#write data for h2o
#write_csv(df_model, dir_res)

# levels(df_model$JobRole) <- c("HlRep", "HR", "LabTech", "Man", "ManDir", "ResDir", "ResSci", "SalEx", "SalRep")
# levels(df_model$EducationField) <- c("HR", "LSci", "Mar", "Med", "Other", "TechDir")

# split data --------------------------------------------------------------

smp_size <- floor(0.75 * nrow(df_model))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df_model)), size = smp_size)

train <- df_model[train_ind, ]
test <- df_model[-train_ind, ]

# modeling ----------------------------------------------------------------

#GLM
mod_glm <- train(as.factor(Attrition) ~ .,
                        family = 'binomial',
                        data = train,
                        method = 'glm')
summary(mod_glm)
preds <- predict(mod_glm, test)
rocv <- roc(as.numeric(test$Attrition), as.numeric(preds))
rocv$auc
plot(rocv)
prop.table(table(test$Attrition, preds, dnn = c("Actual", "Predicted")),1)

mod_glm_imp <- varImp(mod_glm,scale = T)

mod_glm_imp$importance %>% 
  mutate(parameter = rownames(.)) %>% 
  as.tibble() %>% 
  select(parameter,Overall) %>% 
  arrange(desc(Overall)) %>% 
  .[1:10,]


#Xgboost

num.folds <- 6
train_mod_matrix <- model.matrix(Attrition ~.-1,
                                 # data = mutate(train,Attrition = as.numeric(Attrition) - 1),
                                 data = train
                                 )
train_xgb <- xgb.DMatrix(data = train_mod_matrix ,
                         # label = as.numeric(train$Attrition) - 1,
                         label = as.numeric(train$Attrition)
                         )

test_mod_matrix <- model.matrix(Attrition ~.-1,
                         # data = mutate(test,Attrition = as.numeric(Attrition) - 1),
                         data = test
                         )
test_xgb <- xgb.DMatrix(data = test_mod_matrix ,
                         # label = as.numeric(test$Attrition) - 1 ,
                        label = as.numeric(test$Attrition)
                        )

sumwpos <- sum((as.numeric(train$Attrition)==1))
sumwneg <- sum(as.numeric(train$Attrition)==0)

param <- list("objective" = "binary:logistic",
              "scale_pos_weight" = sumwneg / sumwpos,
              "bst:eta" = 0.1,
              "bst:max_depth" = 6,
              "eval_metric" = "auc",
              "eval_metric" = "ams@0.15",
              "silent" = 1,
              "nthread" = 16)
{
# 
# trControl = trainControl(
#   method = 'cv',
#   number = 5,
#   # summaryFunction = giniSummary,
#   classProbs = TRUE,
#   verboseIter = TRUE,
#   allowParallel = TRUE)
# 
# # create the tuning grid. Again keeping this small to avoid exceeding kernel memory limits.
# # You can expand as your compute resources allow. 
# tuneGridXGB <- expand.grid(
#   nrounds=c(200,350),
#   max_depth = c(4, 10),
#   eta = c(0.05, 0.1),
#   gamma = c(0.01,0.1,0.5),
#   colsample_bytree = c(0.75,3,5,8),
#   subsample = c(0.50,0.7),
#   min_child_weight = c(0,0.5,3))
# 
# start <- Sys.time()
# 
# # train the xgboost learner
# xgbmod <- train(
#   x = train[,-2],
#   y = train$Attrition,
#   method = 'xgbTree',
#   metric = 'AUC',
#   trControl = trControl,
#   # tuneGrid = tuneGridXGB,
#   objective = "binary:logit")
} 

mod_xgb <- xgb.cv(param,data = train_xgb,
                  nrounds = 300,
                  nfold = num.folds,metrics = 'auc')
mod_xgb$evaluation_log
params <- mod_xgb$params

mod_xgb <- xgb.train(params,data = train_xgb,nrounds = 3000)
# make predictions
preds <- predict(mod_xgb, newdata = test_xgb, type = "prob")
summary(mod_xgb)
mod_xgb_imp <- xgb.importance(model = mod_xgb,feature_names = colnames(train_xgb))
mod_xgb_imp %>% 
  as.tibble() %>% 
  select(Feature, Importance) %>% 
  arrange(desc(Importance))
mod_xgb_imp %>% 
  as.tibble() %>% 
  rename(Importance = Gain) %>%  
  select(Feature, Importance) %>% 
  arrange(desc(Importance)) %>% 
  mutate(Importance = round(Importance,2)) %>% 
  .[1:10,]



xgb.ggplot.importance(mod_xgb_imp)
xgb.plot.multi.trees(mod_xgb,feature_names = colnames(train_xgb),features_keep = 2)

preds <- predict(mod_xgb, test_xgb,type = 'class')
rocv <- roc(as.numeric(test$Attrition),preds)
rocv$auc
plot(rocv)
prediction <- as.numeric(preds > 0.05)
rocv <- roc(as.numeric(test$Attrition),prediction)
plot(rocv)
rocv$auc
prop.table(table(test$Attrition, prediction, dnn = c("Actual", "Predicted")),1)

# explain by LIME ---------------------------------------------------------------

explainer <- lime(
  as.data.frame(train[,-2]),
  model = mod_glm,
  bin_continuous = T,quantile_bins = T)

to_exp <- as.data.frame(test) %>% 
  # filter(OverTime == 'Yes') %>% 
  .[1:5,]

explanation <- lime::explain(
  to_exp[,-1],
  single_explanation = T,
  explainer = explainer,
  n_labels = 1,
  n_features = 5)

# Error: All permutations have no similarity to the original observation.
# Try setting bin_continuous to TRUE and/or increase kernel_size

#Cannot Continue
plot_features(explanation)
plot_explanations(explanation)







# Explain using DALEX -----------------------------------------------------------



explainer_xgb <- explain(mod_xgb, 
                         data = train_mod_matrix, 
                         y = train$Attrition, 
                         label = "xgboost")
explainer_xgb

#single variable
col_con
mod_xgb_imp
sv_xgb_satisfaction_level  <- single_variable(explainer_xgb, 
                                              variable = "OverTime_Yes", 
                                              type = "ale")
head(sv_xgb_satisfaction_level)
plot(sv_xgb_satisfaction_level)+ theme_bw()



expl_dalex_glm <- explain(mod_glm,
                          data = train,
                          y = train$Attrition,
                          label = 'glm')
expl_dalex_xgb <- explain(mod_xgb, 
                         data = train_mod_matrix, 
                         y = train$Attrition, 
                         label = "xgboost")

expl_dalex_glm_sin <- single_variable(explainer = expl_dalex_glm,variable = 'DistanceFromHome')
expl_dalex_xgb_sin <- single_variable(explainer = expl_dalex_xgb,variable = 'DistanceFromHome')
plot(expl_dalex_glm_sin,expl_dalex_xgb_sin)
res1 <- variable_dropout(expl_dalex_xgb,type = 'raw')
res2 <- variable_dropout(expl_dalex_glm,type = 'raw')
plot(res1,res2)




