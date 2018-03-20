#Install packs for analyzing
# # dependencies
# devtools::install_github("pbiecek/breakDown")
# 
# # DALEX package
# devtools::install_github("pbiecek/DALEX")

# prereq ------------------------------------------------------------------

require(tidyverse)
require(h2o)
require(lime)

dir <- 'data/data_for_mod.csv'
h2o.init(nthreads = 3)
seed = 1

# functions ---------------------------------------------------------------


# read and split data -----------------------------------------------------

data <- h2o.importFile(dir)


data$Attrition <- as.factor(data$Attrition)  #encode the binary repsonse as a factor
h2o.levels(data$Attrition)

splits <- h2o.splitFrame(data = data, 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = seed)  #setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# Identify response and predictor variables
y <- "Attrition"
x <- setdiff(names(data), c(y,'ID'))  #remove the interest rate column because it's correlated with the outcome
print(x)


# modeling ----------------------------------------------------------------

# Now that we have prepared the data, we can train some models
# We will start by training a single model from each of the H2O supervised algos:
# 1. Generalized Linear Model (GLM)
# 2. Distributed Random Forest (DRF)
# 3. Gradient Boosting Machine (GBM)
# 4. Deep Learning (DL)
# 5. Naive Bayes (NB)


###### 1. Generalized Linear Model (GLM)  ######
{
  # cross-val,auto lambda search and balance classes when cross-valid
  glm_fit1 <- h2o.glm(x = x, 
                      y = y, 
                      training_frame = train,
                      model_id = "glm_fit1",
                      validation_frame = valid,
                      nfolds = 3,
                      seed = seed,
                      lambda_search = T,
                      balance_classes = T,
                      # interactions = c(2,3),
                      alpha = 0.5,
                      family = "binomial")  #similar to R's glm, h2o.glm has the family argument
  h2o.auc(glm_fit1)
  h2o.performance(glm_fit1)
  
  #grid search for alpha parameter
  # select the values for `alpha` to grid over
  hyper_params <- list( alpha = c(0, .025, .05, .075, .1) )
  
  # this example uses cartesian grid search because the search space is small
  # and we want to see the performance of all models. For a larger search space use
  # random grid search instead: {'strategy': "RandomDiscrete"}
  
  # build grid search with previously selected hyperparameters
  grid <- h2o.grid(x = x, y = y, 
                   training_frame = train, 
                   validation_frame = valid,
                   algorithm = "glm",
                   nfolds = 3,
                   seed = seed,
                   lambda_search = T,
                   balance_classes = T,
                   family = 'binomial', 
                   grid_id = "glm_fit1_grid1", 
                   hyper_params = hyper_params,
                   search_criteria = list(strategy = "Cartesian"))
  
  # Sort the grid models by AUC
  sortedGrid <- h2o.getGrid("glm_fit1_grid1", sort_by = "auc", decreasing = T)
  
  glm_fit_tuned <- h2o.glm(x = x, 
                           y = y, 
                           training_frame = train,
                           model_id = "glm_fit_tuned",
                           validation_frame = valid,
                           nfolds = 3,
                           seed = seed,
                           alpha = 0.075,
                           lambda_search = T,
                           balance_classes = T,
                           family = "binomial")  #similar to R's glm, h2o.glm has the family argument
  h2o.auc(glm_fit_tuned)
  h2o.performance(glm_fit_tuned)
  glm_predict <- h2o.performance(glm_fit_tuned,test)
  h2o.auc(glm_predict)
}

# try to explain result ---------------------------------------------------
h2o.varimp_plot(glm_fit_tuned)
h2o.varimp(glm_fit_tuned)

explainer <- lime(
  as.data.frame(train[,-2]),
  model = glm_fit_tuned,
  bin_continuous = T,quantile_bins = T)

to_exp <- as.data.frame(test) %>% 
  filter(OverTime == 'Yes') %>% 
  .[1:3,]

explanation <- explain(
  to_exp[,-2],
  # single_explanation = T,
  explainer = explainer,
  n_labels = 2,
  n_features = 5)

# Error: All permutations have no similarity to the original observation.
# Try setting bin_continuous to TRUE and/or increase kernel_size

#Cannot Continue
plot_features(explanation)

plot_explanations(explanation)
tibble::glimpse(explanation)


