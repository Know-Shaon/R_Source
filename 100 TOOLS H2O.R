
#H2o
#Resource :https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/product/tutorials/gbm/gbmTuning.Rmd
#https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R

library("ggplot2")       #Dependents for CARET
library("scales")        #Dependents for CARET
#install.packages('ModelMetrics')
library("ModelMetrics")  #Dependents for CARET
library("ggplot2")    
#install.packages("caret", dependencies = c("Depends", "Suggests"))
library("caret")
library("RANN")
library("ggplot2")

#test <- read.csv("~\Mini_hack\test.csv",stringsAsFactors=F)
train <- read.csv("~/MH_rstudio-export/train_63qYitG.csv")




#look for levels and replace with numbers in train set
train$Type_of_Cab=as.numeric(train$Type_of_Cab)
train$Type_of_Cab[train$Type_of_Cab=="A"]<-"1"
train$Type_of_Cab[train$Type_of_Cab=="B"]<-"2"
train$Type_of_Cab[train$Type_of_Cab=="C"]<-"3"
train$Type_of_Cab[train$Type_of_Cab=="D"]<-"4"
train$Type_of_Cab[train$Type_of_Cab=="E"]<-"5"
train$Type_of_Cab=as.numeric(train$Type_of_Cab)

train$Confidence_Life_Style_Index=as.numeric(train$Confidence_Life_Style_Index)
train$Confidence_Life_Style_Index[train$Confidence_Life_Style_Index=="A"]<-"11"
train$Confidence_Life_Style_Index[train$Confidence_Life_Style_Index=="B"]<-"22"
train$Confidence_Life_Style_Index[train$Confidence_Life_Style_Index=="C"]<-"33"
train$Confidence_Life_Style_Index=as.numeric(train$Confidence_Life_Style_Index)


train$Destination_Type=as.numeric(train$Destination_Type)
train$Destination_Type[train$Destination_Type=="A"]<-"5"
train$Destination_Type[train$Destination_Type=="B"]<-"10"
train$Destination_Type[train$Destination_Type=="C"]<-"15"
train$Destination_Type[train$Destination_Type=="D"]<-"20"
train$Destination_Type[train$Destination_Type=="E"]<-"25"
train$Destination_Type[train$Destination_Type=="F"]<-"30"
train$Destination_Type[train$Destination_Type=="G"]<-"35"
train$Destination_Type[train$Destination_Type=="H"]<-"40"
train$Destination_Type[train$Destination_Type=="I"]<-"45"
train$Destination_Type[train$Destination_Type=="J"]<-"50"
train$Destination_Type[train$Destination_Type=="K"]<-"55"
train$Destination_Type[train$Destination_Type=="L"]<-"60"
train$Destination_Type[train$Destination_Type=="M"]<-"65"
train$Destination_Type[train$Destination_Type=="N"]<-"70"
train$Destination_Type=as.numeric(train$Destination_Type)



train$Gender<-ifelse(train$Gender=="Male",1,0)
train$Surge_Pricing_Type=as.factor(train$Surge_Pricing_Type)


#-----------------------------------------------------------------------
#Imputation
#-----------------------------------------------------------------------
train$Type_of_Cab[is.na(train$Type_of_Cab)]<-"6"
# Best features from CARET
predictors<-c('Type_of_Cab','Trip_Distance','Customer_Rating','Life_Style_Index','Destination_Type')  
response<-c('Surge_Pricing_Type')

preProcValues <- preProcess(train, method = c("medianImpute","center","scale"))
train_processed <- predict(preProcValues, train)


#-----------------------------------------------------------------------
# Create train and validation sets
#-----------------------------------------------------------------------
library(caTools)
split = sample.split(train_processed$Surge_Pricing_Type, SplitRatio = 0.70)
train_set = subset(train_processed, split == TRUE)
val_set = subset(train_processed, split == FALSE)



#install.packages("h2o")
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()

#data to h2o cluster
train.h2o <- as.h2o(train_set)
val.h2o <- as.h2o(val_set)




hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(1,10,1),                                      
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train))-1,1),                                 
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)









search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3500,         
  ## build no more than 100 models
  max_models = 100,                  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "logloss",
  stopping_tolerance = 1e-3
)






grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  ## which algorithm to run
  algorithm = "gbm",
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid", 
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train.h2o, 
  validation_frame = val.h2o,
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3500,                                                 
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "logloss", 
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)


sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)    


bm1 <- h2o.gbm(
  training_frame = train.h2o[,c(predictors,response)],    ## the H2O frame for training
  validation_frame = val.h2o[,c(predictors,response)],      ## the H2O frame for validation (not required)
  x=1:4,                        ## the predictor columns, by column index
  y=5,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

