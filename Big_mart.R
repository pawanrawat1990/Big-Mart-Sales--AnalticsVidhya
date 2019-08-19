################Packages/Functions Catalog####################################

##install.packages("readxl") - For import excel file , both xls and xlsx.
##install.packages("data.table") - Import all files with fread function, do not import categorical as factors.
##install.packages("dplyr") - For data manipulation
##install.packages("ggplot2")- For grapgh and charts
##install.packages("car")- For scatter plots and VIF functions
##install.packages("MASS")- Step AIC fucntion.
##install.packages("caTools")- Sampling is easy
##install.packages("sqldf")- Sql functions
##install.packages("forecast")- For time series forecasting . ETS and auto.arima is part of it.
##install.packages("ROCR")- ROC curve
##install.packages("psych") - Factor analysis
##install.packages("cowplot")## - Combine multiple plots

# library(readxl)
# library(data.table)
# library(dplyr)
# library(ggplot2)
# library(car)
# library(MASS)
# library(caTools)
# library(sqldf)
# library(forecast)
# library(ROCR)
# library(psych)
# library(cowplot)


####################Importing Packages from dir##############################################

source("D:/R Analytics/Regression Project/Packages.R")


####Importing training data#########
train_data <- read.csv(choose.files(), header = T)
str(train_data)
View(train_data)


###Importing test data#############
test_data <- read.csv(choose.files(), header =T)

final <- fread("D:/R Analytics/Analytics Vidhya/Big Mart/SampleSubmission_TmnO39y (3).csv")

dim(train_data)
dim(test_data)


####combining both data and doing some EDA######################
 
test_data$Item_Outlet_Sales <- 0 ###as we do not have target variable in test_data,we need to add the column to it before append##



combi <- rbind(train_data, test_data)

###### Descriptive statistics###############################

mystats <- function(x){
  nmiss <- sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  s <- sd(a)
  min <- min(a)
  max <- max(a)
  q1 <- quantile(a,0.01)
  q95 <- quantile(a,0.95)
  q99 <- quantile(a,0.99)
  out1 <- m+3*s
  iqr <- IQR(a)
  Q3 <- quantile(a,0.75)
  Q1 <- quantile(a,0.25)
  out2 <- Q3+1.5*iqr
  out3 <- Q1-1.5*iqr
return(c(nmiss=nmiss, mean=m,sdv=s,min=min,max=max,q1=q1,q95=q95,q99=q99,out1=out1,out2=out2,out3=out3))
}

num_var <- sapply(train_data,is.numeric)
num_var1 <- sapply(combi,is.numeric)

diag_stats1 <- t(data.frame(apply(train_data[num_var],2,mystats)))
diag_stats2 <- t(data.frame(apply(combi[num_var1],2,mystats)))


write.csv(diag_stats1, file="D:/R Analytics/Analytics Vidhya/Big Mart/diag_stats1.csv")
write.csv(diag_stats2, file= "D:/R Analytics/Analytics Vidhya/Big Mart/diag_stats2.csv")

#### Outlier Capping @m+3SD###################
combi$Item_Weight[combi$Item_Weight > 20.85] <- 20.85
combi$Item_MRP[combi$Item_MRP > 262.76] <- 262.76
combi$Item_Visibility[combi$Item_Visibility > 0.22624886904] <- 0.22624886904
combi$Item_Outlet_Sales[combi$Item_Outlet_Sales > 7366 ] <- 7366

####EDA on target continuous Variables####################

p1 <- ggplot(train_data)+geom_histogram(aes(Item_Outlet_Sales), fill = "blue", binwidth = 100) #### Variable is skewed , will required transformation###


#############EDA on independent continuous variables########################
p2 <- ggplot(combi)+geom_histogram(aes(Item_Weight), fill= "blue", binwidth=0.5)
p3 <- ggplot(combi)+geom_histogram(aes(Item_Visibility), fill= "blue", binwidth=0.005)
p4 <- ggplot(combi)+geom_histogram(aes(Item_MRP), fill= "blue", binwidth=1)

plot_grid(p2,p3,p4,nrow=1)#### plotting item weight,MRP, visiblity all continuous variables#########################


#########EDA on independent categorical variable########################

ggplot(combi %>% group_by(Item_Fat_Content)%>% summarise(count=n()))+
  geom_bar(aes(Item_Fat_Content,count), stat="identity", fill= "coral1")+
  geom_label(aes(Item_Fat_Content, count, label= count), vjust= 0.5)+
  theme(axis.text.x=element_text(angle=45, hjust=1))


combi$Item_Fat_Content <- as.character(combi$Item_Fat_Content)

##############replacing with the correct names####################
combi$Item_Fat_Content[combi$Item_Fat_Content=="LF"] <-  "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="low fat"] <-"Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="reg"] <-"Regular"

combi$Item_Fat_Content <- as.factor(combi$Item_Fat_Content)

str(combi)


p5 <- ggplot(combi %>% group_by(Item_Fat_Content)%>% summarise(count=n()))+  ###creating chart after correcting names###
  geom_bar(aes(Item_Fat_Content,count), stat="identity", fill= "coral1")+
  geom_label(aes(Item_Fat_Content, count, label= count), vjust= 0.5)+
  theme(axis.text.x=element_text(angle=45, hjust=1))
                                   
p6 <- ggplot(combi %>% group_by(Item_Type)%>% summarise(count=n()))+
  geom_bar(aes(Item_Type ,count), stat="identity", fill= "coral1")+
  geom_label(aes(Item_Type,count, label= count), vjust= 0.5)+
  theme(axis.text.x=element_text(angle=45, hjust=1))

p7 <- ggplot(combi %>% group_by(Outlet_Identifier)%>% summarise(count=n()))+
  geom_bar(aes(Outlet_Identifier ,count), stat="identity", fill= "coral1")+
  geom_label(aes(Outlet_Identifier,count, label= count), vjust= 0.5)+
  theme(axis.text.x=element_text(angle=45, hjust=1))

plot_grid(p5,p6,p7, nrow=3)########### Plotting item fat,type and outlet ID############################

p8 <- ggplot(combi %>% group_by(Outlet_Size )%>% summarise(count=n()))+
  geom_bar(aes(Outlet_Size  ,count), stat="identity", fill= "coral1")+
  geom_label(aes(Outlet_Size ,count, label= count), vjust= 0.5)+
  theme(axis.text.x=element_text(angle=45, hjust=1))

p9 <- ggplot(combi %>% group_by(Outlet_Location_Type)%>% summarise(count=n()))+
  geom_bar(aes(Outlet_Location_Type  ,count), stat="identity", fill= "coral1")+
  geom_label(aes(Outlet_Location_Type ,count, label= count), vjust= 0.5)+
  theme(axis.text.x=element_text(angle=45, hjust=1))

p10 <- ggplot(combi %>% group_by(Outlet_Type)%>% summarise(count=n()))+
  geom_bar(aes(Outlet_Type  ,count), stat="identity", fill= "coral1")+
  geom_label(aes(Outlet_Type ,count, label= count), vjust= 0.5)+
  theme(axis.text.x=element_text(angle=45, hjust=1))

plot_grid(p8,p9,p10,nrow=3)######## Plotting outlet size, location and type############################



train <- combi[1:nrow(train_data),]
View(train)
str(train)######While combining target variable become character####################



###############Bivariate analysis of continuous variable#######################
p11 <- ggplot(train)+geom_point(aes(Item_Weight,Item_Outlet_Sales),colour="red")
p12 <- ggplot(train)+geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour="red")
p13 <- ggplot(train)+geom_point(aes(Item_MRP,Item_Outlet_Sales), colour="red")

plot_grid(p11,p12,p13,nrow=3)##############Plotting bivariate continuous variable######################


####################Bivariate analysis of categorical variable using vaolin######################
p14 <- ggplot(train)+geom_boxplot(aes(Item_Fat_Content, Item_Outlet_Sales),fill="red")+
  theme(axis.text.x=element_text(angle=45, hjust =1), axis.text =element_text(size=8),
        axis.title = element_text(size=14))


p15 <- ggplot(train)+geom_boxplot(aes(Item_Type, Item_Outlet_Sales),fill = "blue")+
  theme(axis.text.x=element_text(angle =45, hjust = 1 ), axis.text = element_text(size =8), 
        axis.title = element_text(size=14))

p16 <- ggplot(train)+geom_boxplot(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "blue")+
  theme(axis.text.x = element_text(angle = 45,hjust=1), axis.title = element_text(size = 14))


plot_grid(p14,p15,p16,nrow=3)


######Distribution of target variable accorss the outlet size############################

p17 <- ggplot(train)+geom_boxplot(aes(Outlet_Size,Item_Outlet_Sales),fill="blue")+
  theme(axis.text.x=element_text(angle = 45, hjust=1), axis.title= element_text(size=14))+
  labs(title="Boxplot")


p18 <- ggplot(train)+geom_boxplot(aes(Outlet_Location_Type,Item_Outlet_Sales),fill="blue")+
  theme(axis.text.x=element_text(angle=45, hjust=1),axis.title=element_text(size=14))

p19 <- ggplot(train)+geom_boxplot(aes(Outlet_Type,Item_Outlet_Sales),fill="blue")+
  theme(axis.text.x=element_text(angle=45, hjust=1),axis.title=element_text(size=14))


plot_grid(p17,p18,p19,nrow=1)


######################Missing Value Treatment#####################################################3

#####Item Weight####################################################
sum(is.na(combi$Item_Weight))


##Imputing mean using loop###################################
missing_index <- which(is.na(combi$Item_Weight))
for(i in missing_index){
combi$Item_Weight[i] <- mean(combi$Item_Weight[combi$Item_Identifier==combi$Item_Identifier[i]],na.rm=T)  
}

combi$Item_Weight[combi$Item_Weight > 20.85] <- 20.85
#####Item_Visiblity- Replacing zeros############################################################


zero_values <- which(combi$Item_Visibility==0)

for(i in zero_values){
  combi$Item_Visibility[i] <- mean(combi$Item_Visibility[combi$Item_Identifier==combi$Item_Identifier[i]], na.rm=T)
}

combi$Item_Visibility[combi$Item_Visibility > 0.22624886904] <- 0.22624886904

ggplot(combi)+geom_histogram(aes(Item_Visibility), fill = "blue", binwidth = 0.001)



####################Creating new variable to improve the performance of the model#######################


######Creating Item Category Variable###############################

table(combi$Item_Type, substr(combi$Item_Identifier,1,2))

combi$Item_category <- substr(combi$Item_Identifier,1,2)

combi$Item_category <- as.factor(combi$Item_category)

################Changing Item fat content wherever Item_category is NC#############################

combi$Item_Fat_Content <- as.character(combi$Item_Fat_Content)#########Changing it to character so that text could be fit in###
combi$Item_Fat_Content[combi$Item_category == "NC"] <- "Non-Edible"
combi$Item_Fat_Content <- as.factor(combi$Item_Fat_Content)#######Changing it to factor again#########

########Number of years of outlet###################
combi$outlet_years <- 2013-combi$Outlet_Establishment_Year

#####Price per Weight###########################
combi$price_per_unit_weight <- combi$Item_MRP/combi$Item_Weight

###Chaning years from int to factors########################
combi$Outlet_Establishment_Year <- as.factor(combi$Outlet_Establishment_Year)

###########Creating new varaible based on graph we have seen for Item_MRP###########################
combi$Item_MRP_Clusters <- ifelse(combi$Item_MRP < 69,1,
                        ifelse(combi$Item_MRP>=69 & combi$Item_MRP <= 136, 2, 
                               ifelse(combi$Item_MRP >=136 & combi$Item_MRP <=203,3,4)))
                               
####################Lable Encoding and Hot encoding############################

combi$Outlet_Size[combi$Outlet_Size==""] <- "Small"#######Replacing blank with small outlet size based on ggplot p17#########

combi$Outlet_Size_Num <- ifelse(combi$Outlet_Size=="Small",0,
                                ifelse(combi$Outlet_Size=="Medium",1,2))


combi[,c("Outlet_Size")] <- NULL

View(combi)




training <- combi[1:nrow(train_data),]
testing <- combi[nrow(train_data)+1:nrow(test_data),]


#### Modelling #############################


############# XGBoost###############################################

library(xgboost)
library(Matrix)

xg_matrix <- sparse.model.matrix(Item_Outlet_Sales~price_per_unit_weight+Item_Weight+Item_MRP
                                 +outlet_years+Item_MRP_Clusters+Outlet_Type+Item_Type+Item_category+Outlet_Identifier+
                                 +Item_Visibility+Item_Fat_Content+Outlet_Size_Num+Outlet_Location_Type-1, data = training)

xg_matrix_test <- sparse.model.matrix(~price_per_unit_weight+Item_Weight+Item_MRP
                                      +outlet_years+Item_MRP_Clusters+Outlet_Type+Item_Type+Item_category+Outlet_Identifier+
                                        +Item_Visibility+Item_Fat_Content+Outlet_Size_Num+Outlet_Location_Type-1, data= testing)


xg_training <- xgb.DMatrix(as.matrix(xg_matrix),label=training$Item_Outlet_Sales)

xg_testing <- xgb.DMatrix(as.matrix(xg_matrix_test))

xgb_colsamp <- c()
xgb_subsample <-c()
xgb_rmse <- c()
xgb_index <- c()

for(i in c(0.7,0.75,0.8,0.85)){
  
  for (j in c(0.5,0.6,0.7,0.8,0.9)){
  
    parameter <- list(objective="reg:linear",eval_metric = "rmse",eta=0.1,gamma=1,max_depth=6,subsample=i,colsample_bytree=j, seed=1234)
    
    
    fit4 <- xgb.cv(parameter,xg_training,nround=1000,nfold=5,early_stopping_rounds = 30,maximize = F)
    
    xgb_subsample <- c(xgb_subsample,i)
    xgb_colsamp <- c(xgb_colsamp,j)
    fit_index <- fit4$best_iteration
    min_rmse <- fit4$evaluation_log[fit_index]$test_rmse_mean
    xgb_index <- c(xgb_index,fit_index)
    xgb_rmse <- c(xgb_rmse,min_rmse)
    }
  }


xgb_results <- data.frame(xgb_subsample= xgb_subsample,xgb_colsamp=xgb_colsamp,xgb_index=xgb_index,xgb_rmse=xgb_rmse)
 

############## After Iterations - Choosing the best parameter ###################################
parameter_1 <- list(objective="reg:linear",eval_metric = "rmse",eta=0.1,gamma=1,max_depth=6,subsample=0.80,colsample_bytree=0.7, seed=1234)
 
xgb_fit4 = xgb.train(data = xg_training, params = parameter_1, nrounds = 36)

final$Item_Outlet_Sales = predict(xgb_fit4,xg_testing)

write.csv(final, "D:/R Analytics/Analytics Vidhya/SampleSubmission_TmnO39y.csv", row.names = F)

                    ### Leader Board Score - 1158 

########################## Other Models ##############################################


# ################ Random Forest #######################################
# 
# library(caret)
# 
# 
# custom <- trainControl(method="cv",
#                        number=10)
# 
# grid <- expand.grid(maxdepth=6)
# 
# fit2 <- train(Item_Outlet_Sales~price_per_unit_weight+Item_Weight+Item_MRP
#               +outlet_years+Item_MRP_Clusters+Outlet_Type+Item_Type+Item_category+Outlet_Identifier+
#                 +Item_Visibility+Item_Fat_Content+Outlet_Size_Num+Outlet_Location_Type,method="rf",tunegird=grid,trControl=custom,metric="RMSE",maximise=F,data=training)
# 
# fit2
# 
# ##Training Data
# rf_t1 <-cbind(training, pred_sales=(predict(fit2,training)))
# RMSE <- sqrt(mean((training$Item_Outlet_Sales-rf_t1$pred_sales)^2))
# 
# #Test Data
# final$Item_Outlet_Sales <- predict(fit2,newdata=testing)
# 
# write.csv(final, file= "D:/R Analytics/Analytics Vidhya/Big Mart/.randomF_1.csv", row.names = F)
# 
# 
# rf_submission_file <- fread("D:/R Analytics/Analytics Vidhya/Big Mart/.randomF_1.csv")
# xg_submission_file <- fread("D:/R Analytics/Analytics Vidhya/SampleSubmission_TmnO39y.csv")
# 
# final$Item_Outlet_Sales <- (rf_submission_file$Item_Outlet_Sales+xg_submission_file$Item_Outlet_Sales)/2
# 
# write.csv(final, file= "D:/R Analytics/Analytics Vidhya/Big Mart/SampleSubmission_TmnO39y.csv", row.names = F)
# 
# 
# 
# 
# # ################## KNN #######################################################
# # 
# # library(class)
# # 
# # set.seed(1234)
# # 
# # knnfit5 <- train(Item_Outlet_Sales~price_per_unit_weight
# #                  +outlet_years+Item_MRP_Clusters+Outlet_Type
# #                  +Item_Visibility+Outlet_Size_Num+Outlet_Location_Type_num,data=training, method="knn",trControl=custom,preProcess=c("center","scale"),tunelength=8)
# # 
# # knn_t1 <- cbind(testing,pred_sales=predict(knnfit5,testing))
# # 
# # write.csv(knn_t1[,c("pred_sales","Item_Identifier","Outlet_Identifier")], file= "D:/R Analytics/Analytics Vidhya/Big Mart/.knn.csv")
# # 
# # 
# # ############# Regularized Regression ######################################
# # 
# # library(glmnet)
# # 
# # glm_train <- dplyr::select(training,-c(Item_Identifier,Outlet_Identifier))
# # 
# # glm_tr_matrix <- model.matrix(ln_Item_Outlet_Sales~.-1, data=glm_train)
# # y <- training$ln_Item_Outlet_Sales
# # 
# # glm_test <- dplyr::select(testing, -c(Item_Identifier,Outlet_Identifier))
# # glm_ts_matrix <- model.matrix(~.-1, data=glm_test)
# # 
# # control <- trainControl(method="Cv",
# #                         number=10)
# # 
# # lambdas <- seq(1,0,-0.001)
# # 
# # seed
# # 
# # model_ridge <- train(x=glm_tr_matrix,y=y,
# #                      method="glmnet",
# #                      metric="RMSE",
# #                      maximize=FALSE,
# #                      family="gaussian",
# #                      trControl=control,
# #                      tuneGrid=expand.grid(alpha=0, # Ridge regression
# #                                           lambda=lambdas))
# # 
# # 
# # p_glm <- ggplot(model_ridge$results[model_ridge$results$RMSE<0.50,])+geom_line(aes(x=lambda,y=RMSE))
# # 
# # 
# # mean(model_ridge$resample$RMSE)
# # 
# # 
# # 
# # glm_t1 <- cbind(testing, pred_sales=exp(predict(model_ridge,glm_ts_matrix)))

################################# Linear Model##################################

# hist(log(combi$Item_Visibility))
# hist(log(combi$price_per_unit_weight))
# hist(log(combi$Item_Weight))
# combi$ln_item_Visiblity <- log(combi$Item_Visibility+1)
# combi$ln_Price_per_weight <- log(combi$price_per_unit_weight+1)
# combi$ln_Item_weight <- log(combi$Item_Weight+1)
# var <- sapply(training, is.numeric)
# corr <- cor(training[,var])

# write.csv(corr,file="D:/R Analytics/Regression Project/cor.csv")
# # fit1 <- lm(ln_Item_Outlet_Sales~ln_Price_per_weight
# #            +outlet_years+Item_MRP_Clusters+Outlet_Type,data= training)
# # 
# # summary(fit1)
# # 
# # 
# # AIC(fit1)
# # 
# # vif(fit1)
# # 
# # stepAIC(fit1,direction="both")
# # 
# # 
# # t1 <- cbind(testing, pred_sales=exp(predict(fit1,testing)))
# # write.csv(t1[,c("pred_sales","Item_Identifier","Outlet_Identifier")], file= "D:/R Analytics/Analytics Vidhya/Big Mart/.linear.csv")
# # 
# # 
# # ########## Cross Validation Approach  LM Modelling ##########################
# # 
# # #Randomly shuffle the data
# # training<-training[sample(nrow(training)),]
# # 
# # #Create 5 equally size folds
# # folds <- cut(seq(1,nrow(training)),breaks=5,labels=FALSE)
# # 
# # #Perform 5 fold cross validation
# # modelNumber <- c()
# # modelRMSE <- c()
# # modelRsquare <- c()
# # for(i in 1:5){
# #   #Segement  data by fold using the which() function
# #   testIndexes <- which(folds==i,arr.ind=TRUE)
# #   testdata <- training[testIndexes, ]
# #   traindata <- training[-testIndexes, ]
# #   model <- lm(ln_Item_Outlet_Sales~ln_Price_per_weight
# #               +outlet_years+Item_MRP_Clusters+Outlet_Type, data= traindata)
# #   summary(model)
# #   
# #   predictions <- exp(predict(model,testdata))
# #   error <- predictions- testdata$Item_Outlet_Sales
# #   RMSE <- c(sqrt(mean(error^2)))
# #   modelNumber <- c(modelNumber, i)
# #   modelRMSE <- c(modelRMSE, RMSE)
# #   modelRsquare <- c(modelRsquare,summary(model)$r.squared)
# #   #Use the test and train data partitions however you desire...
# # }
# # 
# # results <- data.frame(modelNumber = modelNumber, modelRMSE = modelRMSE, modelRsquare= modelRsquare)
# # 







