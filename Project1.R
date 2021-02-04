#Set working directory on my computer
setwd("C:/Users/hming/OneDrive - NTNU/Skole/5.klasse/VÅR/StatLær2")
getwd()

#load packages
library(GGally)
library(tidyverse)
library(glmnet)
library(grplasso)

#Load data set
munich_house <- read.table(
  "https://data.ub.uni-muenchen.de/2/1/miete03.asc",
  sep="\t", header=TRUE)
#ggpairs(munich_house) No serious signs of multicolinearity

munich_house$bez=as.factor(munich_house$bez)
munich_house$bj=as.factor(munich_house$bj)
str(munich_house)

#create model matrix without intercept and nmqm
x_mod <- model.matrix(nm~.-nmqm,data=munich_house)[,-1]
y_mod <- munich_house$nm
df_mod <- data.frame(y_mod,x_mod)
colnames(df_mod)[1]="nm"

#### Vanilla regression model for reference

lm_mod <- lm(nm~.,data=df_mod) 
#Printing the summary to assess significant parameters
summary(lm_mod)

#### General lasso 

gen_mod <- glmnet(x=x_mod,y=y_mod,alpha=1,standardize=TRUE)
gen_lambda_mod <- gen_mod$lambda
gen_lambda_mod

plot(gen_mod,xvar="lambda",label=TRUE)
plot(gen_mod) #plot against the L1 norm

#Choosing optimal lambda performing Crossvalidation
cv_gen_mod=cv.glmnet(x=x_mod,y=y_mod,alpha=1)
print(paste("The lamda giving the smallest CV error",cv_gen_mod$lambda.min))
print(paste("The 1sd err method lambda",cv_gen_mod$lambda.1se))
plot(cv_gen_mod)

#use 1sd error rule default
plot(gen_mod,xvar="lambda",label=TRUE)
abline(v=log(cv_gen_mod$lambda.1se))

coef(gen_mod, s= cv_gen_mod$lambda.1se)
results_mod=cbind(coef(gen_mod,s=cv_gen_mod$lambda.1se),coef(lm_mod))
colnames(results_mod)=c("general lasso","vanilla LS")
print(results_mod)

#### Group Lasso

#Adding an intercept to the design matrix
x_mod_group <- cbind(1, x_mod)
colnames(x_mod_group)[1] <- c("Intercept")
df_mod_group <- data.frame(y_mod,x_mod_group)
colnames(df_mod_group)[1] <-c("nm")

#Defining the grouped classs
district <- rep(4,24) #25 levels of factor bez, why 24? This must be changed according to the design matrix
year <- rep(3,43) #44 levels of factor bj, has to be 43?
#index_mod <- c(NA,1,2,year,district,5,5,6,7,8,8,9) #must have length equal to nr of columns in xss
index_mod <- c(NA,1,2,year,district,5,6,7,8,9,10,11) #individual groups for all except bez and bj

grp_lambda_mod <- lambdamax(x=x_mod_group, y = y_mod, index = index_mod, penscale = sqrt,
                          model = LinReg(),standardize = TRUE,center = TRUE)*0.5^(0:70)



#fit using grouped index
grp_mod <- grplasso(x=x_mod_group, y = y_mod, index = index_mod, lambda = grp_lambda_mod, model = LinReg(),
                             penscale = sqrt,
                             control = grpl.control(update.hess = "lambda", trace = 0))


plot(grp_mod)
#don not need all lambdas, remove the first 3
grp_mod <- grplasso(x=x_mod_group, y = y_mod, index = index_mod, lambda = grp_lambda_mod[-(1:3)], model = LinReg(),
                    penscale = sqrt,
                    control = grpl.control(update.hess = "lambda", trace = 0))


## Plot coefficient paths
plot(grp_mod)
str(grp_mod)
coef(grp_mod) #s=cv.lasso.group.test$lambda.1se)#[,1]

#CV for å finne optimal lambda? Bootstrapping? 
library(gglasso)
#package chosen lambdagrid
fitls_mod <- gglasso(x = x_mod_group[,-1], y = y_mod, group = index_mod[-1], loss = "ls")
plot(fitls_mod)
#own defined lambdagrid
fitls_mod <- gglasso(x = x_mod_group[,-1], y = y_mod, group = index_mod[-1], loss = "ls",lambda=grp_lambda_mod)
plot(fitls_mod)

#crossvalidation on self defined lambdagrid - this takes some time
cvfitls_mod <- cv.gglasso(x = x_mod_group[,-1], y = y_mod, group = index_mod[-1], loss = "ls",lambda=grp_lambda_mod)
plot(cvfitls_mod)
str(cvfitls_mod)
cvfitls_mod$lambda.1se

#fit with optimal lambda on defined lambda grid(either package made or made by me)
fin_mod=gglasso(x = x_mod_group[,-1], y = y_mod, group = index_mod[-1], loss = "ls",lambda=cvfitls_mod$lambda.1se)
coef(fin_mod)

#testing with a fixed lambda
test_mod=gglasso(x = x_mod_group[,-1], y = y_mod, group = index_mod[-1], loss = "ls",lambda=5)
coef(test_mod)

#final results, group lasso either shrinks all or nothing and even increases some estimated parameter coefficients, what do we take away from this? 
results_mod=cbind(coef(fin_mod),coef(gen_mod,s=cv_gen_mod$lambda.1se),coef(lm_mod))
colnames(results_mod)=c("group lasso","general lasso","vanilla LS")
print(results_mod)
