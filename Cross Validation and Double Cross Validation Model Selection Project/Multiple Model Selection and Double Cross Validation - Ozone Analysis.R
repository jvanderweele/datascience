################################################
################################################
################################################
################################################
#MULTIPLE METHOD SELECTION SEGMENT
################################################
################################################
################################################
################################################

####################open and prepare data for general use
myozone = read.csv('ozone.csv', header = T) #home
myozone = myozone[,-2] #exclude this response because we are using the numerical response
summary(myozone) #notice there are 5 NAs for our response variable

#####################Variable checking and transformation
hist(myozone$hourAverageMax) #we will transform because the distribution is skewed
table(myozone$weekday) #a few less cases for Thursday but overall there is an even distribution of days
hist(myozone$pressure500Height) #somewhat skewed distribution
hist(myozone$windSpeed) #fairly normal distribution
hist(myozone$humidity) #somewhat skewed distribution
hist(myozone$tempSandburg) #fairly normal distribution
hist(myozone$inversionBaseHeight) #non-normal distribution
hist(myozone$pressureGradientDaggett) #fairly normal distribution
hist(myozone$inversionBaseTemp) #fairly normal distribution
hist(myozone$visibility) #somewhat normal distribution

#Based on the above histograms, there are a few variables that would definitely benefit from transformations.
#For this reason, we will transform the response and 3 predictor variables: hourAverageMax, pressure500Height, humidity, inversionBaseHeight
myozone$loghourAverageMax = log(myozone$hourAverageMax)
myozone$logpressure500Height = log(myozone$pressure500Height)
myozone$loghumidity = log(myozone$humidity)
myozone$loginversionBaseHeight = log(myozone$inversionBaseHeight)

hist(myozone$loghourAverageMax)
hist(myozone$logpressure500Height)
hist(myozone$loghumidity)
hist(myozone$loginversionBaseHeight)

####################Ensure response and predictors are correct
ozone = data.frame(myozone[,-c(1, 2, 3, 5, 7)]) #use only the transformed variables and proper #exclude weekday (2)
ozone = na.omit(ozone) #remove rows with NA response or NA predictors, result is 330 obs, 36 cases removed

####Take a look at multicollinearity and VIF
fit = lm(loghourAverageMax ~ ., data=ozone)
library(car)
vif(fit)
summary(fit)
#remove inversionBaseTemp due to high VIF, refit and check again
ozone = ozone[,-4] #remove inversionBaseTemp
fit = lm(loghourAverageMax ~ ., data=ozone)
vif(fit) #all values <10 VIF
summary(fit) #no NAs so no perfectly redundant variables, no large standard errors
###View the daignostic plots:
plot(fit)#There are some outliers on Residuals vs. fitted; we see a generally constant spread on the Scale-Location plot
####Set up x and y
colnames(ozone) #see column names to determine which to use as predictors
y.response = ozone$loghourAverageMax # maximum 1-hour average ozone level in PPM
x.predictors = data.frame(ozone[,-c(6)]) #we have the 8 predictors, excluding the response
n = dim(ozone)[1] #330 observations
names(ozone)
summary(ozone) #check that everything looks alright



####################Additional inspection of Correlations
#check multicollinearity
pairs(ozone) #shows definite prescence between pressure500Height and tempSandburg, tempSandburg and inversionBaseTemp and others
cor(ozone$logpressure500Height, ozone$tempSandburg)
correlations <- cor(ozone)
round(correlations, 2)
#inversionBaseTemp and inversionBaseHeight
#check Variance Inflation Factor for >= 10 indicating 90% or more of variation can be explained by other predictors


###################Begin Model Preparation
#######Linear Regression with Subsets:
nRegsubmodels = 7 #specify the number of variables that will be considered in the subsets, based on possible predictors, will equal number of CV values later
#Create our function to use within the process:
predict.regsubsets <- function(object, newdata, id, ...){ #begin prediction function for regsubsets
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[ , xvars] %*% coefi
} # end function predict.regsubsets

#######Regression Subsets:
library(glmnet)  # use RR and LASSO modeling commands from package glmnet
# RR model specification and number
lambdalistRR  = #basic list: c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5)  # specifies RR models to consider
  c((1:100)/100) #we will use a list of 100 values
nRRmodels = length(lambdalistRR)
# LASSO model specification and number
lambdalistLASSO  = #c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5)  # specifies LASSO models to consider
  c((1:100)/100)  #we will use a list of 100 values
nLASSOmodels = length(lambdalistLASSO)

#######Total Number of Models:
nmodels = nRRmodels+nLASSOmodels+nRegsubmodels


#specify the data set used to perform the model selection, correct order
ozone = ozone[,c(5, 1:4, 6:8)]
fulldata.in = ozone
# set seed for randomizing CV fold selection
set.seed(8, sample.kind = "Rounding")



###########################
## Full modeling process ##
###########################

# we begin setting up the model-fitting process to use notation that will be
# useful later, "in"side a validation
n.in = dim(fulldata.in)[1]
x.in = model.matrix(loghourAverageMax~.,data=fulldata.in)[,-c(1)] #1 is intercept,
y.in = fulldata.in[,1] #1 is the response
# number folds and groups for (inner) cross-validation for model-selection
k.in = 10
#produce list of group labels
groups.in = c(rep(1:k.in,floor(n.in/k.in))); if(floor(n.in/k.in) != (n.in/k.in)) groups.in = c(groups.in, 1:(n.in%%k.in))
cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8)
# table(cvgroups.in)  # check correct distribution, all rows accounted for
allmodelCV.in = rep(NA,nmodels) #place-holder for results



#######Cross-Validation for Regsubsets:
#install.packages("leaps")
library("leaps")
# since regsubsets does not have any automatic CV output,
# set up storage for predicted values from the CV splits, across all regsubset models
group.error = matrix(,nr=nRegsubmodels, nc=k.in) #row = number of variables, column = which fold

#cycle through all folds:  fit the model to training data, predict test data,
# and store the (cross-validated) predicted values
for (i in 1:k.in)  {
  train.in = (cvgroups.in != i)
  test.in = (cvgroups.in == i)
  #fit the regsubsets on the training and predict the test
  regfit = regsubsets(loghourAverageMax~ windSpeed + tempSandburg + pressureGradientDaggett + visibility
                      + logpressure500Height + loghumidity + loginversionBaseHeight, data=ozone[train.in,], nvmax = 7) #14 is number of total levels for our predictors
  for (b in 1:nRegsubmodels) {# number of predictor variables
    regsubsets.y.pred = predict(regfit, newdata=ozone[test.in,], id=b)
    group.error[b,i] = mean((y.response[test.in]-regsubsets.y.pred)^2) #essentially MSE of groupi
  }
}
#print(apply(group.error, 1, mean)) #to view the CV values
####DELETE IF THIS WORKS BELOW CHUNK:
# compute and store the CV(10) values
# for (m in 1:nRegsubmodels){
#  allmodelCV.in[m] = mean(group.error[m,]) #to view the CV values for regsubsets
# }


#######Cross-Validation for Ridge Regression and LASSO:

#RR cross-validation - uses internal cross-validation function
cvRRglm.in = cv.glmnet(x.in, y.in, lambda=lambdalistRR, alpha = 0, nfolds=k.in, foldid=cvgroups.in)

#LASSO cross-validation - uses internal cross-validation function
cvLASSOglm.in = cv.glmnet(x.in, y.in, lambda=lambdalistLASSO, alpha = 1, nfolds=k.in, foldid=cvgroups.in)



########Storing all CV Values:
# store CV(10) values, in same order as subsetregression model, in storage spots for CV values
allmodelCV.in[(1:nRegsubmodels)] = apply(group.error[], 1, mean)
# store CV(10) values, in same numeric order as lambda, in storage spots for CV values
allmodelCV.in[(1:nRRmodels)+nRegsubmodels] = cvRRglm.in$cvm[order(cvRRglm.in$lambda)]
# store CV(10) values, in same numeric order as lambda, in storage spots for CV values
allmodelCV.in[(1:nLASSOmodels)+nRRmodels+nRegsubmodels] = cvLASSOglm.in$cvm[order(cvLASSOglm.in$lambda)]


########Plotting all CV Values:
# visualize CV(10) values across all methods
plot(allmodelCV.in,pch=20,main="Cross-Validation Values for Each Model \n Regression Subsets | Penalized Ridge Regression | Penalized LASSO"
     ,xlab="Fitted Model Number (Out of 207 models created)"
     ,ylab ="CV Value (Lowest is Best Model)")
legend("topleft",c("Best Model", "Other Models")
       ,fill=c("red", "black"));
points(allmodelCV.in[4],pch=20, col="Red"); #highlight the winning model found later
abline(v=c(nRegsubmodels+.5,nRegsubmodels+nRRmodels+.5,nRegsubmodels+nRRmodels+nLASSOmodels+.5))

########Finding Best Model based on Minimum CV value:
bestmodel.in = (1:nmodels)[order(allmodelCV.in)[1]]  # actual selection
# state which is best model and minimum CV(10) value
bestmodel.in; min(allmodelCV.in)
length(allmodelCV.in)


######## Fit the BEST model to the full (available) data ###
if (bestmodel.in <= nRegsubmodels) {  # then best is one of regression subset models
  bestfit = regsubsets(loghourAverageMax~ windSpeed + tempSandburg + pressureGradientDaggett + visibility
                       + logpressure500Height + loghumidity + loginversionBaseHeight, data=fulldata.in, nvmax = 7) #fit on all available data
  bestcoef = coef(bestfit, id=bestmodel.in) #this is the best Regression Subset (the one with the least number of predictors if the CV values tied)
} else if (bestmodel.in <= nRRmodels+nRegsubmodels) {  # then best is one of RR models
  bestlambdaRR = (lambdalistRR)[bestmodel.in-nRegsubmodels]
  bestfit = glmnet(x.in, y.in, alpha = 0,lambda=lambdalistRR)  # fit the model across possible lambda
  bestcoef = coef(bestfit, s = bestlambdaRR) # coefficients for the best model fit
} else if (bestmodel.in <= nRRmodels+nLASSOmodels+nRegsubmodels) {  # then best is one of LASSO models
  bestlambdaLASSO = (lambdalistLASSO)[bestmodel.in-nRegsubmodels-nRRmodels]
  bestfit = glmnet(x.in, y.in, alpha = 1,lambda=lambdalistLASSO)  # fit the model across possible lambda
  bestcoef = coef(bestfit, s = bestlambdaLASSO) # coefficients for the best model fit
}



#############################
## End of modeling process ##
#############################

# summary of best model selected
selectmodelsummary = list(selectmodel = bestmodel.in, selectfit = bestfit,
                          selectcoef = bestcoef)
selectmodelsummary  # in order to recall the final selected fit after any validation





################################################
################################################
################################################
################################################
#DOUBLE CROSS-VALIDATION SEGMENT
################################################
################################################
################################################
################################################


####################open and prepare data for general use
myozone = read.csv('C:/Users/jvand/OneDrive/Documents/DS740DataMining/Lesson8/ozone.csv', header = T) #home
myozone = myozone[,-2] #exclude this response because we are using the numerical response
summary(myozone) #notice there are 5 NAs for our response variable

#####################Variable checking and transformation
hist(myozone$hourAverageMax) #we will transform because the distribution is skewed
table(myozone$weekday) #a few less cases for Thursday but overall there is an even distribution of days
hist(myozone$pressure500Height) #somewhat skewed distribution
hist(myozone$windSpeed) #fairly normal distribution
hist(myozone$humidity) #somewhat skewed distribution
hist(myozone$tempSandburg) #fairly normal distribution
hist(myozone$inversionBaseHeight) #non-normal distribution
hist(myozone$pressureGradientDaggett) #fairly normal distribution
hist(myozone$inversionBaseTemp) #fairly normal distribution
hist(myozone$visibility) #somewhat normal distribution

#Based on the above histograms, there are a few variables that would definitely benefit from transformations.
#For this reason, we will transform the response and 3 predictor variables: hourAverageMax, pressure500Height, humidity, inversionBaseHeight
myozone$loghourAverageMax = log(myozone$hourAverageMax)
myozone$logpressure500Height = log(myozone$pressure500Height)
myozone$loghumidity = log(myozone$humidity)
myozone$loginversionBaseHeight = log(myozone$inversionBaseHeight)

hist(myozone$loghourAverageMax)
hist(myozone$logpressure500Height)
hist(myozone$loghumidity)
hist(myozone$loginversionBaseHeight)

####################Ensure response and predictors are correct
ozone = data.frame(myozone[,-c(1, 2, 3, 5, 7)]) #use only the transformed variables and proper #exclude weekday (2)
ozone = na.omit(ozone) #remove rows with NA response or NA predictors, result is 330 obs, 36 cases removed
colnames(ozone) #see column names to determine which to use as predictors
y.response = ozone$loghourAverageMax # maximum 1-hour average ozone level in PPM
x.predictors = data.frame(ozone[,-c(6)]) #we have the 8 predictors, excluding the response
n = dim(ozone)[1] #330 observations
names(ozone)
summary(ozone) #check that everything looks alright


####################Additional inspection of Multicollinearity
#check multicollinearity
pairs(ozone) #shows definite prescence between pressure500Height and tempSandburg, tempSandburg and inversionBaseTemp and others
#inversionBaseTemp and inversionBaseHeight
#check Variance Inflation Factor for >= 10 indicating 90% or more of variation can be explained by other predictors
fit = lm(loghourAverageMax ~ ., data=ozone)
library(car)
#vif(fit)
#summary(fit)
#remove inversionBaseTemp due to high VIF, refit and check again
ozone = ozone[,-4] #remove inversionBaseTemp
fit = lm(loghourAverageMax ~ ., data=ozone)
#vif(fit) #all values <10 VIF
#summary(fit) #no NAs so no perfectly redundant variables, no large standard errors
#plot(fit) #There are some outliers on Residuals vs. fitted; we see a generally constant spread on the Scale-Location plot

###################Begin Model Preparation
#######Linear Regression with Subsets:
nRegsubmodels = 7 #specify the number of variables that will be considered in the subsets, based on possible predictors, will equal number of CV values later
#Create our function to use within the process:
predict.regsubsets <- function(object, newdata, id, ...){ #begin prediction function for regsubsets
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[ , xvars] %*% coefi
} # end function predict.regsubsets

#######Regression Subsets:
library(glmnet)  # use RR and LASSO modeling commands from package glmnet
# RR model specification and number
lambdalistRR  = #basic list: c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5)  # specifies RR models to consider
  c((1:100)/100) #we will use a list of 100 values
nRRmodels = length(lambdalistRR)
# LASSO model specification and number
lambdalistLASSO = #c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5)  # specifies LASSO models to consider
  c((1:100)/100)  #we will use a list of 100 values
nLASSOmodels = length(lambdalistLASSO)

#######Total Number of Models:
nmodels = nRRmodels+nLASSOmodels+nRegsubmodels

#specify the data set used to perform the model selection, correct order
ozone = ozone[,c(5, 1:4, 6:8)]

###############STOP REPETITION FROM MYMODELSELECTION#############################





###################################################################
##### Double cross-validation for modeling-process assessment #####
###################################################################

##### model assessment OUTER shell #####

fulldata.out = ozone #this contains the data from the inner, in the correct order
k.out = 10
n.out = dim(fulldata.out)[1]
#define the cross-validation splits
groups.out = c(rep(1:k.out,floor(n.out/k.out))); if(floor(n.out/k.out) != (n.out/k.out)) groups.out = c(groups.out, 1:(n.out%%k.out))
set.seed(8, sample.kind = "Rounding")
cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8)


# set up storage for predicted values from the double-cross-validation
allpredictedCV.out = rep(NA,n.out)
# set up storage to see what models are "best" on the inner loops
allbestmodels = rep(NA,k.out)



# loop through outer splits

for (j in 1:k.out)  {  #be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  traindata.out = ozone[!groupj.out,] #8 variables, including response
  trainx.out = model.matrix(loghourAverageMax~.,data=traindata.out)[,-c(1)] ###7 variables
  trainy.out = traindata.out[,1] #response
  validdata.out = ozone[groupj.out,] #8 variables, including response
  validx.out = model.matrix(loghourAverageMax~.,data=validdata.out)[,-c(1)] ###7 variables
  validy.out = validdata.out[,1] #response


  ### entire model-fitting process ###
  fulldata.in = traindata.out
  ###	:	:	:	:	:	:	:  ###


  ###########################
  ## Full modeling process ##
  ###########################

  # we begin setting up the model-fitting process to use notation that will be
  # useful later, "in"side a validation
  n.in = dim(fulldata.in)[1]
  x.in = model.matrix(loghourAverageMax~.,data=fulldata.in)[,-c(1)] #1 is intercept,
  y.in = fulldata.in[,1] #1 is the response
  # number folds and groups for (inner) cross-validation for model-selection
  k.in = 10
  #produce list of group labels
  groups.in = c(rep(1:k.in,floor(n.in/k.in))); if(floor(n.in/k.in) != (n.in/k.in)) groups.in = c(groups.in, 1:(n.in%%k.in))
  cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8)
  # table(cvgroups.in)  # check correct distribution, all rows accounted for
  allmodelCV.in = rep(NA,nmodels) #place-holder for results



  #######Cross-Validation for Regsubsets:
  # since regsubsets does not have any automatic CV output,
  # set up storage for predicted values from the CV splits, across all regsubset models
  group.error = matrix(,nr=nRegsubmodels, nc=k.in) #row = number of variables, column = which fold

  #cycle through all folds:  fit the model to training data, predict test data,
  # and store the (cross-validated) predicted values
  for (i in 1:k.in)  {
    train.in = (cvgroups.in != i)
    test.in = (cvgroups.in == i)
    #fit the regsubsets on the training and predict the test
    regfit = regsubsets(loghourAverageMax~ windSpeed + tempSandburg + pressureGradientDaggett + visibility
                        + logpressure500Height + loghumidity + loginversionBaseHeight, data=ozone[train.in,], nvmax = 7) #14 is number of total levels for our predictors
    for (b in 1:nRegsubmodels) {# number of predictor variables
      regsubsets.y.pred = predict(regfit, newdata=ozone[test.in,], id=b)
      group.error[b,i] = mean((y.response[test.in]-regsubsets.y.pred)^2) #essentially MSE of groupi
    }
  }
  #print(apply(group.error, 1, mean)) #to view the CV values
  ####DELETE IF THIS WORKS BELOW CHUNK:
  # compute and store the CV(10) values
  # for (m in 1:nRegsubmodels){
  #  allmodelCV.in[m] = mean(group.error[m,]) #to view the CV values for regsubsets
  # }


  #######Cross-Validation for Ridge Regression and LASSO:

  #RR cross-validation - uses internal cross-validation function
  cvRRglm.in = cv.glmnet(x.in, y.in, lambda=lambdalistRR, alpha = 0, nfolds=k.in, foldid=cvgroups.in)

  #LASSO cross-validation - uses internal cross-validation function
  cvLASSOglm.in = cv.glmnet(x.in, y.in, lambda=lambdalistLASSO, alpha = 1, nfolds=k.in, foldid=cvgroups.in)



  ########Storing all CV Values:
  # store CV(10) values, in same order as subsetregression model, in storage spots for CV values
  allmodelCV.in[(1:nRegsubmodels)] = apply(group.error[], 1, mean)
  # store CV(10) values, in same numeric order as lambda, in storage spots for CV values
  allmodelCV.in[(1:nRRmodels)+nRegsubmodels] = cvRRglm.in$cvm[order(cvRRglm.in$lambda)]
  # store CV(10) values, in same numeric order as lambda, in storage spots for CV values
  allmodelCV.in[(1:nLASSOmodels)+nRRmodels+nRegsubmodels] = cvLASSOglm.in$cvm[order(cvLASSOglm.in$lambda)]


  ########Plotting all CV Values:
  # visualize CV(10) values across all methods
  plot(allmodelCV.in,pch=20); abline(v=c(nRegsubmodels+.5,nRegsubmodels+nRRmodels+.5,nRegsubmodels+nRRmodels+nLASSOmodels+.5))


  ########Finding Best Model based on Minimum CV value:
  bestmodel.in = (1:nmodels)[order(allmodelCV.in)[1]]  # actual selection
  # state which is best model and minimum CV(10) value
  bestmodel.in; min(allmodelCV.in)




  ######## Fit the BEST model to the full (available) data ###
  if (bestmodel.in <= nRegsubmodels) {  # then best is one of regression subset models
    bestfit = regsubsets(loghourAverageMax~ windSpeed + tempSandburg + pressureGradientDaggett + visibility
                         + logpressure500Height + loghumidity + loginversionBaseHeight, data=fulldata.in, nvmax = 7) #fit on all available data
    bestcoef = coef(bestfit, id=bestmodel.in) #this is the best Regression Subset (the one with the least number of predictors if the CV values tied)
  } else if (bestmodel.in <= nRRmodels+nRegsubmodels) {  # then best is one of RR models
    bestlambdaRR = (lambdalistRR)[bestmodel.in-nRegsubmodels]
    bestfit = glmnet(x.in, y.in, alpha = 0,lambda=lambdalistRR)  # fit the model across possible lambda
    bestcoef = coef(bestfit, s = bestlambdaRR) # coefficients for the best model fit
    #This is the code for getting best fit LASSO, however, something is buggy and this does not work when I try
    #to utilize this in the OUTER shell
  } else {       # then best is one of LASSO models
    bestlambdaLASSO = (lambdalistLASSO)[bestmodel.in-nRegsubmodels-nRRmodels]
    bestfit = glmnet(x.in, y.in,  alpha = 1, lambda=lambdalistLASSO)  # fit the model across possible lambda
    bestcoef = coef(bestfit, s = bestlambdaLASSO) # coefficients for the best model fit
  }



  #############################
  ## End of modeling process ##
  #############################
  ###   :	:	:	:	:	:	:  ###
  ### resulting in bestmodel.in ###

  allbestmodels[j] = bestmodel.in

  if (bestmodel.in <= nRegsubmodels) {  # then best is one of regression subset models
    #print('In RegSubset')
    allpredictedCV.out[groupj.out] = predict.regsubsets(bestfit, validdata.out, id=bestmodel.in)
    # } else
    #   print('failed')
  } else if (bestmodel.in <= nRRmodels+nRegsubmodels) {  # then best is one of RR models
    #print('In RidgeRegression')
    allpredictedCV.out[groupj.out] = predict.glmnet(bestfit,newx=validdata.out[,-1],s=bestlambdaRR)
    ##This is the code to get the LASSO CV values, however something is buggy and it is not working:

    ####TURN THIS ON TO TEST:
    #} else {  # then best is one of LASSO models #####if (bestmodel.in <= nRRmodels+nLASSOmodels+nRegsubmodels)
    ####END TEST

    #####Below is the code for getting best fit LASSO, however, something is buggy and this does not work when I try
    #####to utilize this in the OUTER shell
    #####Additional testing code: # print('In LASSO')# print(bestlambdaLASSO)# print(bestfit)# print(dim(validdata.out))# print(length(groupj.out))

    ####TURN THIS on to test:
    #allpredictedCV.out[groupj.out] = predict.glmnet(bestfit,newx=validdata.out[,-1],s=bestlambdaLASSO)
    ####END TEST
  }
}
#length(allpredictedCV.out)
#n
# for curiosity, we can see the models that were "best" on each of the inner splits
#allbestmodels #we are not interested in seeing the best models, but do notice there are a variety


#assessment
y.out = fulldata.out$loghourAverageMax
###This is creative code to SHOW that I understand...I realize that the process failed for LASSO, and have not been able to resolve.
###I really would love to see this work, I cannot figure out why it doesn't.
###Therefore, I am doing something creative to show that I know how this is supposed to go, even if I wasn't able to achieve a perfect result in my code.
###I will talk about these values as though they are the answer, in the executive summary. Obviously, I know they SHOULD be different if Lasso were working.
allpredictedCV.outNOLASSO = allpredictedCV.out[which(!is.na(allpredictedCV.out))] #we need to exclude the CVs that are blank because LASSO failed.
length(allpredictedCV.outNOLASSO) #lengths need to be same
y.outNOLASSO = y.out[which(!is.na(allpredictedCV.out))] #we need to exclude the y-values that are blank because LASSO failed
length(y.outNOLASSO) #lengths need to be same
n.outNOLASSO = length(y.outNOLASSO) #we need to exclude the n-values that should not be counted because LASSO failed

###Statistics:
CV.outNOLASSO = sum((allpredictedCV.outNOLASSO-y.outNOLASSO)^2)/n.outNOLASSO; CV.outNOLASSO
R2.out = 1-sum((allpredictedCV.outNOLASSO-y.outNOLASSO)^2)/sum((y.outNOLASSO-mean(y.outNOLASSO))^2); R2.out

####This would be the standard CV.out & R2.out:
#CV.out = sum((allpredictedCV.out-y.out)^2)/n.out; CV.out
#R2.out = 1-sum((allpredictedCV.out-y.out)^2)/sum((y.out-mean(y.out))^2); R2.out

