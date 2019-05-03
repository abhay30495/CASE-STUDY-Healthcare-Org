##Question 4
> View(diabetes)
> View(diabetes_1)
> diabetes_1=diabetes[c(1:133),c(3,8,9)]
> View(diabetes_1)
> library(caTools)
> data=sample.split(diabetes_1,SplitRatio = 0.8)
> train=subset(diabetes_1,data=="TRUE")
##Warning message:
  ##Length of logical index must be 1 or 133, not 3 
> test=subset(diabetes_1,data=="FALSE")
##Warning message:
  ##Length of logical index must be 1 or 133, not 3 
> View(train)
> View(test)
##############################################################################################
##Checking consistency for both the variable,  FBS & PPBS1 
> library(caret)
> model=glm(NDD~.,train, family="binomial")
> model

Call:  glm(formula = NDD ~ ., family = "binomial", data = train)

Coefficients:
  (Intercept)          FBS        PPBS1  
-24.7263       0.1157       0.0700  

Degrees of Freedom: 87 Total (i.e. Null);  85 Residual
(1 observation deleted due to missingness)
Null Deviance:	    106.8 
Residual Deviance: 16.56 	AIC: 22.56
> prediction=predict(model, test, type="response")
> prediction
> table(test$NDD,prediction>0.5)

FALSE TRUE
0    12    4
1     0   28
> (12+28)/(12+28+4)
[1] 0.9090909
##############################################################################################
##Checking consistency for FBS variable
> View(train)
> View(test)
> model_1=glm(NDD~FBS,train, family = "binomial")
Warning message:
  glm.fit: fitted probabilities numerically 0 or 1 occurred 
> model_1

Call:  glm(formula = NDD ~ FBS, family = "binomial", data = train)

Coefficients:
  (Intercept)          FBS  
-12.7891       0.1195  

Degrees of Freedom: 88 Total (i.e. Null);  87 Residual
Null Deviance:	    107.5 
Residual Deviance: 46.36 	AIC: 50.36
> prediction=predict(model_1, test, type="response")
> prediction
> table(test$NDD,prediction>0.5)

FALSE TRUE
0     8    8
1     2   26
> (8+26)/(8+26+2+8)
[1] 0.7727273 ## lower accuracy from the previous case
##############################################################################################
##Checking consistency for PPBS1 Variable 
> model_2=glm(NDD~PPBS1,train, family = "binomial")
##Warning message:
  ##glm.fit: fitted probabilities numerically 0 or 1 occurred 
> model_2

Call:  glm(formula = NDD ~ PPBS1, family = "binomial", data = train)

Coefficients:
  (Intercept)        PPBS1  
-11.40871      0.06929  

Degrees of Freedom: 87 Total (i.e. Null);  86 Residual
(1 observation deleted due to missingness)
Null Deviance:	    106.8 
Residual Deviance: 28.57 	AIC: 32.57
> prediction=predict(model_2, test, type="response")
> table(test$NDD,prediction>0.5)

FALSE TRUE
0    13    3
1     1   27
> (13+27)/(13+3+1+27)
[1] 0.9090909 
## PPBS1 is having the highest accuracy of all the above mentioned ways.
##Hence, PPBS1 is alone capable of predicting the diabetes.


