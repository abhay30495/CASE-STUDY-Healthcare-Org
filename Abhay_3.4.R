## Question 3
> View(diabetes)
> diabetes_1=diabetes[,c(9,1,2,10,11,12,13,14)] ##Making different table for the below operation
> plot(diabetes_1,main="pairwise scatter plot") ##Pair wise scatter plot
> round(cor(diabetes_1),3)
NDD   AGE   SEX Height Weight BMI HC WHR
NDD    1.000 0.208 0.120     NA     NA  NA NA  NA ##Age is strongly co-related
AGE    0.208 1.000 0.126     NA     NA  NA NA  NA
SEX    0.120 0.126 1.000     NA     NA  NA NA  NA
Height    NA    NA    NA      1     NA  NA NA  NA
Weight    NA    NA    NA     NA      1  NA NA  NA
BMI       NA    NA    NA     NA     NA   1 NA  NA
HC        NA    NA    NA     NA     NA  NA  1  NA
WHR       NA    NA    NA     NA     NA  NA NA   1
> model=lm(NDD~.,data = diabetes_1) ##forming linear model
> model

Call:
  lm(formula = NDD ~ ., data = diabetes_1)

Coefficients:
  (Intercept)          AGE          SEX       Height       Weight          BMI           HC  
-2.086323     0.007247     0.018474     0.012324    -0.008321     0.007888     0.002353  
WHR  
0.307130  

> summary(model)

Call:
  lm(formula = NDD ~ ., data = diabetes_1)

Residuals:
  Min      1Q  Median      3Q     Max 
-0.7600 -0.4008 -0.2485  0.5295  0.9486 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2.086323   1.872833  -1.114    0.266    
AGE          0.007247   0.001375   5.270 1.72e-07 *** ##significant   
  SEX          0.018474   0.036968   0.500    0.617    
Height       0.012324   0.011893   1.036    0.300    
Weight      -0.008321   0.013606  -0.612    0.541    
BMI          0.007888   0.034727   0.227    0.820    
HC           0.002353   0.001877   1.254    0.210    
WHR          0.307130   0.214785   1.430    0.153    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.4768 on 871 degrees of freedom
(42 observations deleted due to missingness)
Multiple R-squared:  0.06537,	Adjusted R-squared:  0.05786 
F-statistic: 8.703 on 7 and 871 DF,  p-value: 2.332e-10
## from summary we see age is strongly related to NDD, and no other variables are significant
#############################################################################################
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


