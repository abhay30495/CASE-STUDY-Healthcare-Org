##Question 2
> View(diabetes)
> diabetes_1=diabetes[c(133:921),c(5,6,9)] ##Only considering the available data. 
##Also, it is advisable to take data that are already present.
> View(diabetes_1)
> library(caTools)
> data=sample.split(diabetes,SplitRatio = 0.8)
> train=subset(diabetes_1, data=="TRUE")
##Warning message:
 ##Length of logical index must be 1 or 789, not 15 
> test=subset(diabetes_1,data=="FALSE")
##Warning message:
  ##Length of logical index must be 1 or 789, not 15 
> library(caret)
> model=glm(NDD~HBA1C1, train, family="binomial")
> model
> prediction_1=predict(model, test, type="response")
> summary(prediction_1)
> model
> table(test$NDD, prediction_1>0.5)

FALSE TRUE
0    84    8
1    24   41
> (84+41)/(84+8+24+41)
[1] 0.7961783 ##Accuracy

## for the second part we will re-run the logistic regression for OGTT1FBS
> View(diabetes_1) ##checking file
> View(train)
> View(test)
> library(caret)
> model_1=glm(NDD~OGTT1FBS, train, family="binomial")
##Warning message:
  ##glm.fit: fitted probabilities numerically 0 or 1 occurred 
> model_1
Call:  glm(formula = NDD ~ OGTT1FBS, family = "binomial", data = train)

Coefficients:
  (Intercept)     OGTT1FBS  
-14.8361       0.1291  

Degrees of Freedom: 631 Total (i.e. Null);  630 Residual
Null Deviance:	    836.2 
Residual Deviance: 301.4 	AIC: 305.4
> prediction_2=predict(model_1,test,type="response")
> prediction_2
> table(test$NDD,prediction_2>0.5)

FALSE TRUE
0    82   10
1    10   55
> (82+55)/(82+10+10+55)
[1] 0.8726115 ##Accuracy 
## Hence on comparing the accuracy we get to know OGTT1FBS cant be replaced with HBA1C1