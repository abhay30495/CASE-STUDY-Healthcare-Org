##Question 1
> setwd("C:/Users/Abhay Dubey/Desktop/UTW@Study/DS/CASE STUDY")
> library(readxl)
> diabetes=read_xlsx("diabetes.xlsx")
> View(diabetes)
> View(diabetes)
> diabetes=read_xlsx("diabetes.xlsx",sheet = "Data")
> View(diabetes)
> diabetes=diabetes[,-1]
> View(diabetes)
> diabetes_1=diabetes[,c(1,2,9)] ##making new table only for reqd variables 
> View(diabetes_1)
> library(caTools)
> data=sample.split(diabetes_1, SplitRatio = 0.8)
> train=subset(diabetes_1,data=="TRUE")
##Warning message:
##Length of logical index must be 1 or 921, not 3 
> test=subset(diabetes_1, data=="FALSE")
##Warning message:
##Length of logical index must be 1 or 921, not 3 
> train
> test
> library(caret)
##Loading required package: lattice
##Loading required package: ggplot2
> logisfit=glm(NDD~.,train, family = "binomial")
> summary(logisfit)

Call:
glm(formula = NDD ~ ., family = "binomial", data = train)

Deviance Residuals: 
Min       1Q   Median       3Q      Max  
-1.5260  -1.0449  -0.8309   1.2379   1.7657  

Coefficients:
Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.933696   0.370033  -5.226 1.73e-07 *** ##AGE is more prominent than SEX 
AGE          0.029098   0.007295   3.989 6.64e-05 ***
SEX          0.425077   0.168120   2.528   0.0115 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 839.10  on 613  degrees of freedom
Residual deviance: 812.58  on 611  degrees of freedom
AIC: 818.58

Number of Fisher Scoring iterations: 4

> prediction=predict(logisfit,test,type="response")
> summary(prediction)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.2056  0.3488  0.4174  0.4253  0.4894  0.7123 
##confusion matrix 
> table(test$NDD, PredictedValue=prediction>0.5)
PredictedValue
FALSE TRUE
0   147   32
1    94   34
> prediction
> accuracy=(147+34)/(147+94+32+34)
> accuracy
[1] 0.5895765
#Less accurate, we then go for graph plot to get cutoff
> prediction1=predict(logisfit,train,type="response")
> library(ROCR)
> ROCRpred=prediction(prediction1, train$NDD)
> ROCRperf=performance(ROCRpred,"tpr","fpr")
> plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
> > table(test$NDD, PredictedValue=prediction>0.6)
PredictedValue
FALSE TRUE
0   170    9
1   120    8
> table(test$NDD, PredictedValue=prediction>0.52)
PredictedValue
FALSE TRUE
0   154   25
1    97   31
> table(test$NDD, PredictedValue=prediction>0.4)
PredictedValue
FALSE TRUE
0   101   78
1    35   93  ##0.4 as the cutoff seems to be a good value
> (101+93)/(101+78+35+93)
[1] 0.6319218