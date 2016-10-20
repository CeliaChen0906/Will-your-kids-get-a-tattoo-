
#Initial Models

stopifnot(require(ISLR))
## Loading required package: ISLR
set.seed(123)
vars <- c("tattoo", "age", "paedu", "lnincome", "outdoorsports", "activesports", "exercise", "motor", "nightout", "cigapuff", "fsmoking", "fdrinking", "fweed", "gut", "Inde.s", "atten.s", "acap")
sub <- d[ , vars]
sub <- na.omit(sub)
indexes <- sample(1 : nrow(sub), size = 0.2 * nrow(sub))
testing <- sub[indexes, ]
training <- sub[-indexes, ]
Logistic Regression

logit1 <- glm(tattoo ~ age + paedu + I(lnincome) + as.factor(outdoorsports) + as.factor(activesports) + as.factor(exercise) + as.factor(motor) + nightout + cigapuff + fsmoking + fdrinking + fweed + gut + I(gut ^ 2) + Inde.s + atten.s + acap + paedu * I(lnincome) + fsmoking * cigapuff + paedu * atten.s + fsmoking * fdrinking, data = training, na.action = na.omit, family = binomial(link = "logit"))
summary(logit1)
## 
## Call:
## glm(formula = tattoo ~ age + paedu + I(lnincome) + as.factor(outdoorsports) + 
##     as.factor(activesports) + as.factor(exercise) + as.factor(motor) + 
##     nightout + cigapuff + fsmoking + fdrinking + fweed + gut + 
##     I(gut^2) + Inde.s + atten.s + acap + paedu * I(lnincome) + 
##     fsmoking * cigapuff + paedu * atten.s + fsmoking * fdrinking, 
##     family = binomial(link = "logit"), data = training, na.action = na.omit)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4267  -0.2829  -0.1518  -0.0759   3.3371  
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -8.8072268  1.6523048  -5.330 9.81e-08 ***
## age                        0.2554761  0.0660998   3.865 0.000111 ***
## paedu                     -0.1106800  0.1600953  -0.691 0.489353    
## I(lnincome)               -0.1903988  0.2456738  -0.775 0.438336    
## as.factor(outdoorsports)1  0.1739895  0.2401277   0.725 0.468715    
## as.factor(outdoorsports)2 -0.9507847  0.5326027  -1.785 0.074234 .  
## as.factor(outdoorsports)3  0.2765952  0.3605658   0.767 0.443014    
## as.factor(activesports)1  -0.0003816  0.2176909  -0.002 0.998601    
## as.factor(activesports)2  -0.3079070  0.2768441  -1.112 0.266050    
## as.factor(activesports)3  -0.5364958  0.2837532  -1.891 0.058663 .  
## as.factor(exercise)1      -0.0791787  0.2572299  -0.308 0.758225    
## as.factor(exercise)2       0.2472293  0.2677819   0.923 0.355878    
## as.factor(exercise)3       0.3153340  0.2724598   1.157 0.247125    
## as.factor(motor)1          0.3474411  0.2321053   1.497 0.134416    
## as.factor(motor)2          1.2150502  0.3254781   3.733 0.000189 ***
## as.factor(motor)3          0.5100692  0.4427682   1.152 0.249321    
## as.factor(motor)4          0.0371925  0.5111145   0.073 0.941991    
## nightout                   0.6502262  0.1952972   3.329 0.000870 ***
## cigapuff                   1.4220128  0.4299916   3.307 0.000943 ***
## fsmoking                   0.6554125  0.2523744   2.597 0.009405 ** 
## fdrinking                  0.1629163  0.1439288   1.132 0.257667    
## fweed                      0.2137921  0.0894173   2.391 0.016805 *  
## gut                       -0.1130148  0.4585433  -0.246 0.805322    
## I(gut^2)                   0.0228708  0.0724767   0.316 0.752336    
## Inde.s                     0.0626451  0.0955909   0.655 0.512245    
## atten.s                   -0.0496061  0.2449627  -0.203 0.839522    
## acap                      -0.2240263  0.0937483  -2.390 0.016864 *  
## paedu:I(lnincome)          0.0112545  0.0443534   0.254 0.799691    
## cigapuff:fsmoking         -0.2482842  0.2470217  -1.005 0.314843    
## paedu:atten.s             -0.0431505  0.0426631  -1.011 0.311812    
## fsmoking:fdrinking        -0.0800430  0.0695317  -1.151 0.249662    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1263.9  on 3724  degrees of freedom
## Residual deviance:  985.3  on 3694  degrees of freedom
## AIC: 1047.3
## 
## Number of Fisher Scoring iterations: 8
logit2 <- glm(tattoo ~ age + paedu + I(lnincome) + as.factor(outdoorsports) + as.factor(activesports) + as.factor(exercise) + as.factor(motor) + nightout + cigapuff + fsmoking + fdrinking + fweed + gut + I(gut ^ 2) + Inde.s + atten.s + acap, data = training, family = binomial(link = "logit"))
summary(logit2)
## 
## Call:
## glm(formula = tattoo ~ age + paedu + I(lnincome) + as.factor(outdoorsports) + 
##     as.factor(activesports) + as.factor(exercise) + as.factor(motor) + 
##     nightout + cigapuff + fsmoking + fdrinking + fweed + gut + 
##     I(gut^2) + Inde.s + atten.s + acap, family = binomial(link = "logit"), 
##     data = training)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4784  -0.2775  -0.1548  -0.0865   3.3067  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -9.038579   1.478182  -6.115 9.68e-10 ***
## age                        0.268714   0.065657   4.093 4.26e-05 ***
## paedu                     -0.055260   0.043737  -1.263 0.206429    
## I(lnincome)               -0.137909   0.113674  -1.213 0.225054    
## as.factor(outdoorsports)1  0.203085   0.239822   0.847 0.397099    
## as.factor(outdoorsports)2 -0.925616   0.532040  -1.740 0.081903 .  
## as.factor(outdoorsports)3  0.320678   0.358922   0.893 0.371618    
## as.factor(activesports)1  -0.008785   0.217835  -0.040 0.967832    
## as.factor(activesports)2  -0.330953   0.275764  -1.200 0.230088    
## as.factor(activesports)3  -0.527745   0.283859  -1.859 0.063002 .  
## as.factor(exercise)1      -0.068161   0.257393  -0.265 0.791154    
## as.factor(exercise)2       0.274309   0.267775   1.024 0.305646    
## as.factor(exercise)3       0.322625   0.271843   1.187 0.235304    
## as.factor(motor)1          0.357016   0.232255   1.537 0.124251    
## as.factor(motor)2          1.251417   0.324808   3.853 0.000117 ***
## as.factor(motor)3          0.489168   0.445509   1.098 0.272206    
## as.factor(motor)4          0.044970   0.514205   0.087 0.930310    
## nightout                   0.654359   0.195790   3.342 0.000831 ***
## cigapuff                   1.186643   0.296545   4.002 6.29e-05 ***
## fsmoking                   0.292267   0.088152   3.315 0.000915 ***
## fdrinking                  0.046312   0.094085   0.492 0.622551    
## fweed                      0.206235   0.089500   2.304 0.021206 *  
## gut                       -0.057176   0.457328  -0.125 0.900506    
## I(gut^2)                   0.014845   0.072406   0.205 0.837549    
## Inde.s                     0.057303   0.095487   0.600 0.548430    
## atten.s                   -0.268238   0.106377  -2.522 0.011683 *  
## acap                      -0.219187   0.093359  -2.348 0.018885 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1263.87  on 3724  degrees of freedom
## Residual deviance:  989.04  on 3698  degrees of freedom
## AIC: 1043
## 
## Number of Fisher Scoring iterations: 7
y_hat_logit1 <- predict(logit1, newdata = testing, type = "response")
summary(y_hat_logit1)
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0001539 0.0040800 0.0152100 0.0428500 0.0458900 0.8160000
z_logit1 <- as.integer(y_hat_logit1 > 0.25)
table(testing$tattoo, z_logit1) # OMG MY MODEL SUCKS!!!!!! But after a few shots it looks better now
##    z_logit1
##       0   1
##   0 874  22
##   1  29   6
(875 + 9) / (875 + 17 + 30 + 9)
## [1] 0.9495166

# Optimization

rgenoud

X <- model.matrix(logit2)
y <- training$tattoo 

ll <- function(beta) {
  eta <- X %*% beta
  p <- 1 / (1 + exp(-eta))
  return(sum(dbinom(y, size = 1, prob = p, log = TRUE)))
}

stopifnot(require(rgenoud))
## Loading required package: rgenoud
## ##  rgenoud (Version 5.7-12.4, Build Date: 2015-07-19)
## ##  See http://sekhon.berkeley.edu/rgenoud for additional documentation.
## ##  Please cite software as:
## ##   Walter Mebane, Jr. and Jasjeet S. Sekhon. 2011.
## ##   ``Genetic Optimization Using Derivatives: The rgenoud package for R.''
## ##   Journal of Statistical Software, 42(11): 1-26. 
## ##
opt2 <- genoud(fn = ll, nvars = ncol(X), max = TRUE)

X_testing <- model.matrix(tattoo ~ age + paedu + I(lnincome) + as.factor(outdoorsports) + as.factor(activesports) + as.factor(exercise) + as.factor(motor) + nightout + cigapuff + fsmoking + fdrinking + fweed + gut + I(gut ^ 2) + Inde.s + atten.s + acap, data = testing, family = binomial(link = "logit"))

y_hat_logit2 <- X_testing %*% opt2$par
summary(y_hat_logit2)
##        V1        
##  Min.   :-8.350  
##  1st Qu.:-5.337  
##  Median :-4.154  
##  Mean   :-4.136  
##  3rd Qu.:-3.065  
##  Max.   : 1.663
stopifnot(require(RSNNS))
y_hat_logit2 <- normalizeData(y_hat_logit2, type = "0_1")
summary(y_hat_logit2)
##        V1        
##  Min.   :0.0000  
##  1st Qu.:0.3009  
##  Median :0.4190  
##  Mean   :0.4209  
##  3rd Qu.:0.5278  
##  Max.   :1.0000
z_logit2 <- as.integer(y_hat_logit2 > 0.65)
table(testing$tattoo, z_logit2) # THIS IS JUST... BAD...
##    z_logit2
##       0   1
##   0 839  57
##   1  23  12
(823 + 18) / (823 + 18 + 69 + 21)
## [1] 0.9033298
glmpath

stopifnot(require(glmpath))
## Loading required package: glmpath
## Loading required package: survival
path1 <- glmpath(X, y, family = binomial)
summary(path1)
## Call:
## glmpath(x = X, y = y, family = binomial)
##         Df  Deviance      AIC      BIC
## Step 1   1 1263.8683 1265.868 1272.091
## Step 3   2 1221.0453 1225.045 1237.491
## Step 4   3 1219.4166 1225.417 1244.085
## Step 5   4 1160.8296 1168.830 1193.721
## Step 6   5 1140.8771 1150.877 1181.991
## Step 7   6 1138.7913 1150.791 1188.128
## Step 8   7 1136.8173 1150.817 1194.377
## Step 9   8 1073.6389 1089.639 1139.421
## Step 10  9 1025.5297 1043.530 1099.535
## Step 11 10 1025.0751 1045.075 1107.303
## Step 12 11 1024.0074 1046.007 1114.458
## Step 13 12 1018.9021 1042.902 1117.576
## Step 14 13 1017.0446 1043.045 1123.941
## Step 15 14 1012.7258 1040.726 1127.845
## Step 17 15 1008.7895 1038.790 1132.132
## Step 19 16 1008.0198 1040.020 1139.585
## Step 20 17 1002.1895 1036.189 1141.977
## Step 21 18  998.2651 1034.265 1146.276
## Step 22 19  997.7309 1035.731 1153.965
## Step 23 20  997.3766 1037.377 1161.833
## Step 24 21  997.3046 1039.305 1169.984
## Step 26 22  997.0759 1041.076 1177.978
## Step 27 23  994.6726 1040.673 1183.798
## Step 28 24  989.5119 1037.512 1186.860
## Step 29 25  989.0600 1039.060 1194.631
## Step 31 26  989.0548 1041.055 1202.848
## Step 32 27  989.0357 1043.036 1211.052
y_hat_path1 <- predict(path1, newx = X_testing, type = "response", s = 30) #AIC is the smallest here
y_hat_path1_n <- normalizeData(y_hat_path1, type = "0_1")
summary(y_hat_path1_n)
##        V1          
##  Min.   :0.000000  
##  1st Qu.:0.005461  
##  Median :0.018279  
##  Mean   :0.051021  
##  3rd Qu.:0.053120  
##  Max.   :1.000000
z_path1 <- as.integer(y_hat_path1_n > 0.4)
table(testing$tattoo, z_path1)
##    z_path1
##       0   1
##   0 886  10
##   1  30   5
(882 + 7) / (882 + 7 + 10 + 32)
## [1] 0.9548872
y_hat_path2 <- predict(path1, newx = X_testing, type = "response", s = 10) #BIC is the smallest here
y_hat_path2_n <- normalizeData(y_hat_path2, type = "0_1")
summary(y_hat_path2_n)
##        V1         
##  Min.   :0.00000  
##  1st Qu.:0.01384  
##  Median :0.03596  
##  Mean   :0.06689  
##  3rd Qu.:0.08259  
##  Max.   :1.00000
z_path2 <- as.integer(y_hat_path2_n > 0.45)
table(testing$tattoo, z_path2)
##    z_path2
##       0   1
##   0 890   6
##   1  31   4
(869 + 10) / (869 + 23 + 29 + 10)
## [1] 0.9441461
BartMachine

stopifnot(require(bartMachine))
## Loading required package: bartMachine
## Loading required package: rJava
## Loading required package: car
## Warning: package 'car' was built under R version 3.2.3
## 
## Attaching package: 'car'
## 
## The following object is masked from 'package:psych':
## 
##     logit
## 
## Loading required package: randomForest
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:psych':
## 
##     outlier
## 
## Loading required package: missForest
## Loading required package: foreach
## Loading required package: itertools
## Loading required package: iterators
## Welcome to bartMachine v1.2.0! You have 0.53GB memory available.
set_bart_machine_num_cores(parallel::detectCores())
## bartMachine now using 4 cores.
X <- data.frame(X)
bart <- bartMachine(X = X[, -1], y = y, mem_cache_for_speed = FALSE)
## bartMachine initializing with 50 trees...
## Now building bartMachine for regression ...
## evaluating in sample data...done
bart
## bartMachine v1.2.0 for regression
## 
## training data n = 3725 and p = 26 
## built in 14.5 secs on 4 cores, 50 trees, 250 burn-in and 1000 post. samples
## 
## sigsq est for y beforehand: 0.036 
## avg sigsq estimate after burn-in: 0.03321 
## 
## in-sample statistics:
##  L1 = 258.18 
##  L2 = 120.11 
##  rmse = 0.18 
##  Pseudo-Rsq = 0.171
## p-val for shapiro-wilk test of normality of residuals: 0 
## p-val for zero-mean noise: 0.63793
X_testing <- data.frame(X_testing)
y_hat_bart <- bart_predict_for_test_data(bart_machine = bart, Xtest = X_testing[, -1], ytest = testing$tattoo )
summary(y_hat_bart$y_hat)
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -0.044710  0.001837  0.018600  0.042390  0.057260  0.564900
y_hat_bart_n <- normalizeData(y_hat_bart$y_hat, type = "0_1")
summary(y_hat_bart_n)
##        V1         
##  Min.   :0.00000  
##  1st Qu.:0.07636  
##  Median :0.10386  
##  Mean   :0.14288  
##  3rd Qu.:0.16727  
##  Max.   :1.00000
z_bart <- as.integer(y_hat_bart$y_hat > 0.25)
table(testing$tattoo, z_bart)
##    z_bart
##       0   1
##   0 884  12
##   1  29   6
(882 + 4) / (882 + 4 + 10 + 35)
## [1] 0.9516649
Final Models
summary(logit2)
## 
## Call:
## glm(formula = tattoo ~ age + paedu + I(lnincome) + as.factor(outdoorsports) + 
##     as.factor(activesports) + as.factor(exercise) + as.factor(motor) + 
##     nightout + cigapuff + fsmoking + fdrinking + fweed + gut + 
##     I(gut^2) + Inde.s + atten.s + acap, family = binomial(link = "logit"), 
##     data = training)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4784  -0.2775  -0.1548  -0.0865   3.3067  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -9.038579   1.478182  -6.115 9.68e-10 ***
## age                        0.268714   0.065657   4.093 4.26e-05 ***
## paedu                     -0.055260   0.043737  -1.263 0.206429    
## I(lnincome)               -0.137909   0.113674  -1.213 0.225054    
## as.factor(outdoorsports)1  0.203085   0.239822   0.847 0.397099    
## as.factor(outdoorsports)2 -0.925616   0.532040  -1.740 0.081903 .  
## as.factor(outdoorsports)3  0.320678   0.358922   0.893 0.371618    
## as.factor(activesports)1  -0.008785   0.217835  -0.040 0.967832    
## as.factor(activesports)2  -0.330953   0.275764  -1.200 0.230088    
## as.factor(activesports)3  -0.527745   0.283859  -1.859 0.063002 .  
## as.factor(exercise)1      -0.068161   0.257393  -0.265 0.791154    
## as.factor(exercise)2       0.274309   0.267775   1.024 0.305646    
## as.factor(exercise)3       0.322625   0.271843   1.187 0.235304    
## as.factor(motor)1          0.357016   0.232255   1.537 0.124251    
## as.factor(motor)2          1.251417   0.324808   3.853 0.000117 ***
## as.factor(motor)3          0.489168   0.445509   1.098 0.272206    
## as.factor(motor)4          0.044970   0.514205   0.087 0.930310    
## nightout                   0.654359   0.195790   3.342 0.000831 ***
## cigapuff                   1.186643   0.296545   4.002 6.29e-05 ***
## fsmoking                   0.292267   0.088152   3.315 0.000915 ***
## fdrinking                  0.046312   0.094085   0.492 0.622551    
## fweed                      0.206235   0.089500   2.304 0.021206 *  
## gut                       -0.057176   0.457328  -0.125 0.900506    
## I(gut^2)                   0.014845   0.072406   0.205 0.837549    
## Inde.s                     0.057303   0.095487   0.600 0.548430    
## atten.s                   -0.268238   0.106377  -2.522 0.011683 *  
## acap                      -0.219187   0.093359  -2.348 0.018885 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1263.87  on 3724  degrees of freedom
## Residual deviance:  989.04  on 3698  degrees of freedom
## AIC: 1043
## 
## Number of Fisher Scoring iterations: 7
table(testing$tattoo, z_path1)
##    z_path1
##       0   1
##   0 886  10
##   1  30   5
(882 + 7) / (882 + 7 + 10 + 32)
## [1] 0.9548872
