
load("ICPSR_21600/DS0001/21600-0001-Data.rda")
d <- da21600.0001
stopifnot(require(data.table))


Loading required package: data.table

## Data Laundry

stopifnot(require(data.table))
outlierReplace <- function(dataframe, cols, rows, newValue = NA) {
  if(any(rows)){
    set(dataframe, rows, cols, newValue)
  }
}
## Dependent Variables

Here we cleaned our Dependent variable which we want to predict: whether the respondent has a permanent tattoo? 0 = NO, 1 = YES

d$tattoo <- as.numeric(d$H1GH55)
outlierReplace(d, "tattoo", which(d$tattoo > 5), NA)
summary(d$tattoo)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.00000 0.00000 0.00000 0.04681 0.00000 1.00000      10
Independent Variables

Demographic and Family SES info

d$age <- 96 - d$H1GI1Y
outlierReplace(d, "age", which(d$age < 3), NA)
summary(d$age) #AGE
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   13.00   16.00   17.00   17.04   18.00   22.00       3
d$race <- d$H1GI8
outlierReplace(d, "race", which(d$race > 5), NA)
d$race <- as.factor(d$race)
summary(d$race)   ## This contains large amount of NA value, don't think I'll use it then...
##    1    2    3    4    5 NA's 
##  134   94   33   24   25 6194
d$paedu <- d$PA12
outlierReplace(d, "paedu", which(d$paedu > 9), NA)
summary(d$paedu)  ## This is parents' education degree
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   4.000   6.000   5.578   7.000   9.000     894
d$income <- d$PA55
outlierReplace(d, "income", which(d$income > 9995), NA)
summary(d$income)     ##This is family anuual income
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0    22.0    40.0    47.7    60.0   999.0    1575
hist(d$income)


d$lnincome <- log(d$income)
summary(d$lnincome)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    -Inf       3       4    -Inf       4       7    1575
d$lnincome[which(d$lnincome == -Inf)] <- NA
hist(d$lnincome)   ##Look so much better, doesn't it?


Behavioral Habits and Personality

Outdoor sports
During the past week, how many times did you go roller blading, roller-skating, skate-boarding or bicycling?

d$outdoorsports <- as.numeric(d$H1DA4)
outlierReplace(d, "outdoorsports", which(d$outdoorsports > 5), NA)
summary(d$outdoorsports)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.6091  1.0000  3.0000       6
Active sports
During the past week, how many times did you play an active sport, such as baseball, softball, basketball, soccer, swimming, or football?

d$activesports <- as.numeric(d$H1DA5)
outlierReplace(d, "activesports", which(d$activesports > 5), NA)
summary(d$activesports)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   1.000   1.381   2.000   3.000       6
Exercise
During the past week, how many times did you do exercise, such as jogging, walking karate, jumping rope, gymnastics or dancing?

d$exercise <- as.numeric(d$H1DA6)
outlierReplace(d, "exercise", which(d$exercise > 5), NA)
summary(d$exercise)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   1.000   2.000   1.622   3.000   3.000       6
Social
During the past week, how many times did you just hang out with friends

d$hangout <- as.numeric(d$H1DA7)
outlierReplace(d, "hangout", which(d$hangout > 5), NA)
summary(d$hangout)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   1.000   2.000   1.971   3.000   3.000       6
Motorcycle
During the past 12 months, how often did you ride a motorcycle?

d$motor <- as.numeric(d$H1GH40)
outlierReplace(d, "motor", which(d$motor > 5), NA)
summary(d$motor)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3631  0.0000  4.0000       7
seatbelt
How often do you wear a seatbelt when you are riding in or driving a car?

d$seatbelt <- as.numeric(d$H1GH42)
outlierReplace(d, "seatbelt", which(d$seatbelt > 5), NA)
summary(d$seatbelt)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   4.000   3.071   4.000   4.000       7
nightout
During the past 12 months, have you ever spent the night away from home without permission?

d$nightout <- as.numeric(d$H1GH53)
outlierReplace(d, "nightout", which(d$nightout > 2), NA)
summary(d$nightout)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.1319  0.0000  1.0000      16
Smoking, Drinking, all the “wild” stuffs

Cigarette
Have you ever tried cigarette smoking, even just 1 or 2 puffs?

d$cigapuff <- as.numeric(d$H1TO1)
outlierReplace(d, "cigapuff", which(d$cigapuff > 5), NA)
summary(d$cigapuff)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.5561  1.0000  1.0000      55
Of your 3 best friends, how many smoke at least 1 cigarette a day?

d$fsmoking <- as.numeric(d$H1TO9)
outlierReplace(d, "fsmoking", which(d$fsmoking > 5), NA)
summary(d$fsmoking)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.8169  1.0000  3.0000     132
Alcohol
Have you had a drink of beer, wine or liquor – not just a sip or a taste of someone else’s drink

d$drink <- as.numeric(d$H1TO12)
outlierReplace(d, "drink", which(d$drink > 5), NA)
summary(d$drink)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.5525  1.0000  1.0000      60
During the past 12 months, on how many days did you drink alcohol?

d$drinkd <- as.numeric(d$H1TO15)
outlierReplace(d, "drinkd", which(d$drinkd > 8), NA)
d$drinkd <- 7 - d$drinkd  
summary(d$drinkd) ## We got around half of sample in the dataset as missing value since this is not a appropriate question to many teens, APPARENTLY!! So I consider to use the other variable to represent! 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   1.000   2.000   1.933   3.000   6.000    2959
Of your 3 best friends, how many drink alcohol at least once a month?

d$fdrinking <- as.numeric(d$H1TO29)
outlierReplace(d, "fdrinking", which(d$fdrinking > 5), NA)
summary(d$fdrinking) ## we only got 153 NA here. But if you don't drink a lot, I bet you won't have a lot of "go to hell" friends.
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   1.000   1.105   2.000   3.000     153
Marijuana
During your life, how many times have you used marijuana?

d$weed <- as.numeric(d$H1TO31)
outlierReplace(d, "weed", which(d$weed > 960), NA)
summary(d$weed)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    1.00    2.00    8.00   39.73   27.25  900.00    4926
During the past 30 days, how many times have you used marijuana?

d$weedm <- as.numeric(d$H1TO32)
outlierReplace(d, "weedm", which(d$weedm > 960), NA)
summary(d$weedm)  ## the problem is there are almost 5000 NA in both of these questions... Seems like we have to use social circle to solve this problem again.
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    1.00    7.22    4.00  800.00    4849
Of your 3 best friends, how many use marijuana at least once a month?

d$fweed <- as.numeric(d$H1TO33)
outlierReplace(d, "fweed", which(d$fweed > 5), NA)
summary(d$fweed)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.5841  1.0000  3.0000     158
All other drugs detailed variables shared the similar quesitons that there are always many NA since most of observation are 10+, not applicable to ask these questions.

Personality

When making decisions, you usually go with your “gut feeling” without thinking too much about the consequences of each alternative

d$gut <- as.numeric(d$H1PF16)
outlierReplace(d, "gut", which(d$gut > 5), NA)
d$gut <- 6 - d$gut
summary(d$gut)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   2.000   3.000   2.984   4.000   5.000      62
When you are attempting to find a solution to a problem, you usually try to think of as many different ways to approach the problem as possible

d$sp <- as.numeric(d$H1PF19)
outlierReplace(d, "sp", which(d$sp > 5), NA)
d$sp <- 6 - d$sp
summary(d$sp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   4.000   4.000   3.972   4.000   5.000      64
You like yourself just the way you are

d$ly <- as.numeric(d$H1PF33)
outlierReplace(d, "ly", which(d$ly > 5), NA)
d$ly <- 6 - d$ly
summary(d$ly)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   4.000   4.000   4.021   5.000   5.000      22
You feel socially accepted

d$sa <- as.numeric(d$H1PF35)
outlierReplace(d, "sa", which(d$sa > 5), NA)
d$sa <- 6 - d$sa
summary(d$sa)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   4.000   4.000   4.091   5.000   5.000      25
Parent Relationship

This is a bit complicated, but relationships with parents always have a huge impacts on everything when it comes to teens.

Independence Parenting Index
This index is measured by a set of questions about whether the parents let the children make their own decisions.

d$I.hometime <- as.numeric(d$H1WP1)
outlierReplace(d, "I.hometime", which(d$I.hometime > 2), NA)
summary(d$I.hometime)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3288  1.0000  1.0000     151
d$I.friends <- as.numeric(d$H1WP2)
outlierReplace(d, "I.friends", which(d$I.friends > 2), NA)
summary(d$I.friends)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.8519  1.0000  1.0000     142
d$I.outfit <- as.numeric(d$H1WP3)
outlierReplace(d, "I.outfit", which(d$I.outfit > 2), NA)
summary(d$I.outfit)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.9062  1.0000  1.0000     141
d$I.tv <- as.numeric(d$H1WP4)
outlierReplace(d, "I.tv", which(d$I.tv > 2), NA)
summary(d$I.tv)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.8248  1.0000  1.0000     141
d$I.tvp <- as.numeric(d$H1WP5)
outlierReplace(d, "I.tvp", which(d$I.tvp > 2), NA)
summary(d$I.tvp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.7719  1.0000  1.0000     142
d$I.sleep <- as.numeric(d$H1WP6)
outlierReplace(d, "I.sleep", which(d$I.sleep > 2), NA)
summary(d$I.sleep)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.6524  1.0000  1.0000     141
d$I.eat <- as.numeric(d$H1WP7)
outlierReplace(d, "I.eat", which(d$I.eat > 2), NA)
summary(d$I.eat)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.8156  1.0000  1.0000     139
stopifnot(require(RSNNS))
## Loading required package: RSNNS
## Loading required package: Rcpp
## Warning: package 'Rcpp' was built under R version 3.2.2
d$I.momI <- 8 - as.numeric(d$H1PF2) #the higher of this number, the more agreement of "Your mother encourages you to be independent"
d$I.zmomI <- normalizeData(d$I.momI, type = "0_1") * 3
summary(d$I.zmomI)
##        V1       
##  Min.   :0.000  
##  1st Qu.:2.571  
##  Median :2.571  
##  Mean   :2.527  
##  3rd Qu.:3.000  
##  Max.   :3.000
Generating the Independence Parenting Index_

I.vars <- c("I.hometime", "I.friends", "I.outfit", "I.tv", "I.tvp", "I.sleep", "I.eat", "I.zmomI")
sub.I <- d[ ,I.vars]

stopifnot(require(psych))
## Loading required package: psych
summary(alpha(sub.I)) #Here Cronbach's a is 0.59, even though it's less than 0.65 which is what we usually looking for, but it's still ok right ;) Cut him some slack plz!!!
## 
## Reliability analysis   
##  raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.52      0.59    0.57      0.15 1.4 0.011 0.95 0.22
d$Inde <- rowSums(sub.I, na.rm = TRUE)
d$Inde.s <- normalizeData(d$Inde, type = "norm")
summary(d$Inde.s)
##        V1         
##  Min.   :-3.8812  
##  1st Qu.:-0.5102  
##  Median : 0.2226  
##  Mean   : 0.0000  
##  3rd Qu.: 0.7356  
##  Max.   : 1.2486
Intimacy and Attention from Parents

This index can tell us how often and how much the parents got interacted with the children. And how close and intimate the children feel the relationship with their parents.

d$c.dinner <- as.numeric(d$H1WP8)
outlierReplace(d, "c.dinner", which(d$c.dinner > 10), NA)
summary(d$c.dinner) ## now from 0 - 7 days for a week at least one of the parents would have dinner togetehr
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   3.000   5.000   4.609   7.000   7.000     154
d$c.mom <- as.numeric(d$H1WP9)
outlierReplace(d, "c.mom", which(d$c.mom > 5), NA)
summary(d$c.mom) # from 1 to 5 how close you are to your mom
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    1.00    4.00    5.00    4.55    5.00    5.00     375
d$c.momc <- as.numeric(d$H1WP10)
outlierReplace(d, "c.momc", which(d$c.momc > 5), NA)
summary(d$c.momc) # how much you mom cares about you from 1 to 5
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   5.000   5.000   4.857   5.000   5.000     374
d$c.dad <- as.numeric(d$H1WP13)
outlierReplace(d, "c.dad", which(d$c.dad > 5), NA)
summary(d$c.dad) # from 1 to 5 how close you are to your dad, BTW there are 1947 NA
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   4.000   5.000   4.278   5.000   5.000    1957
d$c.dadc <- as.numeric(d$H1WP14)
outlierReplace(d, "c.dadc", which(d$c.dadc > 5), NA)
summary(d$c.dadc) # how much you dad cares about you from 1 to 5
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   5.000   5.000   4.747   5.000   5.000    1957
d$c.mshopping <- as.numeric(d$H1WP17A)
outlierReplace(d, "c.mshopping", which(d$c.mshopping > 2), NA)
summary(d$c.mshopping)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.7286  1.0000  1.0000     381
d$c.dshopping <- as.numeric(d$H1WP18A)
outlierReplace(d, "c.dshopping", which(d$c.dshopping > 2), NA)
summary(d$c.dshopping)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2638  1.0000  1.0000    1962
d$c.msport <- as.numeric(d$H1WP17B)
outlierReplace(d, "c.msport", which(d$c.msport > 2), NA)
summary(d$c.msport)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.0892  0.0000  1.0000     381
d$c.dsport <- as.numeric(d$H1WP18B)
outlierReplace(d, "c.dsport", which(d$c.dsport > 2), NA)
summary(d$c.dsport)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3021  1.0000  1.0000    1962
d$c.mreligious <- as.numeric(d$H1WP17C)
outlierReplace(d, "c.mreligious", which(d$c.mreligious > 2), NA)
summary(d$c.mreligious)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3987  1.0000  1.0000     381
d$c.dreligious <- as.numeric(d$H1WP18C)
outlierReplace(d, "c.dreligious", which(d$c.dreligious > 2), NA)
summary(d$c.dreligious)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3199  1.0000  1.0000    1962
d$c.mdate <- as.numeric(d$H1WP17D)
outlierReplace(d, "c.mdate", which(d$c.mdate > 2), NA)
summary(d$c.mdate)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.4728  1.0000  1.0000     381
d$c.ddate <- as.numeric(d$H1WP18D)
outlierReplace(d, "c.ddate", which(d$c.ddate > 2), NA)
summary(d$c.ddate)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2732  1.0000  1.0000    1962
d$c.mevent <- as.numeric(d$H1WP17E)
outlierReplace(d, "c.mevent", which(d$c.mevent > 2), NA)
summary(d$c.mevent)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2654  1.0000  1.0000     381
d$c.devent <- as.numeric(d$H1WP18E)
outlierReplace(d, "c.devent", which(d$c.devent > 2), NA)
summary(d$c.devent)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2453  0.0000  1.0000    1962
d$c.mpp <- as.numeric(d$H1WP17F)
outlierReplace(d, "c.mpp", which(d$c.mpp > 2), NA)
summary(d$c.mpp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3872  1.0000  1.0000     381
d$c.dpp <- as.numeric(d$H1WP18F)
outlierReplace(d, "c.dpp", which(d$c.dpp > 2), NA)
summary(d$c.dpp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   0.000   0.192   0.000   1.000    1962
d$c.mar <- as.numeric(d$H1WP17G)
outlierReplace(d, "c.mar", which(d$c.mar > 2), NA)
summary(d$c.mar)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3294  1.0000  1.0000     381
d$c.dar <- as.numeric(d$H1WP18G)
outlierReplace(d, "c.dar", which(d$c.dar > 2), NA)
summary(d$c.dar)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2457  0.0000  1.0000    1962
d$c.mschool <- as.numeric(d$H1WP17H)
outlierReplace(d, "c.mschool", which(d$c.mschool > 2), NA)
summary(d$c.mschool)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.6289  1.0000  1.0000     381
d$c.dschool <- as.numeric(d$H1WP18H)
outlierReplace(d, "c.dschool", which(d$c.dschool > 2), NA)
summary(d$c.dschool)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.5189  1.0000  1.0000    1962
d$c.mproject <- as.numeric(d$H1WP17I)
outlierReplace(d, "c.mproject", which(d$c.mproject > 2), NA)
summary(d$c.mproject)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.1318  0.0000  1.0000     381
d$c.dproject <- as.numeric(d$H1WP18I)
outlierReplace(d, "c.dproject", which(d$c.dproject > 2), NA)
summary(d$c.dproject)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.1118  0.0000  1.0000    1962
d$c.mtalk <- as.numeric(d$H1WP17J)
outlierReplace(d, "c.mtalk", which(d$c.mtalk > 2), NA)
summary(d$c.mtalk)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    1.00    0.52    1.00    1.00     381
d$c.dtalk <- as.numeric(d$H1WP18J)
outlierReplace(d, "c.dtalk", which(d$c.dtalk > 2), NA)
summary(d$c.dtalk)  # Just spent an hour of my lifetime on Copy and Paste, hope it will be statistical significant... I'm using my next birthday wish quota here... Sweet Please...!!!
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.4377  1.0000  1.0000    1962
Generating Attention(Intimacy) Parenting Index

C.vars <- c("c.mom","c.momc","c.dad","c.dadc","c.dinner", "c.mshopping","c.dshopping", "c.msport", "c.dsport", "c.mreligious", "c.dreligious", "c.mdate", "c.mevent", "c.mpp", "c.mar", "c.mschool", "c.mproject", "c.mtalk",  "c.ddate", "c.devent", "c.dpp", "c.dar", "c.dschool", "c.dproject", "c.dtalk")
sub.C <- d[ ,C.vars]

stopifnot(require(psych))
summary(alpha(sub.C)) #Yay!!! Cronbach's a looks yummy!  
## Warning in alpha(sub.C): Some items were negatively correlated with the
## total scale and probably should be reversed. To do this, run the function
## again with the 'check.keys=TRUE' option
## Some items ( c.mar ) were negatively correlated with the total scale and probably should be reversed.  To do this, run the function again with the 'check.keys=TRUE' option
## Reliability analysis   
##  raw_alpha std.alpha G6(smc) average_r S/N    ase mean   sd
##       0.64      0.74    0.81       0.1 2.9 0.0072  1.3 0.29
d$atten <- rowSums(sub.C, na.rm = TRUE)
d$atten.s <- normalizeData(d$atten, type = "norm")
summary(d$atten.s)
##        V1         
##  Min.   :-2.9529  
##  1st Qu.:-0.7464  
##  Median : 0.1826  
##  Mean   : 0.0000  
##  3rd Qu.: 0.7632  
##  Max.   : 2.5051
Academic Performance

Is it true that only bad students or stupid people would get a tattoo? Let’s find out!

In general, I combined 4 major subjects grades to generate a new variable to represent adolescents’ academic performance. The 4 subjects are English or other language, mathematics, history or social studies and science. I recoded each variables as A = 4, B = 3, C = 2, D = 1, never taken = 0, and define all the other missing values and not applicable as NA. Then normalize the sum of these 4 subjects scores, ran a reliability analysis.

Now let’s take a look of how they are doing at school

d$english <- 5 - as.numeric(d$H1ED11) ## Ranging from 0 to 4, not take the subject to D, C, B, A
outlierReplace(d, "english", which(d$english < 0), NA)
summary(d$english)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   2.796   4.000   4.000     234
d$math <- 5 - as.numeric(d$H1ED12) ## Ranging from 0 to 4, not take the subject to D, C, B, A
outlierReplace(d, "math", which(d$math < 0), NA)
summary(d$math)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   2.533   3.000   4.000     229
d$ss <- 5 - as.numeric(d$H1ED13) ## Ranging from 0 to 4, not take the subject to D, C, B, A
outlierReplace(d, "ss", which(d$ss < 0), NA)
summary(d$ss)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   2.596   4.000   4.000     222
d$science <- 5 - as.numeric(d$H1ED14) ## Ranging from 0 to 4, not take the subject to D, C, B, A
outlierReplace(d, "science", which(d$science < 0), NA)
summary(d$science)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   2.521   4.000   4.000     212
A.vars <- c("english", "math", "ss", "science")
sub.A <- d[ ,A.vars]

stopifnot(require(psych))
summary(alpha(sub.A)) # SEEMS LIKE THERE IS DIFFERENCE BETWEEN GOOD STUDENTS AND BAD STUDNETS... The difference is whether they're coding from 9 till now on a Sunday, it should be brunch time!!!
## 
## Reliability analysis   
##  raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.68      0.68    0.62      0.35 2.2 0.011  2.6 0.87
d$acaps <- rowSums(sub.A, na.rm = TRUE)
d$acap <- normalizeData(d$acaps, type = "norm")
summary(d$acap)
##        V1          
##  Min.   :-2.61927  
##  1st Qu.:-0.54190  
##  Median :-0.02256  
##  Mean   : 0.00000  
##  3rd Qu.: 0.75646  
##  Max.   : 1.53547
