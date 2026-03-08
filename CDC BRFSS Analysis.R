########FINAL PROJECT###########

#Final Project 
# I renamed the datatable in the csv file 'health' 
# for clarity/simplicity. 

library(tidyverse)
library(lm.beta)

health <- read_csv('BRFSS2015.csv')


#####    Q1    #####

Q1 <- health %>%
  select(HLTHPLN1) %>%
  filter(HLTHPLN1 == 1) %>%
  count()

#####    Q2    #####

# In order to work with the data, I needed to 
# rename the column for _State to State so I 
# did that using select(). I also changed the 
# 88 values to 0s.

health %>% 
  group_by(MENTHLTH) %>% 
  count() %>%
  filter(MENTHLTH == 88)

health$MENTHLTH[health$MENTHLTH == 88] <- 0

QM <- health %>%  
  select(State = "_STATE", MENTHLTH) %>%
  filter(between(MENTHLTH, 1,30) | MENTHLTH == 88, State == 42) %>%
  arrange(desc(MENTHLTH))

Q2 <- round(mean(QM$MENTHLTH), 2)

#####    Q3    #####

#####    Q4-Q9 Data Cleaning   #####

#  First, I selected and filtered the data for 
#  MARITAL to exclude NA values and those who 
#  refused to answer.  Then I converted the 
#  MARITAL data to a factor with 6 levels. 

Married_levels <- c('1', '2', '3', '4', '5', '6')

MarriedF <-as.factor(Married_levels)
class(MarriedF)

Married <- health %>%
  select(MARITAL, PA1MIN_, Fruits = "_FRUTSUM") %>%
  filter(between(MARITAL, 1, 6)) %>%
  mutate_at(vars (MARITAL), list(factor) ) %>%
  na.omit()

class(Married$MARITAL)

#####    Q4    #####

#  The outliers in the exercise per week 
#  data were really strange. There were a number
#  of very high values that may be attributable 
#  to human error when entering the data, or some 
#  other problem. Some of the data indicates answers  
#  of exercising over 83 hours per week, or over 
#  11 hours per day, which doesn't make sense 
#  even for professional athletes. At first, I 
#  followed the instructions, and I only 
#  eliminated outliers using the quantile 
#  function (and .3% for upper and lower), but I
#  think some of the upper outliers skewed the 
#  results. I created a new dataset, Married_no
#  for the data with no outliers according to 
#  those instructions. 

#  However, I decided to also to give you a 
#  beta version of the data with more of the
#  upper quantile outliers removed. I've included
#  both versions in my answers. The beta version
#  will Q4b, Q5b, etc. I think the beta version 
#  works better. I visually inspected the data 
#  and removed any value above the boxplot
#  for the exercise 

######    Q4     #####
upper <- quantile(Married$PA1MIN_, 0.997, na.rm = TRUE)
lower <- quantile(Married$PA1MIN_, 0.003, na.rm = TRUE)

out_phys1 <- which(Married$PA1MIN_ > upper | Married$PA1MIN_ < lower)

Q4 <-  round((nrow(Married)-length(out_phys))/nrow(Married)*100, 2)

Married_no <- Married[-out_phys1,]

####    Q4 BETA    #####
upperb <- quantile(Married$PA1MIN_, 0.825, na.rm = TRUE)
lowerb <- quantile(Married$PA1MIN_, 0.003, na.rm = TRUE)

out_physb <- which(Married$PA1MIN_ > upperb | Married$PA1MIN_ < lowerb)

Q4b <-  round((nrow(Married)-length(out_physb))/nrow(Married)*100, 2)

Married_nob <- Married[-out_physb,]

#####    Q5    #####

Q5 <- Married_no %>%
  group_by(MARITAL) %>%
  summarise(mean = round(mean(PA1MIN_), 2), sd = round(sd(PA1MIN_), 2), min = round(min(PA1MIN_), 2), max = round(max(PA1MIN_), 2))


#####    Q5 BETA    #####

Q5b <- Married_nob %>%
  group_by(MARITAL) %>%
  summarise(mean = round(mean(PA1MIN_), 2), sd = round(sd(PA1MIN_), 2), min = round(min(PA1MIN_), 2), max = round(max(PA1MIN_), 2))


#####    Q6    #####


Q6 <- ggplot(data = Married_no) + 
  geom_boxplot(mapping = aes(x = MARITAL, y = PA1MIN_))



####   Q6 BETA   ####

Q6b <- ggplot(data = Married_nob) + 
  geom_boxplot(mapping = aes(x = MARITAL, y = PA1MIN_))



#####    Q7    #####

RegressM <- lm(PA1MIN_ ~ MARITAL, Married_no)

Q7 <-summary(RegressM)

RegressMb <- lm(PA1MIN_ ~ MARITAL, Married_nob)

####   Q7 BETA   ####

Q7b <-summary(RegressMb)


#####    Q8    #####


testtable <- aov(lm(PA1MIN_ ~ MARITAL, Married_no))

Q8 <- TukeyHSD(testtable)

####   Q8 BETA   ####

testtableb <- aov(lm(PA1MIN_ ~ MARITAL, Married_nob))

Q8b <- TukeyHSD(testtableb)



#####    Q9    #####

#  Since the both the R squared and the AIC show that the 
#  regression with Fruits is a better predictor, I chose to 
#  use that as my AIC score for both versions. I think the 
#  beta version is the better version, however. 

RegressMF <- lm(PA1MIN_ ~ MARITAL + Fruits, Married_no)


summary(RegressM)$r.squared
summary(RegressMF)$r.squared

AIC(RegressM, k = 1)
AIC(RegressMF, k = 2)

Q9 <- AIC(RegressMF, k = 2)

#####BETA#####

RegressMFb <- lm(PA1MIN_ ~ MARITAL + Fruits, Married_nob)


summary(RegressMb)$r.squared
summary(RegressMFb)$r.squared

AIC(RegressMb, k = 1)
AIC(RegressMFb, k = 2)

Q9b <- AIC(RegressMFb, k = 2)

#####    Q10-Q11   #####

#  I chose SMOKE100, AVEDRNK2, EXEROFT1, and ADDEPEV2 as my 
#  variables. Two of the four variables were Boolean and 
#  evaluated to 1/2 (Yes/No) so they did not have any 
#  possible outlier values. For the other two variables 
#  (AVEDRNK2, EXEROFT1, which I renamed exercise after 
#  mutating it), I ran histograms and then used the 
#  quantile function to remove outliers of up to .03%. Only 
#  the AVEDRNK2 variable actually had any outliers above 99.7%,
#  so although I left the coding for outlier removal, the 
#  exercise variable did not need outliers removed. For 
#  both of these variables, I did not exclude outliers based 
#  on boxplot visualization because I thought outliers could
#  be reasonable, and I didn't have a good reason to assume
#  human error and exclude them.

#  For the AVEDRNK2 variable, I filtered the results to only
#  include respondents who answered how many drinks they had
#  on average in a day and responded with a number. I 
#  excluded all of the respondents who didn't know, refused,
#  or were left blank/NA.

#  For the SMOKE100 variable, I filtered the results to only 
#  include respondents who answered Yes or No. I excluded all
#  of the respondents who didn't know, refused, or were left 
#  blank/NA.

#  For the ADDEPV2 variable, I filtered the results to only 
#  include respondents who answered Yes or No. I excluded 
#  all of the respondents who didn't know, refused, or were 
#  left blank/NA.

#  For the EXEROFT1 variable (which I changed to exercise),
#  I filtered the results to only include respondents who 
#  answered how many times per week they walked, ran, jogged, 
#  or swam and responded with a number. I excluded all of 
#  the respondents who didn't know, refused, or were left 
#  blank/NA.

final <- health %>%
  select(AVEDRNK2, SMOKE100, EXEROFT1, ADDEPEV2) %>%
  filter(between(AVEDRNK2, 1, 76), between(SMOKE100, 1, 2), between(EXEROFT1, 101, 199), between(ADDEPEV2, 1, 2)) %>%
  mutate(exercise = EXEROFT1 - 100) %>%
  as.data.frame()


ggplot(data = final) +
  geom_histogram(mapping = aes(x=AVEDRNK2))

ggplot(data = final) +
  geom_histogram(mapping = aes(x=exercise))

upper <- quantile(final$AVEDRNK2, 0.997, na.rm = TRUE)
lower <- quantile(final$AVEDRNK2, 0.003, na.rm = TRUE)

out_d <- which(final$AVEDRNK2 > upper | final$AVEDRNK2 < lower)

out_e <- which(final$exercise > upper | final$exercise < lower)

final_no <- final[-out_d, -out_e,]

ggplot(data = final_no) +
  geom_histogram(mapping = aes(x=AVEDRNK2))

ggplot(data = final_no) +
  geom_histogram(mapping = aes(x=exercise))

#####    Q12    #####
 
#  Unfortunately, because 2 of my variables are Boolean,
#  they are not great for visualizations. I plotted
#  histograms for them to get a better sense of the data,
#  and the counts/ratio for each, and then visualized the 
#  other variables as a scatterplot, as well as a scatterplot
#  with smoothing. The scatterplot visualization was probably
#  the most useful. 

ggplot(data = final_no) +
  geom_histogram(mapping = aes(x = ADDEPEV2))

ggplot(data = final_no) +
  geom_histogram(mapping = aes(x = SMOKE100))


ggplot(data = final_no) +
  geom_boxplot(mapping = aes(x = exercise))

ggplot(data = final_no) +
  geom_boxplot(mapping = aes(x = AVEDRNK2))

ggplot(data = final_no) + 
  geom_point(mapping = aes(x=AVEDRNK2, y= exercise), color = "blue")

ggplot(data = final_no) +
  geom_smooth(mapping = aes(x=AVEDRNK2, y= exercise), color = "blue")

ggplot(data = final_no) +
  geom_point(mapping = aes(x=AVEDRNK2, y= exercise), color = "blue") +
  geom_smooth(mapping = aes(x=AVEDRNK2, y= exercise), color = "red")

#####    Q13    #####

#  Most of the summary statistics are only useful for 
#  the exercise and AVEDRNK2 variables. 

summary(final_no)

#####    Q14   #####

#   The best fitting model to predicate exercise included 
#  all three predictors: smoking (SMOKE100), drinking 
#  (AVEDRNK2), and having depression (ADDEPEV2). All three 
#  predictors had p values under 0.05. Smoking and drinking
#  had negative correlations with exercise, whereas depression
#  had a positive correlation. I ran six permutations of the
#  regression model, but the original model, regress_final1,
#  which used all three predictors, had the best overall fit. 

regress_final1 <- lm(exercise ~ ADDEPEV2 + AVEDRNK2 + SMOKE100, data=final_no)

summary(regress_final1)

lm.beta(regress_final1)


regress_final2 <- lm(exercise ~ ADDEPEV2 + AVEDRNK2, data=final_no)


regress_final3 <- lm(exercise ~ ADDEPEV2, data=final_no)

regress_final4 <- lm(exercise ~ AVEDRNK2 + SMOKE100, data=final_no)

regress_final5 <- lm(exercise ~ AVEDRNK2, data=final_no)

regress_final6 <- lm(exercise ~ SMOKE100, data=final_no)

AIC(regress_final1, k = 3)

AIC(regress_final2, k = 2)

AIC(regress_final3, k = 1)

AIC(regress_final4, k = 2)

AIC(regress_final5, k = 1)

AIC(regress_final6, k = 1)

Q14 <- regress_final1 <- lm(exercise ~ ADDEPEV2 + AVEDRNK2 
                            + SMOKE100, data=final_no)