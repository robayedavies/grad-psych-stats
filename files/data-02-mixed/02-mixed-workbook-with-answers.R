

############################################################################################################


# In this workbook -- 02-mixed -- our learning targets are:

# -- Practice how to tidy experimental data for mixed-effects analysis
# -- Begin to develop an understanding of crossed random effects of subjects and stimuli
# -- Practice fitting linear mixed-effects models incorporating random effects of subjects and stimuli


# The aims of the lab session work are:
# -- get practice running the code required to tidy and prepare experimental data for analysis
# -- exercise skills by varying model code -- changing variables, changing options -- so that you can see how the code works
# -- use the opportunity to reflect on and evaluate results -- so that we can support growth in development of 
# understanding of main ideas
# -- optional -- get practice running the code: so that you can reproduce the figures and results from the lecture and in 
# the book chapter



############################################################################################################


# -- We start by tidying the data -- this is useful for you to learn but it is optional: skip ahead to the analyses
# if you prefer.
# -- We start the analysis bit in Step 4



############################################################################################################
## Step 1: Set-up ##########################################################################################


# -- Task 1 -- Run this code to load relevant libraries

library(broom)
library(gridExtra)
library(here)
library(lme4)
library(patchwork)
library(sjPlot)
library(tidyverse)



############################################################################################################
## Step 2: Load data #######################################################################################


# -- Task 2 -- Read in the data files we will be using:

behaviour.rt <- read_tsv("CP study word naming rt 180211.dat", na = "-999")
behaviour.acc <- read_tsv("CP study word naming acc 180211.dat", na = "-999")
subjects <- read_csv("all.subjects 110614-050316-290518.csv", na = "-999")
words <- read_csv("words.items.5 120714 150916.csv", na = "-999")



############################################################################################################
## Step 3: Tidy data #######################################################################################


# -- Tidying the data involves a number of tasks, some essential and some things we do
# for convenience


# -- Task 3 -- Transform the rt and acc data from wide to long using pivot_longer()

rt.long <- behaviour.rt %>%
                            pivot_longer(2:62, names_to = "subjectID", values_to = "RT")

acc.long <- behaviour.acc %>%
                            pivot_longer(2:62, names_to = "subjectID", values_to = "accuracy")


## -- inspect

head(behaviour.rt)
head(behaviour.acc)

head(rt.long)
head(acc.long)


# -- Task 4 -- Join data from different sources

long <- rt.long %>% 
                  full_join(acc.long)

long.subjects <- long %>% 
                        full_join(subjects, by = "subjectID")

long.all <- long.subjects %>%
                            full_join(words, by = "item_name")


# -- Task 5 -- Select just the variables we need

long.all.select <- long.all %>% 
                        select(item_name, subjectID, RT, accuracy, 
                               Lg.UK.CDcount, brookesIMG, AoA_Kup_lem, 
                               Ortho_N, regularity, Length, BG_Mean, 
                               Voice,	Nasal,	Fricative,	Liquid_SV,	
                               Bilabials,	Labiodentals,	Alveolars,	
                               Palatals,	Velars,	Glottals, age.months, 
                               TOWREW_skill, TOWRENW_skill, spoonerisms, CART_score)


# -- Q.1. -- Select different variables -- You could analyze the CP study data for a research 
# report. What if you wanted to analyze a different set of variables, 
# could you select different variables?


# -- Task 6 -- Filter the observations

# -- get rid of observations about FALSE

long.all.select.filter <- long.all.select %>% 
                                           filter(item_name != 'FALSE')

# -- get rid of RT less than or equal to 200

long.all.select.filter <- long.all.select.filter %>%
                                                  filter(RT >= 200)


# -- Q.2. -- Vary the filter conditions in different ways --
# -- Q.2.1. -- Change the threshold for including RTs from RT >= 200 to something else
# -- Q.2.2. -- Can you assess what impact the change has? 
# -- Hint Q.2.2. -- Note that you can count the number of observations (rows) in a dataset
# using e.g. length(data.set.name$variable.name)


# -- Task 7 -- Remove missing values

long.all.noNAs <- na.omit(long.all.select.filter)


## inspect

head(long.all.noNAs, n = 10)


# -- Task 8 -- Create a .csv from the tidied data

write_csv(long.all.noNAs, "long.all.noNAs.csv")



############################################################################################################
## Step 4: Read in pre-tidied data #########################################################################


# -- Task 9 -- Read in .csv of tidied data

long.all.noNAs <- read_csv("long.all.noNAs.csv", 
                           col_types = cols(
                             subjectID = col_factor(),
                             item_name = col_factor()
                           )
                          ) 



############################################################################################################
## Step 5: Analyze data with lm ############################################################################


# -- Task 10 -- Visualize the relationship between frequency and RT, ignoring participant differences

long.all.noNAs %>%
ggplot(aes(x = Lg.UK.CDcount, y = RT)) +
  geom_point(alpha = .2) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, colour="red") + 
  theme_bw() + 
  xlab("Word frequency: log context distinctiveness (CD) count")


# -- Task 11 -- Estimate the relationship between frequency and RT, ignoring participant differences

lm.all.1 <- lm(RT ~  Lg.UK.CDcount,
             
                     data = long.all.noNAs)

summary(lm.all.1)


# -- Q.3. -- Vary the linear model using different outcomes or predictors -- 

# -- Q.3.1. -- What is the estimate of the effect of Length on RT?
# -- Q.3.2. -- What does the estimate tell you about how RT varies in relation to word length?
# -- Q.3.3. -- What is the R-squared for the model?
  
# lm.all.1 <- lm(RT ~  Length,
#                  
#                  data = long.all.noNAs)
# 
# summary(lm.all.1)  

# -- A.3.1. -- The estimate for the Length effect is 18.369.
# -- A.3.2. -- This tells us that RT increases by about 18ms for each increase in word length by one letter.
# -- A.3.3. -- The R-sq is .01.


# -- Q.3.4. -- Change the predictor from frequency to something else: you choose.
# -- Q.3.5. -- Produce a scatterplot to visualize the relationship between the two variables: 
# does the relationship you see in the plot match the coefficient you see in the model estimates?


# -- Task 12 -- Visualize the relationship between frequency and RT, separately for each participant

long.all.noNAs %>%
  ggplot(aes(x = Lg.UK.CDcount, y = RT)) +
    geom_point(alpha = .2) + 
    geom_smooth(method = "lm", se = FALSE, size = 1.25, colour = "red") + 
    theme_bw() + 
    xlab("Word frequency (log10 UK SUBTLEX CD count)") + 
    facet_wrap(~ subjectID)


# -- Q.3.6. -- Can you visualize the relationship between RT and Length, also, separately for each participant?

# long.all.noNAs %>%
#   ggplot(aes(x = Length, y = RT)) +
#   geom_point(alpha = .2) + 
#   geom_smooth(method = "lm", se = FALSE, size = 1.25, colour = "red") + 
#   theme_bw() + 
#   facet_wrap(~ subjectID)

# -- Q.3.7. -- What do you conclude from this plot about the Length effect, and about how the effect varies?
# -- A.3.7. -- We can conclude that the length effect is small and really only apparent for some participants.



############################################################################################################
## Step 6: Analyze data with lmer ##########################################################################


# -- Task 13 -- Fit a linear mixed-effects model, including the random effect of participants on intercepts and on the
# slope of the frequency effect

lmer.all.1 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 1||subjectID),
             
             data = long.all.noNAs)

summary(lmer.all.1)

# -- You can copy into your script the model summary which will be shown to you in the console window:
#   summary(lmer.all.1)
# Linear mixed model fit by REML ['lmerMod']
# Formula: RT ~ Lg.UK.CDcount + ((1 | subjectID) + (0 + Lg.UK.CDcount |      subjectID))
# Data: long.all.noNAs
# 
# REML criterion at convergence: 117805.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.7839 -0.5568 -0.1659  0.3040 12.4850 
# 
# Random effects:
#   Groups      Name          Variance Std.Dev.
# subjectID   (Intercept)   87575    295.93  
# subjectID.1 Lg.UK.CDcount  2657     51.55  
# Residual                  23734    154.06  
# Number of obs: 9085, groups:  subjectID, 61
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)    950.913     39.216  24.248
# Lg.UK.CDcount  -67.980      7.092  -9.586
# 
# Correlation of Fixed Effects:
#   (Intr)
# Lg.UK.CDcnt -0.093


# -- Task 14 -- Fit different kinds of mixed-effects model: vary the random effects terms

lmer.all.1 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 1||subjectID),
                   
                   data = long.all.noNAs)

summary(lmer.all.1)

# > summary(lmer.all.1)
# Linear mixed model fit by REML ['lmerMod']
# Formula: RT ~ Lg.UK.CDcount + ((1 | subjectID) + (0 + Lg.UK.CDcount |      subjectID))
# Data: long.all.noNAs
# 
# REML criterion at convergence: 117805.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.7839 -0.5568 -0.1659  0.3040 12.4850 
# 
# Random effects:
#   Groups      Name          Variance Std.Dev.
# subjectID   (Intercept)   87575    295.93  
# subjectID.1 Lg.UK.CDcount  2657     51.55  
# Residual                  23734    154.06  
# Number of obs: 9085, groups:  subjectID, 61
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)    950.913     39.216  24.248
# Lg.UK.CDcount  -67.980      7.092  -9.586
# 
# Correlation of Fixed Effects:
#   (Intr)
# Lg.UK.CDcnt -0.093

lmer.all.2 <- lmer(RT ~  Lg.UK.CDcount + (1|subjectID),
                   
                   data = long.all.noNAs)

summary(lmer.all.2)

# summary(lmer.all.2)
# Linear mixed model fit by REML ['lmerMod']
# Formula: RT ~ Lg.UK.CDcount + (1 | subjectID)
# Data: long.all.noNAs
# 
# REML criterion at convergence: 117925.2
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.2695 -0.5652 -0.1705  0.3117 12.4390 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# subjectID (Intercept) 13036    114.2   
# Residual              24697    157.2   
# Number of obs: 9085, groups:  subjectID, 61
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)    925.320     17.770   52.07
# Lg.UK.CDcount  -61.919      2.599  -23.82
# 
# Correlation of Fixed Effects:
#   (Intr)
# Lg.UK.CDcnt -0.561

lmer.all.3 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 0|subjectID),
                   
                   data = long.all.noNAs)

summary(lmer.all.3)

# summary(lmer.all.3)
# Linear mixed model fit by REML ['lmerMod']
# Formula: RT ~ Lg.UK.CDcount + (Lg.UK.CDcount + 0 | subjectID)
# Data: long.all.noNAs
# 
# REML criterion at convergence: 118242.7
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7886 -0.5663 -0.1674  0.3102 12.1326 
# 
# Random effects:
#   Groups    Name          Variance Std.Dev.
# subjectID Lg.UK.CDcount   740.1   27.21  
# Residual                25607.0  160.02  
# Number of obs: 9085, groups:  subjectID, 61
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)    908.238     10.261   88.51
# Lg.UK.CDcount  -57.954      4.372  -13.26
# 
# Correlation of Fixed Effects:
#   (Intr)
# Lg.UK.CDcnt -0.596

# -- Q.4. -- Can you describe the differences between the models?
# -- Q.4.1. -- How do the random effects differ?
# -- Q.4.2. -- How do the fixed effects estimates differ?

# -- A.4.1. -- There are three models, 1, 2 and 3. 

# -- lmer.all.1 includes random effects:
# Random effects:
# Groups      Name          Variance Std.Dev.
# subjectID   (Intercept)   87575    295.93  
# subjectID.1 Lg.UK.CDcount  2657     51.55  
# Residual                  23734    154.06 

# -- lmer.all.2 includes:
# Random effects:
# Groups    Name        Variance Std.Dev.
# subjectID (Intercept) 13036    114.2   
# Residual              24697    157.2   

# -- lmer.all.3 includes:
# Random effects:
# Groups    Name          Variance Std.Dev.
# subjectID Lg.UK.CDcount   740.1   27.21
# Residual                25607.0  160.02    

# -- A.4.1. -- We can see that model 1 includes random effects of subjects on intercepts and slopes, model 2 includes random effects
# of subjects on intercepts, and model 3 includes random effects of subjects on slopes.
# -- The residual variance is largest for model 3.

# -- You may have noticed that the variance for the subjectID   (Intercept) term is very different if we compare models
# lmer.all.1 and lmer.all.2
# -- I deal with this in the following section *.

# -- A.4.2. -- The fixed effect, the estimate for the frequency effect coefficient varies between models:
# 1. -67.980; 2. -61.919; 3. -57.954
  

# -- Task 15 -- Fit a linear mixed-effects model in which we estimate the effect of frequency on RT and include random
# effects of participants and of items

lmer.all.si <- lmer(RT ~  Lg.UK.CDcount + 
                         (Lg.UK.CDcount + 1||subjectID) +
                         (1|item_name),
             
             data = long.all.noNAs)

summary(lmer.all.si)


# -- Q.5. -- Can you describe the differences between the models with vs. without the item term?
# -- Q.5.1. -- How do the random effects differ?
# -- Q.5.2. -- How do the fixed effects estimates differ?

# -- A.5.1. -- We can compare models 

# lmer.all.1 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 1||subjectID),
#                    
#                    data = long.all.noNAs)

# lmer.all.si <- lmer(RT ~  Lg.UK.CDcount + 
#                      (Lg.UK.CDcount + 1||subjectID) +
#                      (1|item_name),
#                    
#                    data = long.all.noNAs)

# -- The models differ, obviously, in the inclusion of a random term corresponding to the effect of items on
# intercepts.
# -- We can also see that the residual variance is smaller where we include the item random effect.

# -- A.5.2. -- The estimate for the Lg.UK.CDcount is now -72.33.


# -- Q.6. -- Now fit a mixed-effects model for the effect of Length on RT

lmer.all.2 <- lmer(RT ~  Length + 
                     (Length + 1||subjectID) +
                     (1|item_name),
                   
                   data = long.all.noNAs)

summary(lmer.all.2)

# -- Q.6.1. -- What are the random effects estimates -- variances?
# -- A.6.1. -- We see: items/intercepts variance of 4751, subjects/Length of 757, subjects/intercepts of 4994
# and residuals of 21440
# -- Q.6.2. -- What is the estimate of the effect of length?
# -- A.6.2. -- The estimate is 28.361.
# -- Q.6.3. -- What does the estimate tell you about how RT varies as Length varies?
# -- Q.6.4. -- That RT increases by about 28ms for unit increase in word length.



############################################################################################################
# Optional: reproduce the plots in the chapter and slides ##################################################


# -- This part is completely optional for you to view and work with:
# -- run the code, and consider the code steps *only* if you are interested
# -- in how the materials for the book chapter were created.



############################################################################################################


# -- Visualize the relationship between frequency and RT, separately for each participant

# plot intercept and lm frequency coefficient for each child, data considered separately

freqperchildlm <- long.all.noNAs %>%
  group_by(subjectID) %>% 
  do(tidy(lm(RT ~ Lg.UK.CDcount, data=.)))

freqperchildlm$term <- as.factor(freqperchildlm$term)

freqperchildlmint <- filter(freqperchildlm, term == '(Intercept)')
freqperchildlmfreq <- filter(freqperchildlm, term == 'Lg.UK.CDcount')

pfreqperchildlmint <- freqperchildlmint %>%
  ggplot(aes(x = fct_reorder(subjectID, estimate), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) + 
  geom_point() + geom_linerange() + 
  theme_bw() + 
  xlab("Intercepts in order of size") +
  ylab("Estimated intercept +/- SE") + 
  theme(axis.title.y = element_text(size = 10), 
        axis.text.y = element_text(size = 5), 
        axis.text.x = element_blank(), 
        panel.grid = element_blank())

pfreqperchildlmfreq <- freqperchildlmfreq %>%
  ggplot(aes(x = fct_reorder(subjectID, estimate), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) + 
  geom_point() + geom_linerange() + 
  theme_bw() + 
  xlab("Frequency effect coefficient in order of size") +
  ylab("Estimated coefficient for the frequency effect +/- SE") + 
  theme(axis.title.y = element_text(size = 10), 
        axis.text.y = element_text(size = 5), 
        axis.text.x = element_blank(), 
        panel.grid = element_blank())

grid.arrange(pfreqperchildlmint, pfreqperchildlmfreq,
             ncol = 2) 


# -- Take the linear mixed-effects model, and plot the model predicted differences in intercepts 
# or slopes

lmer.all.1 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 1||subjectID),
                   
                   data = long.all.noNAs)

# -- plot the distribution of random effects -- conditional modes:

BLUPS <- ranef(lmer.all.1)$subjectID
# summary(BLUPS)

BLUPS.df <- data.frame(subjectID = rownames(BLUPS), 
                       intercepts = BLUPS[,1], 
                       frequency = BLUPS[,2])
# summary(BLUPS.df)
# str(BLUPS.df)

p.BLUPS.intercepts <- BLUPS.df %>%
  ggplot(aes(x = intercepts)) +
  geom_histogram() + 
  ggtitle("(a.) Adjustment by child") +
  xlab("In the intercept") +
  geom_vline(xintercept = 0, colour = "red", size = 1.5) +
  theme_bw()

p.BLUPS.slopes <- BLUPS.df %>%
  ggplot(aes(x = frequency)) +
  geom_histogram() + 
  ggtitle("(b.) Adjustment by child") +
  xlab("In the frequency slope (coefficient)") +
  geom_vline(xintercept = 0, colour = "red", size = 1.5) +
  theme_bw()

grid.arrange(p.BLUPS.intercepts, p.BLUPS.slopes,
             ncol = 2
)


# -- Fit different kinds of mixed-effects model, then plot the resulting differences in model predictions

lmer.all.1 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 1||subjectID),
                   
                   data = long.all.noNAs)

# summary(lmer.all.1)

lmer.all.2 <- lmer(RT ~  Lg.UK.CDcount + (1|subjectID),
                   
                   data = long.all.noNAs)

# summary(lmer.all.2)

lmer.all.3 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 0|subjectID),
                   
                   data = long.all.noNAs)

# summary(lmer.all.3)

# -- plot random effect -- per person predictions
# https://bbolker.github.io/morelia_2018/notes/mixedlab.html
# In lmer, predict has a re.form argument that specifies which random effects should 
# be included (NA or ~0=none, population level; NULL (=all) or ~subject=prediction at 
# the subject level; more complex models, might have additional nested levels).

# -- get predictions

long.all.noNAs$pred1 <- predict(lmer.all.1) ## individual level
long.all.noNAs$pred2 <- predict(lmer.all.2) ## individual level
long.all.noNAs$pred3 <- predict(lmer.all.3) ## individual level

# -- show predictions as slopes on top of raw data

p.slopes.intercepts <- long.all.noNAs %>%
  ggplot(aes(x = Lg.UK.CDcount, y = RT)) +
  geom_point(alpha = .25, colour = "darkgrey") +
  geom_line(aes(y = pred1, group = subjectID), colour="red", alpha = .4) +
  ggtitle("(c.)\n(Lg.UK.CDcount + 1||subjectID)") +
  xlab("Frequency (Lg.UK.CDcount)") +
  xlim(0,5) +
  theme_bw()

p.intercepts <- long.all.noNAs %>%
  ggplot(aes(x = Lg.UK.CDcount, y = RT)) +
  geom_point(alpha = .25, colour = "darkgrey") +
  geom_line(aes(y = pred2, group = subjectID), colour="red", alpha = .4) +
  ggtitle("(a.)\n(1|subjectID)") +
  xlab("Frequency (Lg.UK.CDcount)") +
  xlim(0,5) +
  theme_bw()

p.slopes <- long.all.noNAs %>%
  ggplot(aes(x = Lg.UK.CDcount, y = RT)) +
  geom_point(alpha = .25, colour = "darkgrey") +
  geom_line(aes(y = pred3, group = subjectID), colour="red", alpha = .4) +
  ggtitle("(b.)\n(Lg.UK.CDcount + 0|subjectID)") +
  xlab("Frequency (Lg.UK.CDcount)") +
  xlim(0,5) +
  theme_bw()

grid.arrange(p.intercepts, p.slopes, p.slopes.intercepts, 
             ncol = 3
)


# -- Visualize the variation between items in intercepts
# -- for each itemname, plots ordered by meanRT

# create a model for each item, including just the intercept

wperitemlm <- long.all.noNAs %>% 
  group_by(item_name) %>% 
  do(tidy(lm(RT ~ 1, data=.)))
wperitemlm$term <- as.factor(wperitemlm$term)
# wperitemlm

# plot intercepts by-items -- ordering items by item_name by estimated intercept size

pwperitemlmint <- ggplot(wperitemlm, aes(x = fct_reorder(item_name, estimate), y = estimate, ymin = estimate - std.error, ymax = estimate + std.error))
pwperitemlmint + 
  geom_point() +
  geom_linerange() + 
  theme_bw() + 
  ylab("Estimated coefficient +/- SE") + 
  xlab("Per-item estimates of intercept, ordered by intercept estimate size") +
  theme(axis.title.y = element_text(size = 10), 
        axis.text.y = element_text(size = 5), 
        axis.text.x = element_blank(), panel.grid = element_blank())



############################################################################################################


