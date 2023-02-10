

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

# After the lab session, you will see in the answers version of the workbook that you get a further opportunity:
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


# -- Task 3 -- Transform the rt and acc data from wide to long using gather()

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

# -- write code and run it here --


# -- Q.3.4. -- Change the predictor from frequency to something else: you choose.
# -- Q.3.5. -- Produce a scatterplot to visualize the relationship between the two variables: 
# does the relationship you see in the plot match the coefficient you see in the model estimates?

# -- write code and run it here --


# -- Task 12 -- Visualize the relationship between frequency and RT, separately for each participant

long.all.noNAs %>%
  ggplot(aes(x = Lg.UK.CDcount, y = RT)) +
    geom_point(alpha = .2) + 
    geom_smooth(method = "lm", se = FALSE, size = 1.25, colour = "red") + 
    theme_bw() + 
    xlab("Word frequency (log10 UK SUBTLEX CD count)") + 
    facet_wrap(~ subjectID)


# -- Q.3.6. -- Can you visualize the relationship between RT and Length, also, separately for each participant?

# -- write code and run it here --

# -- Q.3.7. -- What do you conclude from this plot about the Length effect, and about how the effect varies?



############################################################################################################
## Step 6: Analyze data with lmer ##########################################################################


# -- Task 13 -- Fit a linear mixed-effects model, including the random effect of participants on intercepts and on the
# slope of the frequency effect

lmer.all.1 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 1||subjectID),
             
             data = long.all.noNAs)

summary(lmer.all.1)


# -- Task 14 -- Fit different kinds of mixed-effects model: vary the random effects terms

lmer.all.1 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 1||subjectID),
                   
                   data = long.all.noNAs)

summary(lmer.all.1)

lmer.all.2 <- lmer(RT ~  Lg.UK.CDcount + (1|subjectID),
                   
                   data = long.all.noNAs)

summary(lmer.all.2)

lmer.all.3 <- lmer(RT ~  Lg.UK.CDcount + (Lg.UK.CDcount + 0|subjectID),
                   
                   data = long.all.noNAs)

summary(lmer.all.3)


# -- Q.4. -- Can you describe the differences between the models?
# -- Q.4.1. -- How do the random effects differ?
# -- Q.4.2. -- How do the fixed effects estimates differ?


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


# -- Q.6. -- Now fit a mixed-effects model for the effect of Length on RT

lmer.all.2 <- lmer(RT ~  Length + 
                     (Length + 1||subjectID) +
                     (1|item_name),
                   
                   data = long.all.noNAs)

# -- write code and run it here --

# -- Q.6.1. -- What are the random effects estimates -- variances?
# -- Q.6.2. -- What is the estimate of the effect of length?
# -- Q.6.3. -- What does the estimate tell you about how RT varies as Length varies?



############################################################################################################


