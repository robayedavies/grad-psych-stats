
############################################################################################################


# In this workbook -- 03-mixed -- our learning targets are:

# -- get practice running the code: so that you can reproduce the figures and results from the lecture and in 
# the book chapter
# -- exercise skills by varying code -- changing variables, changing options -- so that you can see how the code works
# -- use the opportunity to reflect on and evaluate results -- so that we can support growth in development of 
# understanding of main ideas


# We have reached the stage where our learning targets can be detailed -- here, we are concerned with:

# -- be able to specify a mixed-effects model in lmer() code
# -- be able to identify how the mixed-effects model code varies depending on the kinds of random effects 

# -- be able to identify the elements of the output or results that come from an lmer() mixed-effects analysis
# -- be able to interpret the fixed-effects estimates
# -- be able to interpret the random effects estimates, variance, covariance

# -- be able to describe in words and summary tables the results of a mixed-effects model
# -- be able to visualise the effects estimates from a mixed-effects model



############################################################################################################


# -- In the following steps:
# -- I first show you how to prepare data for analysis, just run the code and do the exercises;
# -- I then walk through a sequence of tasks where I show you how to fit and compare a series of models, again,
# just run the code and do the exercises;
# -- the critical step is Step 7, where I ask you to follow the examples, running your own series of models
# but with your selection of variables;
# -- finally, I show you how to reproduce some of the keyplots from the book chapter and slides, for your
# information


############################################################################################################
## Step 1: Set-up ##########################################################################################


# -- Task 1 -- Run this code to load relevant libraries

library(broom)
library(gridExtra)
library(here)
library(lme4)
library(tidyverse)



############################################################################################################
## Step 2: Load data #######################################################################################


# -- Task 2 -- Read in the data file we will be using:

ML.all <- read_csv("subjects.behaviour.words-310114.csv", na = "-999")


# -- And inspect it:

head(ML.all)



############################################################################################################
## Step 3: Tidy data #######################################################################################


# -- We shall be filtering the data and transforming one variable.
# -- We do this work and use data visualization to examine the impacts of the actions.


# -- Task 3 -- Produce a density plot showing word recognition reaction time, correct and incorrect responses

ML.all %>%
  ggplot(aes(x = RT)) +
  geom_density(size=1.5) +
  geom_rug(alpha = .2) +
  ggtitle("Raw RT") +
  theme_bw()  


# -- Task 4 -- You should try out alternative visualisation methods to reveal the patterns in the distribution of
# variables in the ML dataset (or in your own data).

# Take a look at the geoms documented in: 
#   https://ggplot2.tidyverse.org/reference/#section-layer-geoms

# Would a histogram or a frequency polygon provide a more informative view? 
#   https://ggplot2.tidyverse.org/reference/geom_histogram.html

# What about a dotplot? 
#   https://ggplot2.tidyverse.org/reference/geom_dotplot.html


# -- Task 5 -- Filter out incorrect and outlier short RT observations

ML.all.correct <- filter(ML.all, RT >= 200)


# -- Task 6 -- Check out the impact of filtering on the number of rows in the dataset

length(ML.all$RT)
length(ML.all.correct$RT)


# -- Task 7 -- Produce a density plot showing word recognition reaction time, correct responses only

ML.all.correct %>%
  ggplot(aes(x = RT)) +
  geom_density(size=1.5) + 
  geom_rug(alpha = .2) +
  ggtitle("Correct RTs") +
  theme_bw()


# -- Task 8 -- Vary the filter conditions in different ways:
  
# 1. Change the threshold for including RTs from RT >= 200 to something else
# 2. Can you assess what impact the change has? 
# Note that you can count the number of observations (rows) in a dataset using e.g. length()
  
  
# -- Task 9 -- Transform RT to log base 10 RT

ML.all.correct$logrt <- log10(ML.all.correct$RT)			


# -- Task 10 -- Produce a density plot showing log10 transformed reaction time, correct responses only

ML.all.correct %>%
  ggplot(aes(x = logrt)) +
  geom_density(size = 1.5) + 
  geom_rug(alpha = .2) +
  ggtitle("Correct log10 RTs") +
  theme_bw()


# -- Task 11 -- Produce a density plot showing log10 transformed reaction time, correct responses, 
# separately for each participant

ML.all.correct %>%
  mutate(mean_logrt = mean(logrt, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subjectID = fct_reorder(subjectID, mean_logrt)) %>%
  ggplot(aes(x = logrt)) +
  geom_density(size = 1.25) +
  facet_wrap(~ subjectID) +
  geom_vline(xintercept = 2.778807, colour = "red", linetype = 2) +
  scale_x_continuous(breaks = c(2.5,3)) +
  ggtitle("Plot showing distribution of logRT for each participant; red line shows mean log10 RT") +
  theme_bw()


# -- Task 12 -- Can you work out how to adapt the plotting code to show a grid of histograms?


# -- Task 13 -- Can you work out how to adapt the code to show a grid of plots indicating the distribution
# of log RT by different items (instead of participants)?



############################################################################################################
## Step 4: Use lm ##########################################################################################


# -- As we have done before, we can analyze the data to estimate the effect of frequency, at first,
# ignoring the impact of multilevel structure in our data

ML.all.correct.lm  <- lm(logrt ~
                             
                             LgSUBTLCD,     
                           
                           data = ML.all.correct)

summary(ML.all.correct.lm)


# -- Task 14 -- Vary the linear model using different outcomes or predictors

# It would be useful to experiment with the data.
# 1. Change the predictor from frequency to something else
# -- what do you see when you visualize the relationship between outcome and predictor variables 
# using scatterplots?
# 2. Specify linear models with different predictors: do the relationships you see in plots match the 
# coefficients you see in the model estimates?



############################################################################################################
## Step 4: Use lmer ########################################################################################


# -- Task 15 -- fit a linear mixed-effects model to estimate the effect of frequency on log RT
# while taking into account random effects
  
ML.all.correct.lmer  <- lmer(logrt ~

                           LgSUBTLCD +

                           (LgSUBTLCD + 1|subjectID) +

                           (1|item_name),

                         data = ML.all.correct)

summary(ML.all.correct.lmer)



############################################################################################################
## Step 5: Compare models with different random effects ####################################################


# -- Task 16 -- fit a linear mixed-effects model
# -- using REML
# -- with varying random effects

## ------------------------------------------------------------------------------------------------------------------------
ML.all.correct.lmer.REML.si  <- lmer(logrt ~ LgSUBTLCD + 
                                    
                                          (1|subjectID) + (1|item_name),

                                          data = ML.all.correct, REML = TRUE)

summary(ML.all.correct.lmer.REML.si)


## ------------------------------------------------------------------------------------------------------------------------
ML.all.correct.lmer.REML.i  <- lmer(logrt ~

                                          LgSUBTLCD + (1|item_name),

                                          data = ML.all.correct, REML = TRUE)

summary(ML.all.correct.lmer.REML.i)


## ------------------------------------------------------------------------------------------------------------------------
ML.all.correct.lmer.REML.s  <- lmer(logrt ~

                                          LgSUBTLCD + (1|subjectID),

                                          data = ML.all.correct, REML = TRUE)

summary(ML.all.correct.lmer.REML.s)


# -- Task 17 -- Compare the different models using anova()


## ------------------------------------------------------------------------------------------------------------------------
anova(ML.all.correct.lmer.REML.si, ML.all.correct.lmer.REML.i, refit = FALSE)


## ------------------------------------------------------------------------------------------------------------------------
anova(ML.all.correct.lmer.REML.si, ML.all.correct.lmer.REML.s, refit = FALSE)


# -- Task 18 -- We know what our conclusions will be, as these analyses replicate what the chapter does.


# -- Task 19 -- Now examine whether random slopes are required

## ------------------------------------------------------------------------------------------------------------------------
ML.all.correct.lmer.REML.slopes  <- lmer(logrt ~ LgSUBTLCD + 
                                           
                                                (LgSUBTLCD + 1|subjectID) + (1|item_name),

       data = ML.all.correct, REML = TRUE)


## ------------------------------------------------------------------------------------------------------------------------
anova(ML.all.correct.lmer.REML.si, ML.all.correct.lmer.REML.slopes, refit = FALSE)



############################################################################################################
## Step 6: p-values and significance using lmerTest ########################################################


# -- Task 20 -- Get p-values using lmerTest
library(lmerTest)

ML.all.correct.lmer.REML.slopes  <- lmer(logrt ~ LgSUBTLCD + 
                                           
                                                (LgSUBTLCD + 1|subjectID) + (1|item_name),

       data = ML.all.correct, REML = TRUE)

summary(ML.all.correct.lmer.REML.slopes)



############################################################################################################
## Step 7: Now run your own sequence of lmer models ########################################################


# It will be useful for you to examine model comparisons with a different set of models for the same data.

# You could try to run a series of models in which the fixed effects variable is something different, 
# for example, the effect of word Length or the effect of orthographic neighbourhood size Ortho_N.


# I would consider the model comparisons in the sequence shown in the foregoing, one pair of models at a 
# time, to keep it simple.
# When you look at the model comparison, ask: is the difference between the models a piece of complexity 
# (an effect) whose inclusion in the more complex model is justified or warranted by improved model fit 
# to data?



############################################################################################################
# Reproduce the plots in the chapter and slides ############################################################


# Note that to produce the following plots, I adapted Tristan Mahr's code, here:
# https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/


# -- Compare no vs. complete pooling estimates

# -- get no pooling estimates by running lm for each participant
df_no_pooling <- lmList(logrt ~
                          
                          LgSUBTLCD | subjectID, ML.all.correct) %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("subjectID") %>% 
  rename(Intercept = `(Intercept)`, Slope_frequency = LgSUBTLCD) %>% 
  add_column(Model = "No pooling")

# -- you can uncomment and inspect the tibble produced by these lines of code
# summary(df_no_pooling)

# -- make the subjectID and model label vectors a factor
df_no_pooling$subjectID <- as.factor(df_no_pooling$subjectID)
df_no_pooling$Model <- as.factor(df_no_pooling$Model)

# -- in contrast, we might consider a complete pooling model where all the 
# information from the participants is combined together
# -- we fit a single line for all data

# -- fit a model on all the data pooled together
m_pooled <- lm(logrt ~ LgSUBTLCD, ML.all.correct) 

# -- repeat the intercept and slope terms for each participant
df_pooled <- data_frame(
  Model = "Complete pooling",
  subjectID = unique(ML.all.correct$subjectID),
  Intercept = coef(m_pooled)[1], 
  Slope_frequency = coef(m_pooled)[2])

# -- inspect
# summary(df_pooled)

# -- make the model label vector a factor
df_pooled$Model <- as.factor(df_pooled$Model)

# -- we can compare these two approaches
# -- instead of calculating the regression lines with stat_smooth(), we can use 
# geom_abline() to draw the lines from our dataframe of intercept and slope 
# parameters

# -- join the raw data so we can use plot the points and the lines
df_models <- bind_rows(df_pooled, df_no_pooling) %>% 
  left_join(ML.all.correct, by = "subjectID")

# -- produce the plot, as a .pdf
# 
# pdf("ML-data-frequency-no-or-complete-pooling.pdf", w = 6, h = 8)

p_model_comparison <- ggplot(df_models) + 
  aes(x = LgSUBTLCD, y = logrt) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_point(alpha = .05) +
  geom_abline(aes(intercept = Intercept, 
                  slope = Slope_frequency, 
                  color = Model),
              size = .75) +
  facet_wrap(~ subjectID) +
  scale_x_continuous(breaks = 1.5:4 * 1) + 
  theme_bw() + 
  theme(legend.position = "top")

p_model_comparison

# dev.off()


# -- Compare no vs. complete  vs. partial pooling estimates
# -- mixed-effects models represent partial pooling of data

# -- we fit a separate line for each cluster of data, one for each participant 
# -- the lmList() function in lme4 automates this process
df_no_pooling <- lmList(logrt ~
                          
                          LgSUBTLCD | subjectID, ML.all.correct) %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("subjectID") %>% 
  rename(Intercept = `(Intercept)`, Slope_frequency = LgSUBTLCD) %>% 
  add_column(Model = "No pooling")

# -- inspect the tibble produced by these lines of code
# summary(df_no_pooling)

# -- make the subjectID and model label vectors a factor
df_no_pooling$subjectID <- as.factor(df_no_pooling$subjectID)
df_no_pooling$Model <- as.factor(df_no_pooling$Model)

# -- in contrast, we might consider a complete pooling model where all the 
# information from the participants is combined together
# -- we fit a single line for all data

# -- fit a model on all the data pooled together
m_pooled <- lm(logrt ~ LgSUBTLCD, ML.all.correct) 

# -- repeat the intercept and slope terms for each participant
df_pooled <- data_frame(
  Model = "Complete pooling",
  subjectID = unique(ML.all.correct$subjectID),
  Intercept = coef(m_pooled)[1], 
  Slope_frequency = coef(m_pooled)[2])

# -- inspect
# summary(df_pooled)

# -- make the model label vector a factor
df_pooled$Model <- as.factor(df_pooled$Model)

# -- we can compare these two approaches
# -- instead of calculating the regression lines with stat_smooth(), we can use 
# geom_abline() to draw the lines from our dataframe of intercept and slope 
# parameters

# -- join the raw data so we can use plot the points and the lines
# df_models <- bind_rows(df_pooled, df_no_pooling) %>% 
#   left_join(ML.all.correct, by = "subjectID")

# -- we fit a mixed-effects model including the fixed effects of the intercept and the frequency effect
# -- as well as the random effects due to differences between participants (subject ID) in intercepts or
# in the slope of the frequency effect, as well as differences between items (item_name) in intercepts

ML.all.correct.lmer  <- lmer(logrt ~
                               
                               LgSUBTLCD + (LgSUBTLCD + 1|subjectID) + (1|item_name),     
                             
                             data = ML.all.correct, REML = FALSE)
# summary(ML.all.correct.lmer)

# -- to visualize these estimates, we extract each participant’s intercept and slope using coef()

# -- make a dataframe with the fitted effects
df_partial_pooling <- coef(ML.all.correct.lmer)[["subjectID"]] %>% 
  rownames_to_column("subjectID") %>% 
  as_tibble() %>% 
  rename(Intercept = `(Intercept)`, Slope_frequency = LgSUBTLCD) %>% 
  add_column(Model = "Partial pooling")

# -- inspect the tibble produced by these lines of code
# summary(df_partial_pooling)

# -- make the subjectID and model label vectors both factors
df_partial_pooling$subjectID <- as.factor(df_partial_pooling$subjectID)
df_partial_pooling$Model <- as.factor(df_partial_pooling$Model)

# -- update the previous plot by using a dataset consisting of of all three models’ estimates

df_models <- rbind(df_pooled, df_no_pooling, df_partial_pooling) %>%
  left_join(ML.all.correct, by = "subjectID")

# -- inspect the differences between the datasets
# summary(df_pooled)
# summary(df_no_pooling)
# summary(df_partial_pooling)
# summary(ML.all.correct)
# summary(df_models)

# -- replace the data-set used in the last plot with this new data-set
# -- and regenerate the plot
# -- we  produce a plot showing the no-pooling, complete-pooling and partial-pooling (mixed effects)
# estimates

# # pdf("ML-data-frequency-no-or-complete-or-partial-pooling.pdf", w = 6, h = 8)

p_model_comparison <- ggplot(df_models) +
  aes(x = LgSUBTLCD, y = logrt) +
  # Set the color mapping in this layer so the points don't get a color
  geom_point(alpha = .05) +
  geom_abline(aes(intercept = Intercept,
                  slope = Slope_frequency,
                  color = Model),
              size = .75) +
  facet_wrap(~ subjectID) +
  scale_x_continuous(breaks = 1.5:4 * 1) +
  theme_bw() +
  theme(legend.position = "top")

p_model_comparison

# # dev.off()


## Zoom in on a few participants
# -- use filter to select a small number of participants

df_zoom <- df_models %>% 
  filter(subjectID %in% c("EB5", "JP3", "JL3", "AA1"))

# -- replace the data-set of the last plot
# -- re-generate the plot with this zoomed-in selection dataset

# pdf("ML-data-frequency-no-or-complete-or-partial-pooling-zoom.pdf", w = 5, h = 5)

p_model_comparison <- ggplot(df_zoom) + 
  aes(x = LgSUBTLCD, y = logrt) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_point(alpha = .05) +
  geom_abline(aes(intercept = Intercept, 
                  slope = Slope_frequency, 
                  color = Model),
              size = .75) +
  facet_wrap(~ subjectID) +
  scale_x_continuous(breaks = 1.5:4 * 1) + 
  theme_bw() + 
  theme(legend.position = "top")

p_model_comparison


## Illustrate shrinkage

# -- the partial pooling model pulls (or shrinks) more extreme estimates towards an overall average
# -- we can visualize this shrinkage effect by plotting a scatterplot of intercept and slope 
# parameters from each model and connecting estimates for the same participant
# -- we use arrows to connect the different estimates for each participant, different estimates
# from no-pooling (per-participant) compared to partial-pooling (mixed-effects) models
# -- the plot shows how more extreme estimates are shrunk towards the global average estimate

# pdf("ML-data-frequency-no-or-complete-or-partial-pooling-shrinkage.pdf", w = 6.5, h = 6.5)

df_fixef <- data_frame(
  Model = "Partial pooling (average)",
  Intercept = fixef(ML.all.correct.lmer)[1],
  Slope_frequency = fixef(ML.all.correct.lmer)[2])

# Complete pooling / fixed effects are center of gravity in the plot
df_gravity <- df_pooled %>% 
  distinct(Model, Intercept, Slope_frequency) %>% 
  rbind(df_fixef)
# df_gravity
#> # A tibble: 2 x 3
#>                       Model Intercept Slope_Days
#>                       <chr>     <dbl>      <dbl>
#> 1          Complete pooling  252.3207   10.32766
#> 2 Partial pooling (average)  252.5426   10.45212

df_pulled <- rbind(df_no_pooling, df_partial_pooling)

ggplot(df_pulled) + 
  aes(x = Intercept, y = Slope_frequency, color = Model) + 
  geom_point(size = 2) + 
  geom_point(data = df_gravity, size = 5) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = subjectID, color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = subjectID, color = NULL),
    data = df_no_pooling) +
  # Don't forget 373
  # ggrepel::geom_text_repel(
  #   aes(label = subjectID, color = NULL), 
  #   data = filter(df_partial_pooling, Subject == "373")) + 
  theme(legend.position = "right") + 
  # ggtitle("Pooling of regression parameters") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") +
  theme_bw()
