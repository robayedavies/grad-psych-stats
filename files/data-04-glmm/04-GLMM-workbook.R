


############################################################################################################


# The aims of the lab session work are to:
# -- understand the reasons for using Generalized Linear Mixed-effects models (GLMMs) when we 
# analyze discrete outcome variables
# -- recognize the limitations of alternative methods for analyzing such outcomes
# -- practice running GLMMs with varying random effects structures
# -- practice reporting the results of GLMMs, including through the use of model plots

# My recommendations for learning are:
# -- run generalized linear mixed-effects models of demonstration data
# -- run generalized linear mixed-effects models of alternate data sets
# -- play with the .R code used to create examples for the lecture
# -- edit example code to create alternate visualizations



############################################################################################################
## Step 1: Set-up ##########################################################################################


# -- Task 1 -- Run this code to load relevant libraries

library(broom)
library(effects)
library(gridExtra)
library(here)
library(lattice)
library(knitr)
library(lme4)
library(MuMIn)
library(sjPlot)
library(tidyverse)



############################################################################################################
## Step 2: Load data #######################################################################################


# -- Task 2 -- Read in the data files we will be using --

# -- The core example for week 20: the child word learning study, Ricketts et al. (in press):
long.orth <- read_csv("long.orth_2020-08-11.csv", 
                      col_types = cols(
                        Participant = col_factor(),
                        Time = col_factor(),
                        Study = col_factor(),
                        Instructions = col_factor(),
                        Version = col_factor(),
                        Word = col_factor(),
                        Orthography = col_factor(),
                        Measure = col_factor(),
                        Spelling.transcription = col_factor()
                      )
                    )

# -- The additional example: the adult (gavagai) word learning study, Monaghan et al. (2015):
gavagai <- read_csv("noun-verb-learning-study.csv", na = "-999")



############################################################################################################
## Step 3: Tidy data #######################################################################################


# -- We shall need to recode the categorical variables (factors) in  the core example data, long.orth


# -- Task 3 -- Inspect the data --
summary(long.orth)


# -- Q.1.1. -- Look at the summaries for the variables Time, Instructions and Orthography. Assuming that
# the read_csv() action went as required, you will see how R presents factor summaries by default.
# What do the variable summaries tell you about the factor level coding, and the number of observations
# in each level?
# -- Hint Q.1.1. -- For a factor like Orthography, the column values code for whether the
# observations in a data row are associated with the condition absent or the condition present.
# It is simpler to talk about absent or present as being *levels* of Orthography condition.
# And you can ask R what the levels of a factor are:
levels(long.orth$Orthography)


# -- Q.1.2. -- Are there any surprises in the summary of factors?
# -- Hint Q.1.2. -- We would hope to see equal numbers of observations at different levels for a factor.


# -- Task 4 -- Fit a Generalized Linear Model --

# -- You can reproduce the example analysis in the chapter with:
summary(glm(Score ~ Orthography, family = "binomial", data = long.orth))


# -- Q.2.1. -- What is the estimated effect of Instructions on Score?
# -- hint: Q.2.1. -- Fit a model with Instructions as the fixed effect

# -- write your code here and run it --

# -- Q.2.2. -- Can you briefly indicate in words what the estimate says about how log odds Score 
# (response correct vs. incorrect) changes in association with different Instructions conditions?


# -- Task 5 -- Use the memisc library and recode the factors, to use contrast sum (effect) coding --

# -- Loading the memisc library may avoid triggering warnings as seen by some, previously, we will see:
library(memisc)

# -- see information about the potential warnings here:
# https://github.com/melff/memisc/issues/47  
  
  
# -- Now use the following code to first check then change the factor coding for each factor

## -----------------------------------------------------------------------------------------
contrasts(long.orth$Orthography)

## -----------------------------------------------------------------------------------------
contrasts(long.orth$Orthography) <- contr.sum(2, base = 1)
contrasts(long.orth$Orthography)

## -----------------------------------------------------------------------------------------
contrasts(long.orth$Instructions)

## -----------------------------------------------------------------------------------------
contrasts(long.orth$Instructions) <- contr.sum(2, base = 2)
contrasts(long.orth$Instructions)

## -----------------------------------------------------------------------------------------
contrasts(long.orth$Time)

## -----------------------------------------------------------------------------------------
contrasts(long.orth$Time) <- contr.sum(2, base = 1)
contrasts(long.orth$Time)


# -- You can check out the manual information here:
# https://rdrr.io/cran/memisc/man/contrasts.html
# https://www.elff.eu/software/memisc/manual/contrasts/  


# -- Q.3.1. -- Experiment: what happens if you change the first number for one of the factors?
# contrasts(long.orth$Time) <- contr.sum(3, base = 1)
# contrasts(long.orth$Time)

# -- Q.3.2. -- Experiment: what happens if you change the base number for one of the factors?
# contrasts(long.orth$Time) <- contr.sum(2, base = 1)
# contrasts(long.orth$Time)



############################################################################################################
## Step 3: Analyze the data: random intercepts #############################################################


# -- Task 6 -- We can take a quick look at a random intercepts model --
# -- Run the code to fit the model and get a summary of results, as shown in the chapter

long.orth.min.glmer <- glmer(Score ~ 
                               Time + Orthography + Instructions + zConsistency_H + 
                               
                               Orthography:Instructions +
                               
                               Orthography:zConsistency_H +
                               
                               (1 | Participant) + 
                               
                               (1 | Word),
                             
                             family = "binomial", 
                             glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                             
                             data = long.orth)

summary(long.orth.min.glmer)


# -- Q.4.1. -- Experiment: replace the fixed effects with another variable e.g. mean_z_read (an aggregate 
# measure of reading skill) to get a feel for how the code works.
# -- What is the estimate for the mean_z_read effect?

# -- write your code here and run it --

# -- Q.4.2. -- Can you briefly indicate in words what the estimate says about how log odds Score 
# (response correct vs. incorrect) changes in association with mean_z_read score?


# -- Task 7 -- Visualize the significant effects of the Orthography and Consistency factors --

# First using sjPlot plot_model() function
porth <- plot_model(long.orth.min.glmer,
           type="pred",
           terms = "Orthography") +
         theme_bw() +
         ggtitle("Predicted probability") +
         ylim(0,1)

pzconsH <- plot_model(long.orth.min.glmer,
           type="pred",
           terms = "zConsistency_H") +
         theme_bw() +
         ggtitle("Predicted probability") +
         ylim(0,1)

grid.arrange(porth, pzconsH,
            ncol=2)

# Second using the effects library effect() function
porth <- plot(effect("Orthography", mod = long.orth.min.glmer))

pzconsH <- plot(effect("zConsistency_H", mod = long.orth.min.glmer))

grid.arrange(porth, pzconsH,
            ncol=2)


# -- Task 8 -- Pick a different predictor variable to visualize
# -- Hint Task 8 -- Notice that in the preceding code chunks, I assign the plot objects to names
# and then use grid.arrange to present the named plots in grids.
# To produce and show a plot, don't do that, just adapt and use the plot functions.

# -- write your code here and run it --


# -- Task 9 -- optional -- Can you figure out how to plot interaction effects?
# -- Hint Task 9 -- Take a look at the documentation referenced in the book chapter
# https://strengejacke.github.io/sjPlot/articles/plot_interactions.html

# -- write your code here and run it --



############################################################################################################
## Step 4: Analyze the data: model comparisons #############################################################


# -- Task 10 -- We are now going to fit a series of models and evaluate their relative fit --

# -- Note that the models all have the same fixed effects.
# -- Note also that while we will be comparing models varying in random effects (see Week 19 materials)
# we are not going to use REML=TRUE
# -- See here for why:
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#reml-for-glmms


# -- First the minimum random intercepts model
# -- Run the code to fit the model and get a summary of results, as shown in the chapter
long.orth.min.glmer <- glmer(Score ~ 
                               Time + Orthography + Instructions + zConsistency_H + 
                               
                               Orthography:Instructions +
                               
                               Orthography:zConsistency_H +
                               
                               (1 | Participant) + 
                               
                               (1 |Word),
                             
                             family = "binomial", 
                             glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                             
                             data = long.orth)

summary(long.orth.min.glmer)


# -- Second the maximum random intercepts model: this will take several seconds
# -- Run the code to fit the model and get a summary of results, as shown in the chapter
long.orth.max.glmer <- glmer(Score ~ 
                           Time + Orthography + Instructions + zConsistency_H + 
                           
                           Orthography:Instructions +
                           
                           Orthography:zConsistency_H +
                           
                           (Time + Orthography + zConsistency_H + 1 | Participant) + 
                           
                           (Time + Orthography + Instructions + 1 |Word),
                         
                         family = "binomial",
                         glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                         
                         data = long.orth)

summary(long.orth.max.glmer)

# -- Then models adding random effects a bit at a time --

# -- Add orthography
long.orth.2.glmer <- glmer(Score ~ 
                             Time + Orthography + Instructions + zConsistency_H + 
                             
                             Orthography:Instructions +
                             
                             Orthography:zConsistency_H +
                             
                             (dummy(Orthography) + 1 || Participant) + 
                             
                             (dummy(Orthography) + 1 || Word),
                           
                           family = "binomial", 
                           glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                           
                           data = long.orth)

summary(long.orth.2.glmer)

# -- Add Instructions
long.orth.3.glmer <- glmer(Score ~ 
                             Time + Orthography + Instructions + zConsistency_H + 
                             
                             Orthography:Instructions +
                             
                             Orthography:zConsistency_H +
                             
                             (dummy(Orthography) + 1 || Participant) + 
                             
                             (dummy(Orthography) + dummy(Instructions) + 1 || Word),
                           
                           family = "binomial", 
                           glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                           
                           data = long.orth)

summary(long.orth.3.glmer)

# -- Models 2 and 3 should both converge

# -- Add consistency
long.orth.4.a.glmer <- glmer(Score ~ 
                             Time + Orthography + Instructions + zConsistency_H + 
                             
                             Orthography:Instructions +
                             
                             Orthography:zConsistency_H +
                             
                             (dummy(Orthography) + zConsistency_H + 1 || Participant) + 
                             
                             (dummy(Orthography) + dummy(Instructions) + 1 || Word),
                           
                           family = "binomial", 
                           glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                           
                           data = long.orth)

summary(long.orth.4.a.glmer)

# -- Add Time
long.orth.4.b.glmer <- glmer(Score ~ 
                             Time + Orthography + Instructions + zConsistency_H + 
                             
                             Orthography:Instructions +
                             
                             Orthography:zConsistency_H +
                             
                             (dummy(Orthography) + dummy(Time) + 1 || Participant) + 
                             
                             (dummy(Orthography) + dummy(Instructions) + dummy(Time) + 1 || Word),
                           
                           family = "binomial", 
                           glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                           
                           data = long.orth)

summary(long.orth.4.b.glmer)

# -- Models 4.a. and 4.b. should not converge


# -- Task 11 -- Now compare the models that converge: min vs. 2, vs. 3 --
anova(long.orth.min.glmer, long.orth.2.glmer)
anova(long.orth.min.glmer, long.orth.3.glmer)


# -- Task 12 -- Before we move on, check out some of the code adjustments we have used --
  
# -- Q.5.1. -- What does using bobyqa do? Delete glmerControl() and see what impact that has.
# -- Increase the number of iterations, maxfun

# -- write your code here and run it --

# -- Q.5.2. -- What does using dummy() in the random effects coding do?
# -- Experiment: check out models 2 or 3, removing dummy() from the random effects coding to see.

# -- write your code here and run it --



############################################################################################################
## Step 4: Exercise analyses ###############################################################################


# -- Task 13 -- Now analyze the gavagai adult word learning data --

#-- Read the data in
gavagai <- read_csv("noun-verb-learning-study.csv", na = "-999")

# -- Create a centered learning block numeric variable
gavagai$cblock <- scale(gavagai$block, scale = FALSE, center = TRUE)

# -- Fit a random intercepts model
gavagai.glmer <- glmer(accuracy ~

          Experiment*cblock +

          (1|Subjecta) + (1|targetobject) + (1|targetaction),
          data = gavagai, family = binomial,
          glmerControl(optimize = "bobyqa"))

summary(gavagai.glmer)

# -- Now, using the study information and a series of model comparisons, can you select
# a model with a comprehensive defensible random effects structure?



############################################################################################################
# Reproduce the plots in the chapter and slides ############################################################


# This plot concerning the gavagai data is enlightening about the limits of using lm to model accuracy
# -- each point shows per-subject accuracy proportion per block
# -- each thin line shows the trend per-subject mis-estimated by lm rather than logit GLM but indicative
# -- the thick lines show the group trend                                                                           
# -- the plot pretty much shows what the original arcsine analysis was doing
# -- we can edit the plot for use

# -- first, we need to calculate the proportion of responses that are correct per person for each block of (24) learning trials

correct <- gavagai %>%
  group_by(Experiment, Subjecta, block) %>%                                 
  # group data by subject ID (Subjecta) and block, to get summary of
  # accuracy for each subject for each block, then
  summarise(sum = sum(accuracy)) %>%
  # calculate accuracy of responses per person per block then
  mutate(proportion = sum/24)

# -- then we do the plot
pcorrect <- ggplot(correct, aes(x = block, y = proportion))
pcorrect + 
  geom_jitter(alpha = .3) + # jitter points, adding random noise to x,y coordinates; use alpha() to modify transparency
  geom_smooth(aes(group = Subjecta), method="lm", se = F, alpha = .65, colour = "darkgrey") + # add a learning model line for each person
  geom_smooth(aes(group = 1), method="lm", se = F, size = 2, colour = "black") + # add a line to show average group learning
  scale_x_continuous(breaks = seq(2,12, by = 2)) + xlab("Block") + # modify scale axis labeling
  scale_y_continuous(breaks = seq(0,1, by = .2)) +  ylab("Proportion correct") + # modify scale axis labeling
  theme_bw() + # use black and white theme
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), axis.text = element_text(size = 15)) +
  facet_grid(.~Experiment) + # create a separate plot for each experimental condition
  theme(strip.text.x = element_text(size = 20)) + theme(strip.background = element_rect(colour = "grey")) # modify appearance of subplots

