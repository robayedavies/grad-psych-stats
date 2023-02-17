

############################################################################################################


# In this workbook, our learning targets are:

# -- concepts: multilevel data and multilevel modeling
# -- skills: visualization -- examine overall and within-class trends
# -- skills: run linear models over all data -- and within each class
# -- skills: use the lmer() function to fit models of multilevel data


# The aims of the lab session work are:
# -- get practice running the code: so that you can reproduce the figures and results from the lecture and in 
# the book chapter
# -- exercise skills by varying code -- changing variables, changing options -- so that you can see how the code works
# -- use the opportunity to reflect on and evaluate results -- so that we can support growth in development of 
# understanding of main ideas



############################################################################################################
## Step 1: Set-up ##########################################################################################


# -- Task 1 -- Run this code to load relevant libraries

library(broom)
library(gridExtra)
library(lme4)
library(tidyverse)



############################################################################################################
## Step 2: Load data #######################################################################################


# -- Task 2 -- Read in the data file we will be using: BAFACALO_DATASET.RData

load("BAFACALO_DATASET.RData")


# -- Inspect what you have:

summary(BAFACALO_DATASET)



############################################################################################################
## Step 3: Tidy data #######################################################################################


# -- Tidying the data involves a number of tasks, some essential and some things we do
# for convenience


# -- Task 3 -- We start by selecting the variables we want
# -- We use the dplyr function select() to pick just the variables needed

brazil <- BAFACALO_DATASET %>%
                              select(
                              class_number, participant_id,
                              portuguese, english, math, physics
                              )

# -- inspect:

summary(brazil)


# -- Q.1. -- Do you know what you are doing with select()? Can you select a different set of variables?
# -- A.1. -- You can exercise your skill by checking that you can view the BAFACALO_DATASET and select
# any variable you like. Be careful and make sure you go back to select the variables shown in the
# given code chunk: we will need them later.


# -- Task 4 -- Take missing values out of the brazil data

brazil <- na.omit(brazil)

# -- inspect:

summary(brazil)


# -- Q.2. -- Do you see the difference between the summary of the brazil data shown before and after you
# run na.omit()? What is it?
# -- A.2. -- You should see that NA's are listed in some columns in the data before but not after you
# run na.omit().


# -- Task 5 -- Get R to treat a variable as a type object of the kind required -- using the 
# as.numeric() or as.factor() functions

brazil$portuguese <- as.numeric(brazil$portuguese)
brazil$english <- as.numeric(brazil$english)
brazil$math <- as.numeric(brazil$math)
brazil$physics <- as.numeric(brazil$physics)

brazil$class_number <- as.factor(brazil$class_number)
brazil$participant_id <- as.factor(brazil$participant_id)

# -- inspect:

summary(brazil)	


# -- Q.3. -- Do you see the difference between the summary of the brazil data shown before and after you
# run as.numeric() or as.factor() function lines? What is it?
# -- A.3. -- You should see that numeric variables like portuguese are listed with summary statistics
# like the mean after but not before you coerce the variables.


# -- Task 6 -- Exercise -- experiment with coercion
# 1. Test out variable type using the is...() function for some of the variables
# 2. Test out coercion -- and its results -- using the as...() function for some of the variables
# 3. Look at the results using summary()




############################################################################################################
## Step 4: Visualize relationship ##########################################################################


# -- Task 7 -- Visualize the relationship between portuguese and english grades using a scatterplot

brazil %>%
  ggplot(aes(x = portuguese, y = english)) +
  geom_point(colour = "black", size = 2.5, alpha = .5) +
  geom_smooth(method = "lm", size = 1.5, se = FALSE, colour = "red") +
  xlab("Portuguese") + ylab("English") + theme_bw() 


# -- Task 8 -- Exercise -- edit plots

# 1. Change the x and y variables to math and physics
# 2. Change the theme from theme_bw() to something different
# 3. Change the appearance of the points, try different colour, shape, size

# -- Hint -- Task 8 -- Use the ggplot reference documentation to help you make choices:
#   https://ggplot2.tidyverse.org/reference/
# -- You should be using webpages like the reference often.



############################################################################################################
## Step 5: Analyze relationship lm #########################################################################


# -- Task 9 -- Analyze the relationship between english and portuguese grades in the brazil data
# -- You should be able to reproduce the results shown in the slides and the book chapter.

summary(lm(english ~ portuguese, data = brazil))


# -- Task 10 -- Exercise -- adapt the lm() code to do a different analysis
# -- Change the outcome and predictor variables to math and physics

summary(lm(physics ~ math, data = brazil))


# -- Q.4 -- What is the estimated coefficient of the "effect" of math ability (grade) on physics grade?
# -- A.4. -- The summary shows that physics grades are on average 1.3 higher for unit increase in
# math grade. 


# -- Q.5. -- Draw a scatterplot showing the relationship between math and physics grades. Does
# the trend you see in the plot reflect the coefficient you see in the linear model summary?
# -- A.5. -- The plot shows the positive association between math and physics grade also indicated
# by the estimated coefficient of the math effect.

brazil %>%
  ggplot(aes(x = math, y = physics)) +
  geom_point(colour = "black", size = 2.5, alpha = .5) +
  geom_smooth(method = "lm", size = 1.5, se = FALSE, colour = "red") +
  theme_bw() 


# -- Q.6. -- How does the strength of the math-physics relationship compare with the english-portuguese
# relationship?
# -- Q.7. -- Both the linear model and the plots indicate that the math-physics relationship is 
# much stronger.



############################################################################################################
## Step 6: visualize relationship for each class ###########################################################


# -- Task 11 -- Plot the relationship between english and portuguese grades separately for each class
# using facet_wrap

ggplot(data = brazil, aes(x = portuguese, y = english)) +
  geom_point(colour = "darkgrey") + geom_smooth(method = "lm", se = FALSE, colour = "black") +
  xlab("Portuguese") + ylab("English") + theme_bw() +
  scale_x_continuous(breaks=c(25,50,75)) + scale_y_continuous(breaks=c(0,50,100)) +
  facet_wrap(~ class_number) 


# -- Task 12 -- Exercises to practice your facet_wrap() skills
# 1. Change the x and y variables to math and physics and draw a facetted scatterplot again
# 2. Experiment with showing the differences between classes in a different way: instead of using
# facet_wrap(), in aes() add colour = class_number, and remove colour from geom_point and
# geom_smooth

ggplot(data = brazil, aes(x = math, y = physics)) +
  geom_point(colour = "darkgrey") + geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~ class_number) 

ggplot(data = brazil, aes(x = math, y = physics, colour = class_number)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)


# -- Q.8. -- Evaluate the consistency between classes of the relationship between math and physics
# grades: what do the plots show? how does this compare with what you see of the relationship
# between english and portuguese grades?
# -- A.8. -- The plots show that the relationship between math and physics is very consistent
# between classes, and more consistent than the relationship between english and portuguese
# grades appears to be.



############################################################################################################
## Step 7: mixed-effects analysis ##########################################################################


# -- Task 13 -- Run a linear mixed-effects analysis of the relationship between english and portuguese
# grades using lmer()
# You should be able to replicate the results shown in the slides and the book chapter.

porto.lmer1 <- lmer(english ~ portuguese +
                      (portuguese + 1|class_number),
                      data = brazil)
summary(porto.lmer1)


# -- Task 14 - Exercise mixed-effects model coding
# 1. Vary the random effects part of the model, while keeping this bit the same:
# lmer(english ~ portuguese +
# 1.1. Change the random effect from (portuguese + 1 | class_number) to
# (1 | class_number) -- what you are doing is asking R to ignore the differences in the slope of the effect of Portuguese grades
# 1.2. Change the random effect from (portuguese + 1 | class_number) to
# (portuguese + 0 | class_number): -- what you are doing is asking R to ignore the differences 
# in the intercept

porto.lmer1 <- lmer(english ~ portuguese +
                      (portuguese + 1|class_number),
                    data = brazil)
summary(porto.lmer1)

porto.lmer2 <- lmer(english ~ portuguese +
                      (1|class_number),
                    data = brazil)
summary(porto.lmer2)

porto.lmer3 <- lmer(english ~ portuguese +
                      (portuguese + 0|class_number),
                    data = brazil)
summary(porto.lmer3)


# -- Q.9. -- Compare the results of the different versions of the model. Can you identify where
# the results are different?
# -- A.9. -- It can be seen that:
# -- The estimated effect of portuguese varies between the models but the estimate is more similar,
# around .65, where the random effect is specified as 
# (portuguese + 1|class_number) or
# (portuguese + 0|class_number)
# -- The residual variance term is different between the models.
# -- Which random effects variances are shown is also different.
# -- There is a convergence warning for:
#   english ~ portuguese + (portuguese + 0 | class_number)  


# -- Task 154 - Exercise mixed-effects model coding
# Change the outcome (from english) and the predictor (from portuguese) -- this is about changing
# the fixed effect part of the model
# -- Note that you will need to change the random effect part as well.

porto.lmer1 <- lmer(physics ~ math +
                      (math + 1|class_number),
                    data = brazil)
summary(porto.lmer1)


# -- Q.10. -- What elements of the model summary stand out for you?
# -- It will help to see what you should notice if you compare the math-physics model with the
# first english-portuguese model.
# -- A.10. -- You may notice that:
# -- The sumary comes with a fit is singular? warning.
# -- The variance terms for intercept or the math effect by class number and the residual
# are very very small: much smaller than for the english-portuguese model.



############################################################################################################
## Step 8: Extension #######################################################################################


# In the lecture materials, I display a plot showing the estimated intercept and coefficient for 
# each class, estimated using separate models for different classes.
# -- Some of you may be interested in how I did that, you can run the following code to see.

# -- use the dplyr %>% syntax to run a model for each class separately, collect together the results into a dataframe
brazlm <- brazil %>% group_by(class_number) %>% do(tidy(lm(english ~ portuguese, data=.)))
brazlm$term <- as.factor(brazlm$term)

# -- extract the per-class estimates of the intercepts and the 'portuguese' effect coefficient estimates
brazlmint <- filter(brazlm, term == '(Intercept)')
brazlmport <- filter(brazlm, term == 'portuguese')

# -- plot the estimates
pbrazlmint <- ggplot(brazlmint, aes(x = class_number, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error))
pbrazlmint <- pbrazlmint + geom_point(size = 2) + geom_linerange() + theme_bw() 
pbrazlmint <- pbrazlmint + ggtitle("Intercept") + ylab("Estimated coefficient +/- SE") + xlab("Class")
pbrazlmint <- pbrazlmint + theme(axis.title.y = element_text(size = 10), axis.text.x = element_blank(), panel.grid = element_blank())
# pbrazlmint

pbrazlmport <- ggplot(brazlmport, aes(x = class_number, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error))
pbrazlmport <- pbrazlmport + geom_point(size = 2) + geom_linerange() + theme_bw() 
pbrazlmport <- pbrazlmport + ggtitle("Portuguese effect") + ylab("Estimated coefficient +/- SE") + xlab("Class")
pbrazlmport <- pbrazlmport + theme(axis.title.y = element_text(size = 10), axis.text.x = element_blank(), panel.grid = element_blank())
# pbrazlmport

# -- ask R to make a grid:

grid.arrange(pbrazlmint, pbrazlmport,
             ncol = 2)


# Can you change the code to show the estimates for the relationship between physics and math grades?



############################################################################################################


