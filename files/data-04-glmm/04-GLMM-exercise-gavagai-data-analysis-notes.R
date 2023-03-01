


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
gavagai <- read_csv(file="noun-verb-learning-study.csv", na = "-999")

# -- inspect 
summary(gavagai)

# -- Note that:
# -- the outcome variable is accuracy
# -- we have a variable, Experiment, coding for experimental condition, with conditions nounonly, nounverb, verbonly
# -- we have a variable, block, coding for learning trial block (blocks 1-12)
# -- we have a variable coding for subject, Subjecta
# -- we have variables coding for two kinds of items, targetobject, and targetaction


# -- Task 3 -- Make sure response accuracy gets treated as a factor (necessary for some functions) --

# -- Note that accuracy gets treated as a number, a continuous numeric variable, because correct vs. incorrect responses are coded using numbers
# -- but we need the accuracy variable to be treated as a categorical variable, a factor: so we use coercion with as.factor()
gavagai$accuracy <- as.factor(gavagai$accuracy)



############################################################################################################
## Step 2: Plot data #######################################################################################


# -- Task 4 -- Plot the distribution of accuracy outcome (0,1) over different conditions --


# -- We can start by plotting the outcome variable for each experimental condition:
# -- from blocks 1 to 12 of learning trials
# -- where stimuli to be learnt are: noun only; verb only; noun and verb together

pdf("gavagai-accuracy-bar chart.pdf", w = 12, h = 6)

paccuracy <- ggplot(data = gavagai, aes(x = accuracy, fill = accuracy, colour = accuracy))
paccuracy + 
  geom_bar() + 
  theme_bw() +
  theme(axis.title.x = element_text(size=25)) + 
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=25)) + 
  theme(axis.text.y = element_text(size=20)) +
  ggtitle("Response accuracy") + 
  theme(title = element_text(size=30)) +
  facet_grid(Experiment ~ block)

dev.off()


# I think a second plot is more enlightening
# -- each point shows per-subject accuracy proportion per block
# -- each thin line shows the trend per-subject misestimated by lm rather than logit GLM but indicative
# -- the thick lines show the group trend                                                                           
# -- the plot pretty much shows what the original arcsine analysis was doing
# -- we can edit the plot for use

# -- First, we need to calculate the proportion of responses that are correct per person for each block of (24) learning trials
# -- before we can do this we need to turn accuracy  back into a number
gavagai$accuracy <- as.numeric(gavagai$accuracy)
correct <- gavagai %>%
                      group_by(Experiment, Subjecta, block) %>%                                 
                      # group data by subject ID (Subjecta) and block, to get summary of accurayc for each subject for each block, then
                      summarise(sum = sum(accuracy)) %>%
                      # calculate accuracy of responses per person per block then
                      mutate(proportion = sum/24)
# -- inspect:
summary(correct)

# -- Then we produce a plot, showing the effects of experimental condition (Experiment: noun only, noun and verb, verb only)
# and learning trial block, on accuracy of responses

pdf("word-learning-lm-per-subject-for-report.pdf", width = 12, height = 12)

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

dev.off()



############################################################################################################
## Step 3: Analyze the data ################################################################################


# -- Task 4 -- Start with GLMM analyses --

# -- We can use to multilevel modeling to take into account the random effects of subjects on the effects of block 
# and experimental condition:                                                                          
# we can get generalized mixed effect models incorporating random effects of subjects and item objects and actions

# -- Note that we start with a model with random intercepts, estimating the random effects of subjects and of
# both object and action stimuli on intercepts.
# -- In the fixed effects part, we specify the model to allow interaction between effects of experimental condition 
# and cblock: we get estimates of the main effects and of the interaction effect.

# -- Before we do any modeling, we center the blocks variable:
gavagai$cblock <- scale(gavagai$block, scale = FALSE, center = TRUE)
# -- And make sure accuracy is treated as a factor:  
gavagai$accuracy <- as.factor(gavagai$accuracy)

# -- Then we use the model code -- the model will take several seconds to run:
gavagai.min.glmm <- glmer(accuracy ~ 
                                   
                                   Experiment*cblock +
                                   
                                   (1|Subjecta) + (1|targetobject) + (1|targetaction),    
                                 
                                 data = gavagai, 
                                 family = binomial, 
                                 glmerControl(optimize = "bobyqa"))

summary(gavagai.min.glmm)

# -- Your results should look like this:
# > summary(gavagai.min.glmm)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: accuracy ~ Experiment * cblock + (1 | Subjecta) + (1 | targetobject) +  
#   (1 | targetaction)
# Data: gavagai
# Control: glmerControl(optimize = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 16887.8  16955.6  -8434.9  16869.8    13815 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.4867 -1.0120  0.4819  0.7795  1.7579 
# 
# Random effects:
#   Groups       Name        Variance Std.Dev.
# Subjecta     (Intercept) 0.41607  0.6450  
# targetobject (Intercept) 0.01274  0.1129  
# targetaction (Intercept) 0.01667  0.1291  
# Number of obs: 13824, groups:  Subjecta, 48; targetobject, 9; targetaction, 9
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.765843   0.213380   3.589 0.000332 ***
#   Experimentnounverb         0.003108   0.270740   0.011 0.990841    
# Experimentverbonly        -0.404870   0.295550  -1.370 0.170723    
# cblock                     0.128433   0.009778  13.135  < 2e-16 ***
#   Experimentnounverb:cblock -0.004128   0.013877  -0.297 0.766094    
# Experimentverbonly:cblock -0.077470   0.013215  -5.862 4.56e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Exprmntn Exprmntv cblock Exprmntn:
#   Exprmntnnvr -0.761                                   
# Exprmntvrbn -0.722  0.575                            
# cblock       0.022 -0.017   -0.016                   
# Exprmntnnv: -0.015  0.023    0.011   -0.704          
# Exprmntvrb: -0.016  0.012    0.014   -0.740  0.521  



############################################################################################################
## Step 4: Visualize effects ###############################################################################


# -- Task 5 -- Plot effects as model predictions, given estimates --

# -- Plotting the effects can be very helpful in interpreting them
# -- Yhere are different libraries available for doing this
# -- Which library you use will depend on how easy you find it to use and how effective
# the visualisation is for you or your target audience


# -- First, we use the languageR function plotting function plotLMER.fnc()
# -- run the library call if you have not done so already
library(languageR)
# -- Check the languageR manual for details:
# https://cran.r-project.org/web/packages/languageR/languageR.pdf

# -- This will get us all main effects:
plotLMER.fnc(gavagai.min.glmm, fun=plogis)

# -- This will get us a plot of the interaction between the effects of experiment and the effect of learning block
pdf("noun-verb-learning-glmm-partial-effects-languageR-plot.pdf", w = 5, h = 5)

plotLMER.fnc(gavagai.min.glmm, pred = "cblock", 
             intr=list("Experiment", c("nounonly", "verbonly", "nounverb"), "end"),
             fun=plogis, addlines = TRUE,
             cex.main=2, cexsize = 1.2
             )

dev.off()


# -- Then we use the effects package functions
# -- check the effects manual for details:
#   https://cran.r-project.org/web/packages/effects/effects.pdf
# -- see also:
#   https://www.jstatsoft.org/article/view/v008i15  

# -- (centered block) for each experiment condition, in different sub-plots:

plot(effect("Experiment*cblock", mod = gavagai.min.glmm), 
     multiline = FALSE, # change this argument from FALSE to TRUE to show the different block effects per experiment condition all in one plot
     rug = FALSE,
     ylab = "Probability (correct response)"
     )


# -- Then we use the sjPlot library functions
library(sjPlot)
# -- install the sjPlot package first 
# -- see:
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_marginal_effects.html
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# https://cran.r-project.org/web/packages/sjPlot/vignettes/blackwhitefigures.html
# https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf
# -- for details

pdf("noun-verb-learning-glmm3-partial-effects-sjplot-plot.pdf", w = 6, h = 5)

plot_model(gavagai.min.glmm, type = "pred", terms = c("cblock", "Experiment"))

dev.off()


# -- You can turn the colour off and use gray scale and varying line type for different effects,
# if required for report
plot_model(gavagai.min.glmm, type = "pred", terms = c("cblock", "Experiment"), colors = "bw")


# -- Note that you can edit the plot appearance using ggplot() functions: see the Week 20 chapter code for examples.



############################################################################################################
## Step 4: Compare models varying in random effects ########################################################


# -- Task 6 -- Work through a series of model variants, and check for convergence, model fit --


# -- In this model, we have just random effects of participants or items on intercepts
gavagai.min.glmm <- glmer(accuracy ~ 
                        
                        Experiment*cblock +
                        
                        (1|Subjecta) + 
                        (1|targetobject) + (1|targetaction),    
                      
                      data = gavagai, 
                      family = binomial, 
                      glmerControl(optimizer = "bobyqa"))
summary(gavagai.min.glmm)


# -- add random effects of subjects on the slope of the block effect
gavagai.sblocks <- glmer(accuracy ~ 
                                
                                Experiment*cblock +
                                
                                (cblock + 1|Subjecta) + 
                                (1|targetobject) + (1|targetaction),    
                              
                              data = gavagai, 
                              family = binomial, 
                              glmerControl(optimizer = "bobyqa"))
summary(gavagai.sblocks)


# -- add random effects of subjects and items on the slope of the block effect
gavagai.siblocks <- glmer(accuracy ~ 
                                 
                                 Experiment*cblock +
                                 
                                 (cblock + 1|Subjecta) + 
                                 (cblock + 1|targetobject) + 
                                 (cblock + 1|targetaction),    
                               
                               data = gavagai, 
                               family = binomial, 
                               glmerControl(optimizer = "bobyqa"))
summary(gavagai.siblocks)


# -- add random effects of subjects on the slope of the experiment effect
gavagai.siblocks.sexpt <- glmer(accuracy ~ 
                                       
                                       Experiment*cblock +
                                       
                                       (Experiment + cblock + 1|Subjecta) + 
                                       (cblock + 1|targetobject) + 
                                       (cblock + 1|targetaction),    
                                     
                                     data = gavagai, 
                                     family = binomial, 
                                     glmerControl(optimizer = "bobyqa"))
summary(gavagai.siblocks.sexpt)

# -- Notice that the last model takes a long time then almost fails to converge


# -- Task 6 -- Compare models: now we can check what model complexity significantly contributes to model fit --

# -- Does the addition of random slopes improve model fit?
# -- Take it one comparison, one addition, at a time:
anova(gavagai.min.glmm, gavagai.sblocks)
anova(gavagai.sblocks, gavagai.siblocks)
anova(gavagai.siblocks, gavagai.siblocks.sexpt)

# -- Or do all comparisons at once
anova(gavagai.min.glmm, gavagai.sblocks, 
      gavagai.siblocks, gavagai.siblocks.sexpt)



############################################################################################################
## Step 5: Present the model results #######################################################################


# -- Task 7 -- Visualize to present and check results --


# -- You will have seen that the model comparison results suggest that the following model may be the most parimonious
# adequate fit to the data -- given our predictors, and our sample data

gavagai.sblocks <- glmer(accuracy ~ 
                                
                                Experiment*cblock +
                                
                                (cblock + 1|Subjecta) + 
                                (1|targetobject) + (1|targetaction),    
                              
                              data = gavagai, 
                              family = binomial, 
                              glmerControl(optimizer = "bobyqa"))
summary(gavagai.sblocks)  
  

# -- Presenting the model:-
# -- We can present the model estimates in a summary table
# -- We may wish to present coefficient estimates for the fixed effects along with SEs, z and Wald p-values
# -- We may also wish to present confidence intervals
# -- We may wish to report R^2 estimates using the Johnson-Nakagawa-Schielzeth method, with the MuMIn function
# -- Finally, we may wish to present plots showing the model partial effects


# -- Take the last model and get a summary of the fixed and random effects
summary(gavagai.sblocks)


# -- We can get confidence intervals using different methods:
confint(gavagai.sblocks, method = "Wald") # -- less accurate, approximately correct, more for larger samples, quick
# confint(gavagai.sblocksa, method = "profile") # -- more accurate, less restrictive assumptions than wald, slower
# confint(gavagai.sblocks, method = "boot") # -- more accurate as makes no distribution assumptions but can take a long time, uncomment and try it if you have hours to spare


# -- We can calculate R^2 using the MuMIn r.squaredGLMM() function, following the approach outlined by Nakagawa & 
# Schielzeth (2013) and Johnson (2014)
library(MuMIn)
r.squaredGLMM(gavagai.sblocks)


# -- Plotting the effects can be very helpful in interpreting them:-
plot_model(gavagai.sblocks, type = "pred", terms = c("cblock", "Experiment"), colors = "bw")


# -- It is useful to consider the random effects:-
# -- The adjustments that our final model predicts are required to explained the unexplained differences between
# subjects or items in the intercept or the slope of the effect of learning block

# -- We can plot the random effects predictions as follows:
# http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot

# -- First defining a caterpillar plot function: NB changed original to use darkgrey not blue for points
# -- Select and run all code between ggCaterpillar and }

ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="darkgrey") 
    return(p)
  }
  
  lapply(re, f)
}


# -- Notice that we get a series of plots in the plot window -- ending with the last plot showing the item random effect 
# -- so require scroll back through plots to get the subject effects:

# -- Showing standard normal quantiles
ggCaterpillar(ranef(gavagai.sblocks, condVar=TRUE))

# -- Or predictions per group (subject or item):
ggCaterpillar(ranef(gavagai.sblocks, condVar=TRUE), QQ=FALSE)

# -- We can see the raw random effects numbers as:
ranef(gavagai.sblocks)

# -- Or predictions per group (subject or item): just random effects by subjects -- using the $grouping.variable addition

pdf("noun-verb-learning-glmm-ggcaterpillar-subjects.pdf", w = 5, h = 10)

ggCaterpillar(ranef(gavagai.sblocks, condVar=TRUE), QQ=FALSE)$Subjecta

dev.off()


