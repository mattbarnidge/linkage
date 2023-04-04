#Load Libraries
library(dplyr)
library(tidyr)

#Set WD and load data
setwd("~/Documents/GitHub/linkage/INE")
load("ine_noMI.Rdata")

#Correlations among indicators of involvement (.32 < r < .71)
with(x, round(cor(cbind(mot, int, fol, alg, cur), 
                  use="complete.obs"), digits=2))

#Recode variables for LCA (LCA requires non-zero integers)
x$x1 = x$mot + 1
x$x2 = round(x$int, digits = 0) 
x$x3 = round(x$fol, digits = 0) 
x$x4 = round(x$cur, digits = 0)
x$x5 = x$alg + 1

#Define LCA function
f <- cbind(x1, x2, x3, x4, x5) ~ 1

#Load Library for LCA
library(poLCA)

#Write function to find best fitting model
#min = 2 classes, max = 5 classes (trouble with convergence above 5)
min_bic <- 100000
for(i in 2:5){
  lc <- poLCA(f, x, nclass=i, 
              maxiter=3000, tol=1e-5, 
              na.rm=FALSE, nrep=10, 
              verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model <- lc
  }
} 

#Print results (4-class model is best, according to BIC)
print(LCA_best_model)

#Estimate Model, re-set order based on predicted probabilities 
#(biggest group first), then re-estimate models
lc <- poLCA(f, x, nclass=4, 
            maxiter=3000, tol=1e-5, 
            na.rm=FALSE,  nrep=10, 
            verbose=TRUE, calc.se=TRUE, 
            graph=TRUE) #initial model

probs.start <- poLCA.reorder(lc$probs.start, 
                             order(lc$P, decreasing=TRUE)) #create object for order

lc <- poLCA(f, x, nclass = 4, 
            maxiter=3000, tol=1e-5, 
            na.rm=FALSE, nrep=10, 
            verbose=TRUE, calc.se=TRUE, 
            probs.start = probs.start, 
            graph=TRUE) #refit model with order object (probs.start)

#Extract grouping variable and add to dataset
x$inv = as.factor(lc$predclass)
levels(x$inv) = c("Mod Unmot", "Low", "Mod Mot", "High")
x$inv <- factor(x$inv, levels = c("Low", "Mod Unmot", "Mod Mot", "High"))
table(x$inv)

##########################################################################

#Load Libraries
library(lme4)
library(lmerTest)

#Center Controls
x <- x %>% 
  group_by(frame) %>% 
  mutate(age.c = age-mean(age, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(female.c = female-mean(female, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(poc.c = poc-mean(poc, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(edu.c = edu-mean(edu, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(inc.c = inc-mean(inc, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(ideo.c = ideo-mean(ideo, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(pid.c = pid-mean(pid, na.rm=TRUE)) 

#More centering
x <- x %>% 
  group_by(frame) %>% 
  mutate(sm.freq.c = sm.freq-mean(sm.freq, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(size.c = size-mean(size, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(div.c = div-mean(div, na.rm=TRUE)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(grp.c = grp-mean(grp, na.rm=TRUE))

#DV: Incidental exposure
lm1 = lmer(ipe ~ inv + 
              age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
              sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))

#DV: Overall/story exposure
lm2 = lmer(pol ~ inv +
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
             sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))

summary(lm1)
summary(lm2)







