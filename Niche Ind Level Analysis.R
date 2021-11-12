#Load Libraries
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(dotwhisker)
library(visreg)

#Set WD and Load Data
setwd("~/Desktop")
load("niche.mi.Rdata")
load("niche.mi.sub.Rdata")

#Grand Descriptives
with(m2, mean(newsideo, na.rm=TRUE))
with(m2, var(newsideo, na.rm=TRUE)) 

#Group Descriptives
with(m2, aggregate(newsideo, by=list(niche), mean)) 
with(m2, aggregate(newsideo, by=list(niche), var)) 

#Anova
aov = aov(newsideo ~ niche, data = m2, weights = weights)
summary(aov)

#Boxplot
p2 <- ggplot(m2, aes(x = niche, y = newsideo, fill = niche)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "", x = "Individual Level", y = "")
p2 + scale_fill_grey() + theme_classic()

#Combine with plot (p1) from org level data
grid.arrange(p1 + scale_y_continuous(limits = c(-3, 3)) + scale_fill_grey() + theme_classic(), 
             p2 + scale_y_continuous(limits = c(-3, 3)) + scale_fill_grey() + theme_classic(), 
             nrow = 1)

#Create Group Means Variables
ideo.means = with(m2, aggregate(ideo, by = list(niche), mean, na.rm=TRUE))[,2]
m2$ideo.g[m2$niche == "local"] <- ideo.means[1]
m2$ideo.g[m2$niche == "elite"] <- ideo.means[2]
m2$ideo.g[m2$niche == "cable"] <- ideo.means[3]

news.means = with(m2, aggregate(newsideo, by = list(niche), mean, na.rm=TRUE))[,2]
m2$news.g[m2$niche == "local"] <- news.means[1]
m2$news.g[m2$niche == "elite"] <- news.means[2]
m2$news.g[m2$niche == "cable"] <- news.means[3]

#Clean Up Environment
rm(ideo.means, news.means)

#Group Mean Center
m2 <- m2 %>%
  group_by(niche) %>%
  mutate(across(ideo:int, 
                ~(as.numeric(scale(., scale=FALSE))), 
                .names = "{col}.c"))

#Change Variable Labels for Plotting
m2 <- rename(m2, 
             Individual = ideo.c, 
             Audience = ideo.g, 
             Organization = news.g)

#Fit Random Intercepts Models
mlm1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm2 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm3 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm4 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + Audience + Individual*Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm5 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + Organization + Individual*Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))


#Summaries
options(scipen=10)
summary(mlm1, cor=FALSE); logLik(mlm1)
summary(mlm2, cor=FALSE); logLik(mlm2)
summary(mlm3, cor=FALSE); logLik(mlm3)
summary(mlm4, cor=FALSE); logLik(mlm4)
summary(mlm5, cor=FALSE); logLik(mlm5)

#Plotting Regression Coefficients
dwplot(list(mlm1, mlm2, mlm3), 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), 
       vars_order = c("Individual", "Audience", "Organization")) + 
  theme(legend.title = element_blank()) + 
  scale_colour_grey() + 
  xlab("Effect on News Ideology") + 
  ylab("Level of Observation")

#Plotting Interactions
par(mfrow=c(1,2))
visreg::visreg(mlm4, "Individual", by = "Audience", 
               overlay=TRUE, jitter=TRUE,
               line=list(col=c("black", "grey")), 
               points=list(col=c("black", "grey")),
               breaks=c(-.52, .62), 
               strip.names=c("Aud. Ideo. = -1 SD", "Aud. Ideo. = +1 SD"), 
               xlab = "Individual Ideology", 
               ylab = "News Ideology")
visreg::visreg(mlm5, "Individual", by = "Organization", 
               overlay=TRUE, jitter=TRUE,
               line=list(col=c("black", "grey")), 
               points=list(col=c("black", "grey")),
               breaks=c(-.35, .15), 
               strip.names=c("Org. Ideo. = -1 SD", "Org. Ideo. = +1 SD"), 
               xlab = "", 
               ylab = "")

#Adding on Social Media Variables

#Group Mean Center
m <- m %>%
  group_by(frame) %>%
  mutate(across(ideo:alg, 
                ~(as.numeric(scale(., scale=FALSE))), 
                .names = "{col}.c"))

mlm6 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + ideo.c + 
              sm.freq.c + sm.ns.c + sm.div.occ.c + 
              smfollow.c + ine.c +
              alg.c +
              engage + sent + 
              (1 + ideo.c | niche:frame), 
            data = m, weights = weights, 
            control = lmerControl(optimizer="Nelder_Mead"))
mlm7 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + ideo.c + 
              sm.freq.c + sm.ns.c + sm.div.occ.c + 
              smfollow.c + ine.c +
              alg.c +
              engage + sent + 
              ideo.c*engage +
              (1 + ideo.c | niche:frame), 
            data = m, weights = weights, 
            control = lmerControl(optimizer="Nelder_Mead"))
mlm8 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + ideo.c + 
              sm.freq.c + sm.ns.c + sm.div.occ.c + 
              smfollow.c + ine.c +
              alg.c +
              engage + sent + 
              ideo.c*sent +
              (1 + ideo.c | niche:frame), 
            data = m, weights = weights, 
            control = lmerControl(optimizer="Nelder_Mead"))

summary(mlm6, cor=FALSE); logLik(mlm6)
summary(mlm7, cor=FALSE); logLik(mlm7)
summary(mlm8, cor=FALSE); logLik(mlm8)

#Visualize
par(mfrow=c(1,2))
visreg::visreg(mlm7, "ideo.c", by="engage", 
               overlay=TRUE, jitter=TRUE, 
               line=list(col=c("black", "grey")), 
               points=list(col=c("black", "grey")), 
               breaks=c(.18, .64), 
               strip.names=c("Eng. = -1 SD", "Eng. = +1 SD"), 
               xlab = "Individual Ideology", 
               ylab = "News Ideology", 
               ylim=c(-3,3), 
               xlim=c(-5,5))
visreg::visreg(mlm8, "ideo.c", by="sent", 
               overlay=TRUE, jitter=TRUE, 
               line=list(col=c("black", "grey")), 
               points=list(col=c("black", "grey")), 
               breaks=c(.20, .56), 
               strip.names=c("Sent. = -1 SD", "Sent. = +1 SD"),
               xlab = "", 
               ylab = "", 
               ylim=c(-3,3), 
               xlim=c(-5,5))


