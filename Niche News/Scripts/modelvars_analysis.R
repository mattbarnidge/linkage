#Social media for news and communities of partisan overlap#
#Model Variables + Sandbox Analysis

####Data
setwd("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/data")

#load('EMCP20_coded.Rdata')
#load("newsfeed_data.Rdata")
#load('niche_news.Rdata')

#d1 = d
#d1 = merge(d1, d3, by = "ResponseId") # merging story-level data 

####Variables 
vars = c("ResponseId","ID", "group_engage",  "group_sent", "wave_engage","wave_sent","frame","age", "female", "poc", "edu", "inc", "int", "sm.freq", "smnews", "sm.newsintent",
         "alg", "story.aware", "ideo.ext","pid", "nsmnews", "sminc", "smincexp2", "smcur", "smfollow", "sm.ns", "sm.div.occ", "grp",
         "weights", "ideo", "newsideo", "news_code1", "news_code2", "news_code3", "clean_news1", "clean_news2", "clean_news3",
         "cluster.1", "cluster.2", "cluster.3", "sm.div.tie", "sminc.dis", "sminc.agr")

d1 = d1[vars]


#fold partisanship var
d1$p.strength = abs(d1$pid)
table(d1$pid)
table(d1$p.strength)

#partisan news vars
d1$newsideo_ext = abs(d1$newsideo) #news ideological strength
d1$news_code1_r = ifelse(d1$news_code1 == 0, 1, 0)#neutral
d1$news_code2_r = ifelse(d1$news_code2 == 0, 1, 0)#neutral
d1$news_code3_r = ifelse(d1$news_code3 == 0, 1, 0)#neutral
d1$news_code1_r.1 = ifelse(d1$news_code1 > 0, 1, 0)#right
d1$news_code2_r.1 = ifelse(d1$news_code2 > 0, 1, 0)#right
d1$news_code3_r.1 = ifelse(d1$news_code3 > 0, 1, 0)#right
d1$news_code1_r.2 = ifelse(d1$news_code1 < 0, 1, 0)#left
d1$news_code2_r.2 = ifelse(d1$news_code2 < 0, 1, 0)#left
d1$news_code3_r.2 = ifelse(d1$news_code3 < 0, 1, 0)#left

#Left/Right/Neutral
d1$neut_news = rowSums(d1[,c('news_code1_r', 'news_code2_r', 'news_code3_r')], na.rm =T)#neutral cluster
d1$right_news = rowSums(d1[,c('news_code1_r.1', 'news_code2_r.1', 'news_code3_r.1')], na.rm =T)#right cluster
d1$left_news = rowSums(d1[,c('news_code1_r.2', 'news_code2_r.2', 'news_code3_r.2')], na.rm =T)#left cluster

#Counter-attitudinal 
d1$right_cross = ifelse(d1$pid > 0 & d1$left_news > 0, 1, 0)
d1$left_cross = ifelse(d1$pid < 0 & d1$right_news > 0, 1, 0)
d1$news_counter = rowSums(d1[,c('left_cross','right_cross')], na.rm = T)

#Pro-attitudinal confirmation
d1$right_conf = ifelse(d1$pid > 0 & d1$right_news > 0, 1, 0)
d1$left_conf = ifelse(d1$pid < 0 & d1$left_news > 0, 1, 0)
d1$news_pro = rowSums(d1[,c('left_conf','right_conf')], na.rm = T)

#Avoidance
d1$right_avoid = ifelse(d1$pid > 0 & d1$left_news == 0, 1, 0)
d1$left_avoid = ifelse(d1$pid < 0 & d1$right_news == 0, 1, 0)
d1$news_avoid = rowSums(d1[,c('left_avoid','right_avoid')], na.rm = T)

#Cluster membership/strength
#Tie strength: bonding versus bridging behaviors

#in-group strength
d1$cluster_r1 = ifelse(d1$cluster.1 == 1, 1, 0)#cluster 1
d1$cluster_r2 = ifelse(d1$cluster.2 == 1, 1, 0)#cluster 1
d1$cluster_r3 = ifelse(d1$cluster.3 == 1, 1, 0)#cluster 1
d1$cluster_r4 = ifelse(d1$cluster.1 == 2, 1, 0)#cluster 2
d1$cluster_r5 = ifelse(d1$cluster.2 == 2, 1, 0)#cluster 2
d1$cluster_r6 = ifelse(d1$cluster.3 == 2, 1, 0)#cluster 2
d1$cluster_r7 = ifelse(d1$cluster.1 == 3, 1, 0)#cluster 3
d1$cluster_r8 = ifelse(d1$cluster.2 == 3, 1, 0)#cluster 3
d1$cluster_r9 = ifelse(d1$cluster.3 == 3, 1, 0)#cluster 3

#bonding: membership strength & assign case to single cluster by bond strength
d1$bonding_1 = rowSums(d1[,c('cluster_r1', 'cluster_r2', 'cluster_r3')], na.rm =T)#bonding cluster 1
d1$bonding_2 = rowSums(d1[,c('cluster_r4', 'cluster_r5', 'cluster_r6')], na.rm =T)#bonding cluster 2
d1$bonding_3 = rowSums(d1[,c('cluster_r7', 'cluster_r8', 'cluster_r9')], na.rm =T)#bonding cluster 3
d1$max_bond = pmax(d1$bonding_1, d1$bonding_2, d1$bonding_3, na.rm = T)#max tie strength across categories 

#Extract the 'strongest' niche, competing categories are treated as 0
#note that coding could differ from org-level data due to the way R treats '1' as the reference category
d1$niche = ifelse(d1$bonding_1 > d1$bonding_2 & d1$bonding_1 > d1$bonding_3, 3, 0) #cable 
d1$niche = ifelse(d1$bonding_2 > d1$bonding_1 & d1$bonding_2 > d1$bonding_3, 2, d1$niche) #elite
d1$niche = ifelse(d1$bonding_3 > d1$bonding_1 & d1$bonding_3 > d1$bonding_2, 1, d1$niche) #local
table(d1$niche)

#Create a factor var w/labels
d1$niche_belong = ifelse(d1$niche > 0, 1, 0)#binary niche 
d1$niche = ifelse(d1$niche == 0, NA, d1$niche)#drop non-categorized
d1$niche = factor(d1$niche, labels=c("local","elite", "cable"))#use local as reference
d1$niche_cable = ifelse(d1$niche == 'cable', 1, 0)
d1$niche_elite = ifelse(d1$niche == 'elite', 1, 0)

#bridging: sum of cross-cluster activity 
#d1$bridging_r1 = ifelse(d1$cluster.1 > 0 & d1$cluster.2 > 0 & d1$cluster.1 != d1$cluster.2, 1, 0)#bridging clusters 1&2
#d1$bridging_r2 = ifelse(d1$cluster.1 > 0 & d1$cluster.3 > 0 & d1$cluster.1 != d1$cluster.3, 1, 0)#bridging clusters 1&3
#d1$bridging_r3 = ifelse(d1$cluster.2 > 0 & d1$cluster.3 > 0 & d1$cluster.2 != d1$cluster.3, 1, 0)#bridging clusters 2&3
#d1$bridge_score = rowSums(d1[,c('bridging_r1', 'bridging_r2', 'bridging_r3')], na.rm =T)#total bridging

#mean center engagement 
#d1$engagement = as.numeric(scale(d1$engagement, center = T, scale = T))
#d1$net_sent = as.numeric(scale(d1$net_sent, center = T, scale = T))
#d1$group_engage = as.numeric(scale(d1$group_engage, center = T, scale = T))
#d1$group_sent = as.numeric(scale(d1$group_sent, center = T, scale = T))


#Descriptive stats, truncate and save data set to model vars
#modelvars = c("ResponseId","ID", "group_engage",  "group_sent", "wave_engage","wave_sent","frame","age", "female", "poc", "edu", "inc", "int", "sm.freq", "smnews", "sm.newsintent",
              #"alg", "story.aware", "ideo.ext","pid", "nsmnews", "sminc", "smincexp2", "smcur", "smfollow", "sm.ns", "sm.div.occ", "grp",
              #"weights", "ideo", "newsideo", "niche", "niche_belong", "news_avoid", "news_pro", "news_counter")

#d2 = d1[modelvars]
#rm(modelvars)

#names(d)
#library(psych)
#describe(d)
#save(d2, file = 'niche_news.RData')
#write.csv(d2, file = "niche_news.csv")


##Sandbox Models and Plotting############################

#modeling the ideological clusters
library(lme4)
library(lmerTest)#p.values 
library(sjstats)
library(car)#vif


#Pre-analysis checks
#test for need for MM models as needed
random = glmer(niche_belong ~ 1 + (1|frame), family = binomial, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data = d1, na.action = "na.omit")
fixed = glm(niche_belong ~ 1, data = d1, na.action = "na.omit")
AIC(fixed, random); BIC(fixed, random); lmtest::lrtest(fixed, random) #better model will have lower AIC & BIC + significant difference on lrtest

random = lmer(newsideo_ext ~ 1 + (1|frame), data = d1, REML=FALSE)
fixed = lm(newsideo_ext ~ 1, data = d1)
AIC(fixed, random); BIC(fixed, random); lmtest::lrtest(fixed, random) 


random = lmer(newsideo ~ 1 + (1|frame), data = d1, REML=FALSE)
fixed = lm(newsideo ~ 1, data = d1)
AIC(fixed, random); BIC(fixed, random); lmtest::lrtest(fixed, random) 



###linear/logit regressions

#Models
#Social News & Ideological News Consumption

m1 = glmer(news_avoid ~ age + female + poc + edu + inc + int + pid + sm.ns + sm.freq + sm.newsintent + smincexp2 + sminc + alg + group_engage + group_sent + (1|frame),
         data = d1,family=binomial, nAGQ=0, control=glmerControl(optimizer="Nelder_Mead"), na.action = "na.omit")
summary(m1)
exp(coef(m1))#odds ratios
vif(m1)


m2 = glmer(news_pro ~ age + female + poc + edu + inc + int + pid + sm.ns + sm.freq + sm.newsintent + smincexp2 + sminc + alg + group_engage + group_sent +  (1|frame),
         data = d1,family=binomial, nAGQ=0, control=glmerControl(optimizer="Nelder_Mead"), na.action = "na.omit")
summary(m2)
exp(coef(m2))#odds ratios
vif(m2)

m3 = glmer(news_counter ~ age + female + poc + edu + inc + int + pid + sm.ns + sm.freq + sm.newsintent + smincexp2 + sminc + alg + group_engage + group_sent +  (1|frame),
         data = d1,family=binomial, nAGQ=0, control=glmerControl(optimizer="Nelder_Mead"), na.action = "na.omit")
summary(m3)
exp(coef(m3))#odds ratios
vif(m3)

#Discriminant Analysis: Predictors of Niche Membership
#library(nnet)
#m4 = multinom(niche ~ age + female + poc + edu + inc + int + pid + sm.ns + sm.freq + alg + sm.newsintent + sminc.agr + sminc.dis, data = d1, na.action = "na.omit")
#p <- (1 - pnorm(abs(summary(m4)$coefficients/summary(m4)$standard.errors),0,1)) *2
#summary(m4)
#exp(coef(m4))
#round(p, digits = 4)

#Multilevel logit models for niche membership
#note to mean-center vars
m4 = glmer(niche_belong ~ age + female + poc + edu + inc + int + ideo + sm.ns + sm.newsintent + smincexp2 + sminc + alg + group_engage + group_sent + (1|frame),
           data = d1,family=binomial, nAGQ=0, control=glmerControl(optimizer="Nelder_Mead"), na.action = "na.omit")
m5 = glmer(niche_cable ~ age + female + poc + edu + inc + int + ideo + sm.ns + sm.newsintent + smincexp2 + sminc + alg + group_engage + group_sent + (1|frame),
           data = d1,family=binomial, nAGQ=0, control=glmerControl(optimizer="Nelder_Mead"), na.action = "na.omit")
m6 = glmer(niche_elite ~ age + female + poc + edu + inc + int + ideo + sm.ns + sm.newsintent + smincexp2 + sminc + alg + group_engage + group_sent + (1|frame),
           data = d1,family=binomial, nAGQ=0, control=glmerControl(optimizer="Nelder_Mead"), na.action = "na.omit")

summary(m4)
summary(m5)
summary(m6)

#News ideology

m7 = lmer(newsideo_ext ~ age + female + poc + edu + inc + int + ideo + sm.ns + sm.newsintent + smincexp2 + sminc + (1|frame),
           data = d1,na.action = "na.omit")
m8 = lmer(newsideo_ext ~ age + female + poc + edu + inc + int + ideo + sm.ns + sm.newsintent + smincexp2 + sminc + alg + engagement + net_sent + (1|frame),
           data = d1, na.action = "na.omit")
m9 = lmer(newsideo_ext ~ age + female + poc + edu + inc + int + ideo + sm.ns + sm.newsintent + smincexp2 + sminc + alg + engagement + net_sent + niche + (1|frame),
          data = d1, na.action = "na.omit")

summary(m7)
vif(m7)


#tools for html tables and plotting model
#see: http://www.strengejacke.de/sjPlot/index.html
library(sjPlot)
library(sjmisc)
library(sjlabelled)

#Tables (takes a minute when computing odds ratios)
tab_model(m1, m2, m3, show.ci = FALSE, show.intercept = FALSE, auto.label = FALSE)
tab_model(m4, m5, m6, show.ci = FALSE, show.intercept = FALSE, auto.label = FALSE) 
tab_model(m7, m8, m9, show.ci = FALSE, show.intercept = FALSE, auto.label = FALSE)


#Marginal Effects Plotting###########################################################
library(ggpubr)
library(ggplot2)

#Labels
#SjPlot uses labeled variables *(need to re-run models if you want labels to show up)*
d1$newsideo_ext = set_label(d1$newsideo_ext, "Strength News Ideology")
d1$news_counter = set_label(d1$news_counter, "Counter")
d1$alg = set_label(d1$alg, "Algorithmic Curation")
d1$sminc = set_label(d1$sminc, "SM Encounter Politics")
d1$smincexp2 = set_label(d1$smincexp2, "SM Encounter News")
d1$pid = set_label(d1$pid, "Party")
d1$pid = add_labels(d$pid, labels = c('Left' = -3, 'Right' = 3))
d1$ideo.ext = set_label(d1$ideo.ext, "Ideo. Ext.")
d1$ideo.ext = add_labels(d1$ideo.ext, labels = c('Low' = 0, 'High' = 5))
d1$sm.freq = set_label(d1$sm.freq, "General Social Media")
d1$smnews = set_label(d1$smnews, "News on Social Media")
d1$news_pro = set_label(d1$news_pro, "Confirmation")
d1$news_avoid = set_label(d1$news_avoid, "Avoidance") 
d1$nsmnews = set_label(d1$nsmnews, "Non-Social News")
d1$niche = set_label(d1$niche, "Niche")

#for smooth plots use glm
m1 = glm(news_avoid ~ age + female + poc + edu + inc + int + pid + sm.ns + sm.freq + sm.newsintent + smincexp2 + sminc + alg + engagement + net_sent + niche,
          data = d1,family=binomial, na.action = "na.omit")
m2 = glm(news_pro ~ age + female + poc + edu + inc + int + pid + sm.ns + sm.freq + sm.newsintent + smincexp2 + sminc + alg + engagement + net_sent + niche,
           data = d1,family=binomial,  na.action = "na.omit")
m3 = glm(news_counter ~ age + female + poc + edu + inc + int + pid + sm.ns + sm.freq + sm.newsintent + smincexp2 + sminc + alg + engagement + net_sent + niche,
           data = d1,family=binomial, na.action = "na.omit")

m10 = lm(newsideo_ext ~ age + female + poc + edu + inc + int + ideo + sm.ns + sm.newsintent + smincexp2 + sminc + alg + engagement + net_sent + niche,
          data = d1, na.action = "na.omit")


theme_set(theme_sjplot())
#Relationships of interest
p1 = plot_model(m1, type = "pred", terms = c("ideo.ext"))
p2 = plot_model(m2, type = "pred", terms = c("smnews"))
p3 = plot_model(m3, type = "pred", terms = c("alg"))

#Marginal Effects
#p1 = plot_model(m8, type = "pred", terms = c("sminc", "niche [cable, elite, local]"))
#p2 = plot_model(m8, type = "pred", terms = c("smnews", "niche [cable, elite, local]"))


p4 = plot_model(m10, type = "pred", terms = c("sminc [all]", "niche [cable, elite, local]"))
p5 = plot_model(m10, type = "pred", terms = c("smincexp2 [all]", "niche [cable, elite, local]"))



#Grid multiple plots
figure = ggarrange(p4, p5, 
                   ncol = 2, nrow = 1)

figure


#correlations############################################
#requires function
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coefficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .10, "#", "    ")))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

#
corstars(d1[,c('alg', 'smnews', 'sm.freq', 'nsmnews', 'int', 'ideo.ext', 'pid', 'news_avoid', 'news_counter', 
               'news_pro')])

