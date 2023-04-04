library(dplyr)

setwd("~/Documents/GitHub/linkage/Niche News")
load("niche_news.Rdata")

#Recode Some Variables
d2$alg[d2$alg==2] <- 1

d2$engage <- NA
d2$sent <- NA
d2$engage <- ifelse(d2$niche_belong==1, d2$group_engage, d2$wave_engage)
d2$sent <- ifelse(d2$niche_belong==1, d2$group_sent, d2$wave_sent)
d2$engage = scales::rescale(d2$engage, to=c(0,1))
d2$sent = scales::rescale(d2$sent, to=c(0,1))
d2$group_engage <- scales::rescale(d2$group_engage, to=c(0,1))
d2$group_sent <- scales::rescale(d2$group_sent, to=c(0,1))
d2$wave_engage <- scales::rescale(d2$wave_engage, to=c(0,1))
d2$wave_sent <- scales::rescale(d2$wave_sent, to=c(0,1))

d2$ine = sqrt(d2$sminc*d2$smincexp2)

#Reorder Variables
x <- d2 %>% select(newsideo, ideo,
                   age, female, poc, edu, inc, int,
                   sm.freq, 
                   sm.newsintent, 
                   smnews, ine, smcur, smfollow,
                   sm.ns, sm.div.occ, grp,
                   alg,
                   engage, sent,
                   group_engage, group_sent,
                   wave_engage, wave_sent,
                   niche, niche_belong, 
                   frame, weights)

#Multiple Imputation
t <- mice::mice(x[1:18], m=1, maxit=50, meth='pmm', seed=500)
t2 <- mice::complete(t, 1)
m <- cbind(t2, x[,19:28])
rm(t, t2)

#Create Subset of Niche Members Only
m2 <- subset(m, niche_belong == 1)

#Save Data
save(m, file = "niche.mi.Rdata")
save(m2, file = "niche.mi.sub.Rdata")


