#RQ: Are the uninvolved more likely to report incidental exposure?

#Correlations among indicators of involvement
with(x, round(cor(cbind(mot, int, fol, alg), use="complete.obs"), digits=2))

#LCA based on indicators of involvement

#Fix variables
x$x1 = x$mot + 1
x$x2 = round(x$int, digits = 0)
x$x3 = round(x$fol, digits = 0)
x$x4 = x$alg + 1

#Define function
f <- cbind(x1, x2, x3, x4) ~ 1

#Determine Best Model
library(poLCA)
max_II <- -100000
min_bic <- 100000
for(i in 2:5){
  lc <- poLCA(f, x, nclass=i, maxiter=3000, tol=1e-5, na.rm=FALSE,  nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model <- lc
  }
} 
print(LCA_best_model)
rm(LCA_best_model, i, max_II, min_bic)

#Estimate Model, Fix Order, Re-Estimate Model
lc <- poLCA(f, x, nclass=3, maxiter=3000, tol=1e-5, na.rm=TRUE,  nrep=10, verbose=TRUE, calc.se=TRUE, graph=TRUE)
probs.start <- poLCA.reorder(lc$probs.start, order(lc$P, decreasing=TRUE))
lc <- poLCA(f, x, nclass = 3, maxiter=3000, tol=1e-5, na.rm=TRUE, nrep=10, verbose=TRUE, calc.se=TRUE, probs.start = probs.start, graph=TRUE)

rm(probs.start, f)

#Extract Grouping Variable
x$inv = as.factor(lc$predclass)

#Fit Multinomial Model
table(x$inv, x$incexp.f1)
mn = nnet::multinom(incexp.f1 ~ inv + age + female + poc + edu + inc + ideo + pid + sm.freq + size + div + grp + cur, data=x, weights=weights)

#Compare to Null Model
nmn = nnet::multinom(incexp.f1 ~ 1, data=x, weights=weights)
anova(nmn, mn)

#Goodness of Fit
chisq.test(x$incexp.f1, predict(mn)) 
performance::r2_nagelkerke(mn)

#Extract Key Information
p.mn <- (1 - pnorm(abs(summary(mn)$coefficients/summary(mn)$standard.errors), 0, 1)) * 2
coef.mn <- summary(mn)$coefficients
se.mn <- summary(mn)$standard.errors
exp.mn <- exp(coef(mn))
n.mn <- length(residuals(mn))/3

#Print
print(n.mn)
round(coef.mn,2)
round(se.mn,2)
round(exp.mn,2)
round(p.mn,3)

par(mfrow=c(1,3))
visreg::visreg(mn, "inv")

#Fit Binomial Model
x$incexp.sk = ifelse(x$recall == 1 & x$incexp == 1, 1, 0) #include skips
lg = glm(incexp.sk ~ inv + age + female + poc + edu + inc + ideo + pid + sm.freq + size + div + grp + cur, 
            data=x, family=poisson, weights=weights)
summary(lg)
par(mfrow=c(1,1))
visreg::visreg(lg, "inv")




