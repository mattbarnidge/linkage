df <- with(d, data.frame(story.click, story.scan, story.read, 
                 story.seek, story.comm, story.disc, story.share))

library(mice)
t <- mice(df, m=1, maxit=50, meth='pmm', seed=500)
c <- complete(t, 1)

with(d, ltm::cronbach.alpha(c)) #.765

factanal(c, factors=2, rotation="promax")