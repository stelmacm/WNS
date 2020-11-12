library(DHARMa)
library(lme4)
library(mlmRev)
cc <- na.omit(Contraception)
m1 <- glmer(use ~ livch*age*urban + (1|district),
            family=binomial,
            data=cc)
plot(m1)
length(residuals(m1))
m2 <- simulateResiduals(m1)
plot(m2)
plotResiduals(m2)
plotResiduals(m2, cc$age, quantreg=TRUE)
plotResiduals(m2, cc$livch)
plotResiduals(m2, cc$urban)
plotResiduals(m2, interaction(cc$livch,cc$urban))

dd <- data.frame(cc,res=m2$scaledResiduals)
library(ggplot2)
ggplot(dd,aes(livch,res,fill=urban)) +
    geom_boxplot()

ggplot(dd,aes(livch,res,fill=urban)) +
    geom_violin()

ggplot(dd,aes(age,res, colour=livch))+
    geom_point() +
    geom_smooth() +
    facet_wrap(~urban)




pred <- m2$fittedPredictedResponse
pred2 <- rank(pred, ties.method = "average")
pred2 <- pred2/max(pred2)
plot(pred2,m2$scaledResiduals)
plot(pred,m2$scaledResiduals)



plotResiduals(m2, rank=TRUE)
plotResiduals(m2, ~ age)
