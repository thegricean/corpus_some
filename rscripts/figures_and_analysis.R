# created by jdegen
# code for generating the figures and analysis reported in 
# Degen, J. (2015). Investigating the distribution of _some_ (but not _all_) implicatures using corpora and web-based methods. Semantics & Pragmatics.

library(Hmisc)
library(gridExtra)
library(ggplot2)
library(bootstrap)
library(MuMIn)
library(lme4)
theme_set(theme_bw())

# set your path here
setwd("path/to/corpus_some")
source("rscripts/helpers.R")

# read data
d = read.table("data/some_database.csv",sep="\t",header=T,quote="")


# Figure 1: Distribution of mean per-item implicature strength ratings.
agr = aggregate(Rating ~ Item, data=d, FUN=mean)
ggplot(agr,aes(x=Rating)) +
  geom_histogram() +
  scale_x_continuous(name="Mean by-item implicature strength rating",breaks=seq(1,7,by=1)) 


# Figure 10: Distribution of simulated mean by-item ratings.
likert = c(1,2,3,4,5,6,7)
means = data.frame(Mean=replicate(1363,mean(sample(likert,size=10,replace=T))))
summary(means$Mean)
sd(means$Mean)
ggplot(means,aes(x=Mean)) +
  geom_histogram() +
  scale_y_continuous(limits=c(0,180)) +  
  scale_x_continuous(limits=c(1,7),breaks=seq(1,7,by=1),name=c("Mean by-item rating (simulated)"))


# Figure 2: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for non-partitive and partitive some-NPs.
agr = aggregate(Rating ~ Item + Partitive, data=d,FUN=mean)
agrr = aggregate(Rating ~ Partitive, data=agr, FUN=mean)
agrr$CILow = aggregate(Rating ~ Partitive, data=agr, FUN=ci.low)$Rating
agrr$CIHigh = aggregate(Rating ~ Partitive, data=agr, FUN=ci.high)$Rating
agrr$YMin = agrr$Rating - agrr$CILow
agrr$YMax = agrr$Rating + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Partitive))$Freq

pm = ggplot(agrr,aes(x=Partitive,y=Rating,fill=Partitive)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_x_discrete(name="",breaks=levels(agrr$Partitive),labels=c("non-partitive","partitive")) +
  scale_fill_grey(start=0.7,end=0.35,name="") +  
  scale_y_continuous("Mean implicature strength") +
  geom_text(y=0.5,aes(label=Freq),size=4)
pm

agr = aggregate(Rating ~ Item + Partitive, data=d, FUN=mean)
pd = ggplot(agr,aes(x=Rating,fill=Partitive)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.35,breaks=levels(agr$Partitive),labels=c("non-partitive","partitive")) +
  scale_y_continuous(name="Number of cases") +
  scale_x_continuous("Mean by-item implicature strength",breaks=seq(1,7,by=1))
pd

grid.arrange(pm,pd,nrow=1)


# Figure 3: Distribution of mean by-item determiner strength ratings overall (left) and conditioned on whether or not the some-NP was overtly partitive (right). Higher ratings indicate weaker determiner uses.
agr = aggregate(StrengthSome ~ Item, data=d, FUN=mean)
p = ggplot(agr,aes(x=StrengthSome)) +
  geom_histogram(position="dodge") +
  geom_density(alpha=.3) +
  scale_x_continuous(name="Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker"))  
p

agr = aggregate(StrengthSome ~ Item + Partitive, data=d, FUN=mean)
pp = ggplot(agr,aes(x=StrengthSome,fill=Partitive)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.3,breaks=c("no","yes"),labels=c("non-partitive","partitive")) +
  scale_x_continuous(name="Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker")) 
pp

grid.arrange(p,pp,nrow=1)
            

# Figure 4: Mean by-item implicature rating as a function of decreasing determiner strength.
agr = aggregate(Rating ~ Item + StrengthSome, data=d, FUN=mean)
ggplot(agr,aes(x=StrengthSome,y=Rating)) +
  stat_sum(size=3,aes(alpha=..n..)) +
  scale_y_continuous(name="Mean implicature strength rating") +
  scale_x_continuous("Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker")) +
  geom_smooth(method="lm",color="black",size=1)  


# Figure 5: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for new, mediated, and old embedded NP referents.
agr = aggregate(Rating ~ Item + Mention, data=d, FUN=mean)
agrr = aggregate(Rating ~ Mention, data=agr, FUN=mean)
agrr$CILow = aggregate(Rating ~ Mention, data=agr, FUN=ci.low)$Rating
agrr$CIHigh = aggregate(Rating ~ Mention, data=agr, FUN=ci.high)$Rating
agrr$YMin = agrr$Rating - agrr$CILow
agrr$YMax = agrr$Rating + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Mention))$Freq
agrr$Mention = factor(x=as.character(agrr$Mention),levels=c("new","med","old"))

pm = ggplot(agrr,aes(x=Mention,y=Rating, fill=Mention)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_fill_manual(values=c("gray60","gray85","gray30")) +  
  scale_x_discrete(name="",breaks=levels(agrr$Mention),labels=c("new","mediated","old")) +
  scale_y_continuous("Mean implicature strength") +
  geom_text(y=0.5,aes(label=Freq),size=4)
pm

agr = aggregate(Rating ~ Item + Mention, data=d, FUN=mean)
agr$Mention = factor(x=as.character(agr$Mention),levels=c("new","med","old"))
pd = ggplot(agr,aes(x=Rating,fill=Mention)) +
  geom_histogram(position="dodge") +
  scale_fill_manual(values=c("gray60","gray85","gray30")) +
  scale_y_continuous(name="Number of cases") +
  scale_x_continuous("Mean by-item implicature strength",breaks=seq(1,7,by=1))
pd

grid.arrange(pm,pd,nrow=1)


# Figure 6: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for other and subject some-NPs.
agr = aggregate(Rating ~ Item + Subjecthood, data=d, FUN=mean)
agrr = aggregate(data=agr, Rating ~ Subjecthood, FUN=mean)
agrr$CILow = aggregate(Rating ~ Subjecthood, data=agr, FUN=ci.low)$Rating
agrr$CIHigh = aggregate(Rating ~ Subjecthood, data=agr, FUN=ci.high)$Rating
agrr$YMin = agrr$Rating - agrr$CILow
agrr$YMax = agrr$Rating + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Subjecthood))$Freq

pm = ggplot(agrr,aes(x=Subjecthood,y=Rating,fill=Subjecthood)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_fill_grey(start=.9,end=.35) +
  scale_x_discrete(name="",breaks=levels(agrr$Subjecthood),labels=c("other","subject")) +
  scale_y_continuous("Mean implicature strength") +
  geom_text(y=0.5,aes(label=Freq),size=4)
pm

agr = aggregate(Rating ~ Item + Subjecthood, data=d, FUN=mean)
pd = ggplot(agr,aes(x=Rating,fill=Subjecthood)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.35,breaks=levels(agr$Subjecthood),labels=c("other","subject"),name="Subjecthood") +
  scale_y_continuous(name="Number of cases") +
  scale_x_continuous("Mean by-item implicature strength",breaks=seq(1,7,by=1))
pd

grid.arrange(pm,pd,nrow=1)


# Figure 7: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for modified and unmodified some-NPs.
agr = aggregate(Rating ~ Item + Modification, data=d, FUN=mean)
agrr = aggregate(data=agr, Rating ~ Modification, FUN=mean)
agrr$CILow = aggregate(Rating ~ Modification, data=agr, FUN=ci.low)$Rating
agrr$CIHigh = aggregate(Rating ~ Modification, data=agr, FUN=ci.high)$Rating
agrr$YMin = agrr$Rating - agrr$CILow
agrr$YMax = agrr$Rating + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Modification))$Freq

pm=ggplot(agrr,aes(x=Modification,y=Rating,fill=Modification)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_x_discrete(name="",breaks=levels(agrr$Modification),labels=c("modified","unmodified")) +
  scale_fill_grey(start=.7,end=.25) +
  scale_y_continuous("Mean implicature strength") +
  geom_text(y=0.5,aes(label=Freq),size=4)
pm

agr = aggregate(Rating ~ Item + Modification, data=d, FUN=mean)
pd = ggplot(agr,aes(x=Rating,fill=Modification)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.25,breaks=levels(agr$Modification),labels=c("modified","unmodified"),name="Modification") +
  scale_y_continuous(name="Number of cases") +
  scale_x_continuous("Mean by-item implicature strength",breaks=seq(1,7,by=1))
pd

grid.arrange(pm,pd,nrow=1)

# Figure 8: Mean implicature strength ratings by linguistic mention (old/new embedded NP referent), subjecthood (subject/other some-NP), and modification (modified/unmodified embedded NP).
d$redMention = as.factor(ifelse(d$Mention == "new","new","old"))
agr = aggregate(Rating ~ Item + Modification + Subjecthood + redMention,FUN=mean, data=d)
agrr = aggregate(data=agr, Rating ~ Modification + Subjecthood + redMention, FUN=mean)
agrr$CILow = aggregate(Rating ~ Modification + Subjecthood + redMention, data=agr, FUN=ci.low)$Rating
agrr$CIHigh = aggregate(Rating ~ Modification + Subjecthood + redMention, data=agr, FUN=ci.high)$Rating
agrr$YMin = agrr$Rating - agrr$CILow
agrr$YMax = agrr$Rating + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Modification,agr$Subjecthood,agr$redMention))$Freq
dodge = position_dodge(.9)

ggplot(agrr,aes(x=redMention,y=Rating,fill=Subjecthood)) +
  geom_bar(stat="identity",position=dodge,width=.9) +  
  geom_bar(stat="identity",color="black",position=dodge,width=.9,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_x_discrete(name="Linguistic mention") +
  scale_fill_manual(values=c("gray40","gray80"),name="Subjecthood") +
  scale_y_continuous("Mean implicature strength") +
  geom_text(aes(label=Freq,y=0.5),position=dodge,size=4) +
  facet_wrap(~Modification)


# Figure 9: Scatterplot of empirical versus predicted mean by-item strength ratings for basic model (left panel, only by-participant random intercepts), intermediate model (center panel, additionally fixed effects of interest), and final model (right panel, additionally by-item random intercepts and by-participant random slopes for fixed effects). 
d$logSentenceLength = log(d$SentenceLength)
centered = cbind(d, myCenter(d[,c("StrengthSome","logSentenceLength","Subjecthood","Modification","Partitive","redMention")]))

m.random = lmer(Rating ~  (1|workerid), data=centered)
summary(m.random)

m.fixed = lmer(Rating ~ cPartitive*cStrengthSome+credMention*cSubjecthood*cModification + clogSentenceLength + (1|workerid), data=centered)
summary(m.fixed)

anova(m.random,m.fixed)

m = lmer(Rating ~ cPartitive*cStrengthSome+credMention*cSubjecthood*cModification + clogSentenceLength + (1|workerid) + (0 + cPartitive|workerid) + (0 + cStrengthSome|workerid) + (0 + credMention|workerid) + (0 + cSubjecthood|workerid) + (0+cModification|workerid) + (0 + cPartitive:cStrengthSome|workerid) + (1|Item), data=centered)
msummary = summary(m)

coefs = as.data.frame(msummary$coefficients)
summary(coefs)

# create the model summary reported in Table 5, Appendix D
createLatexTableLinear(coefs,predictornames=c("Intercept","Partitive","Strength","Linguistic mention","Subjecthood","Modification","Sentence length","Partitive:Strength","Linguistic mention:Subjecthood","Linguistic mention:Modification","Subjecthood:Modification","Linguistic mention:Subjecthood:Modification"))

anova(m.fixed,m)

# BIC comparison
BIC(m.random) # bic: 58452.95
BIC(m.fixed) # bic: 55938
BIC(m) # bic: 54015.73

# R squared -- marginal: proportion of variance explained by the fixed factors alone: .14. conditional: proportion of variance explained by both the fixed and random factors: .46.
r.squaredGLMM(m.random)
r.squaredGLMM(m.fixed)
r.squaredGLMM(m)

centered$Predicted = fitted(m)
centered$PredictedFixed = fitted(m.fixed)
centered$PredictedRandom = fitted(m.random)

# plot predicted values
agr = aggregate(Rating ~ Item, FUN=mean, data=centered)
agr$Predicted = aggregate(Predicted ~ Item, FUN=mean, data=centered)$Predicted
agr$PredictedFixed = aggregate(PredictedFixed ~ Item, FUN=mean, data=centered)$PredictedFixed
agr$PredictedRandom = aggregate(PredictedRandom ~ Item, FUN=mean, data=centered)$PredictedRandom

cor(agr$Predicted, agr$Rating) # corr: .99
cor(agr$PredictedFixed, agr$Rating) # corr: .66
cor(agr$PredictedRandom, agr$Rating) # corr: .16

r = data.frame(X=c(6),Y=c(1.5),R=paste("r = ",gsub("0.",".",round(cor(agr$Predicted,agr$Rating),2)),sep=""))
p1=ggplot(agr, aes(x=Predicted,y=Rating)) +
  geom_point(size=1.5) +
  geom_smooth(method="lm",size=2) +
  scale_x_continuous(name="",limits=c(1.5,6.5)) +  
  scale_y_continuous(name="",limits=c(1,6.5))  +  
  geom_text(data=r,aes(x=X,y=Y,label=R))  
p1

r = data.frame(X=c(6),Y=c(1.5),R=paste("r = ",gsub("0.",".",round(cor(agr$PredictedFixed,agr$Rating),2)),sep=""))
p2=ggplot(agr, aes(x=PredictedFixed,y=Rating)) +
  geom_point(size=1.5) +
  geom_smooth(method="lm",size=2) +
  scale_x_continuous(name="",limits=c(1.5,6.5)) +  
  scale_y_continuous(name="",limits=c(1,6.5))  +  
  geom_text(data=r,aes(x=X,y=Y,label=R))
p2

r = data.frame(X=c(6),Y=c(1.5),R=paste("r = ",gsub("0.",".",round(cor(agr$PredictedRandom,agr$Rating),2)),sep=""))
p3=ggplot(agr, aes(x=PredictedRandom,y=Rating)) +
  geom_point(size=1.5) +
  geom_smooth(method="lm",size=2) +
  scale_x_continuous(name="",limits=c(1.5,6.5)) +  
  scale_y_continuous(name="",limits=c(1,6.5))  +  
  geom_text(data=r,aes(x=X,y=Y,label=R))
p3

grid.arrange(p3,p2,p1,nrow=1,sub=textGrob("Model-predicted by-item mean strength",gp=gpar(fontsize=17),vjust=0),left=textGrob("Empirical by-item mean strength",gp=gpar(fontsize=17),vjust=0.5,rot=90))



