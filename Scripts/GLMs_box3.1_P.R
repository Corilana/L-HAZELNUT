#box3.1_P:GLM: #how many V and M buds in proleptic?
#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))
#import library
library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)
library(nnet)
library(effects)
#import dataset
bud=read.csv(paste0(wd, "DF/auto/mtp use/bud_level_LATERALS.csv"))
#subset for proleptic
PROL_bud_scale=bud[bud$from_=="PROL",]
#change columns names to not make confusion
a=grep("^Length$", names(PROL_bud_scale))
b=grep(".node.$", names(PROL_bud_scale))
c=grep("rank", names(PROL_bud_scale))
d=grep("abs", names(PROL_bud_scale))
e=grep("tot_", names(PROL_bud_scale))
colnames(PROL_bud_scale)[c(a,b,c,d,e)]=c("length_cm",
                                          "length_nodes",
                                          "rank_node",
                                          "distance",
                                          "tot_buds")
#check dataset
str(PROL_bud_scale)
#trasfrom as factor "fate"
PROL_bud_scale$fate=as.factor(PROL_bud_scale$fate)
#set "V" as the base factor
PROL_bud_scale$fate=relevel(PROL_bud_scale$fate, "V")
#1: parent lenght(cm)+distance+ rank_node+length(node)####
#multinomial function
test=multinom(fate~length_cm+distance+rank_node+length_nodes,data = PROL_bud_scale)
summary(test)$AIC
exp(coef(test))
#computing p value
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
#2: parent lenght(cm)+distance+ rank_node####
test=multinom(fate~length_cm+distance+rank_node,data = PROL_bud_scale)
summary(test)
exp(coef(test))
#computing p value
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#3: distance+ rank_node####
test=multinom(fate~distance+rank_node,data = PROL_bud_scale)
summary(test)
exp(coef(test))
#computing p value
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#4: rank_node####
test=multinom(fate~rank_node,data = PROL_bud_scale)
summary(test)
exp(coef(test))
#using proba to understan
fit=fitted(test)
#computing p value
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
#preficted value
df=data.frame(rank_node=(seq(1,23)))
pred=cbind(df,predict(test, newdata = df, "probs", se=T))
pred
#confidence interval
fit.eff <- Effect("rank_node", test)
data.frame(fit.eff$model.matrix, fit.eff$prob, fit.eff$lower.prob, 
           fit.eff$upper.prob)

png("3.1_b.png",width=1200, height=900, res=150)
plot(allEffects(test))
dev.off()

#real value
tab=as.matrix(prop.table(table(PROL_bud_scale$rank_node,PROL_bud_scale$fate),1))
tab

#graph
png("3.1_a.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,5))
cols<-brewer.pal(n=3,name="Set2")
r=barplot(t(tab),
               col = cols, names.arg = row.names(tab),
               main="frequence of B,M,V(#B/tot buds)", xlab = "rank_node",
               ylab="%", ylim = c(0,1))
with(pred, lines(x = r,pred$V,lwd=5, col="black"))
with(pred, lines(x = r,pred$B,lwd=5, col="blue"))
with(pred, lines(x = r,pred$M,lwd=5, col="red"))
legend("topright",
       inset=c(-0.13,0),xpd = TRUE,
       legend = c(colnames(tab),paste0("predict",colnames(tab))),
       lty=c(NA,NA,NA,1,1,1),fill = c(cols, c("black","blue","red")), cex=0.8)
dev.off()