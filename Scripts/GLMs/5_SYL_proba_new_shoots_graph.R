#logigram:MOD2
#AIM: graph of proportion of new shoots from buds in sylleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/GLMs/5_SYL_proba_new_shoots_analysis.R")

library(RColorBrewer)

M = droplevels(MV.bud.SYL[MV.bud.SYL$fate == "M", ])
V = droplevels(MV.bud.SYL[MV.bud.SYL$fate == "V", ])

#create real data
#relative frequency of new shoots from M with other M
M_m = data.frame("new shoots" = prop.table(table(M$m, M$nb_new_shoots), 1)[, 2])
#relative frequency of new shoots from V with other M
V_m = data.frame("new shoots" = prop.table(table(V$m, V$nb_new_shoots), 1)[, 2])

#relative frequency of new shoots from M with other V
M_v = data.frame("new shoots" = prop.table(table(M$v, M$nb_new_shoots), 1)[, 2])
#relative frequency of new shoots from M with other V
V_v = data.frame("new shoots" = prop.table(table(V$v, V$nb_new_shoots), 1)[, 2])

#graph:
# png("Own_rooted_young/Outputs/Plots/5_SYL_proba_new_shoots.png",width=1200, height=900, res=150)# save plot
par(mfrow=c(1,2))
cols<-brewer.pal(n=3,name="Set2")
par(mar=c(5,4,1,0)+0.1)
#real data
with(M_m,plot(new.shoots~rownames(M_m), ylim=c(0,1),
              ylab = "new shoot proportion",
              xlab = "number of mixed buds",
              pch=19,
              col=cols[1]))
#predict fate M:m
with(M_m,lines(x=seq(0,5),
               lwd=2,
               y=predict(model,
                         newdata = data.frame("fate"="M", "m"=seq(0,5),
                                              "v"=mean(M$v)),
                         type="response"),col=cols[1]))
#compute confidence interval
pred=predict(model,newdata = data.frame("fate"="M",
                                           "m"=seq(0,5),"v"=mean(M$v)),se.fit = T)
lw<-with(pred, plogis(fit + qnorm(0.025)*se.fit))
up<-with(pred, plogis(fit + qnorm(0.975)*se.fit))
#cofidence intervals
with(M_m, polygon(x=c(seq(5,0),seq(0,5)),
                  y=c(rev(lw),up),
                  col=rgb(0,1,0.8,0.5), border = NA))
with(V_m,points(V_m$new.shoots~rownames(V_m), pch=19,
                ylim=c(0,1),
                col=cols[2]))
#legend
legend("top", horiz = T, legend=c("fate M", "fate V"), pch=c(19,19),col=cols[c(1,2)], cex=0.8)
par(mar=c(5,0,1,4)+0.1)
with(M_v,plot(new.shoots~rownames(M_v), ylim=c(0,1),
              ylab=NA,
              xlab="number of vegetative buds",pch=19,
              yaxt='n',
              col=cols[1]))
#predict fate M:v
with(M_v,lines(x=seq(0,5),
               lwd=2,
               y=predict(model,
                         newdata = data.frame("fate"="M", "v"=seq(0,5),
                                              "m"=mean(M$m)),
                         type="response"),col=cols[1]))
#compute confidence interval
pred=predict(model,newdata = data.frame("fate"="M",
                                           "v"=seq(0,5),"m"=mean(M$m)),se.fit = T)
lw<-with(pred, plogis(fit + qnorm(0.025)*se.fit))
up<-with(pred, plogis(fit + qnorm(0.975)*se.fit))
#cofidence intervals
with(M_v, polygon(x=c(seq(5,0),seq(0,5)),
                  y=c(rev(lw),up),
                  col=rgb(0,1,0.8,0.5), border = NA))
with(V_v,points(new.shoots~rownames(V_v), pch=19,
                col=cols[2],
                ylim=c(0,1)))
#predict fate V:v
with(V_v,lines(x=seq(0,5),
               lwd=2,
               col=cols[2],
               y=predict(model, newdata = data.frame("fate"="V", "m"=mean(V$m), "v"=seq(0,5)),
                         type="response")))
#compute confidence interval
pred=predict(model,newdata = data.frame("fate"="V", "v"=seq(0,5),"m"=mean(V$m)),se.fit = T)
lw<-with(pred, plogis(fit + qnorm(0.025)*se.fit))
up<-with(pred, plogis(fit + qnorm(0.975)*se.fit))
#cofidence intervals
with(V_m, polygon(x=c(seq(5,0),seq(0,5)),
                  y=c(rev(lw),up),
                  col=rgb(1,0.5,0,0.5), border = NA))
legend("top", horiz = T, legend=c("fate M", "fate V"), pch=c(19,19),col=cols[c(1,2)], cex=0.8)
# dev.off()

#create a graph with other models to compute other buds when they are not significant
#in the previous graph we used (mean$nb_buds)

#generate a glm without "other V"
mod1 = glm(nb_new_shoots ~ fate * m, family = "binomial", data = MV.bud.SYL)
summary(mod1)
#generate a glm without "other M"
mod2 = glm(nb_new_shoots ~ fate * v, family = "binomial", data = MV.bud.SYL)
summary(mod2)

# #graph
# png("imagines/proba_nb_new_shoots_syll2.png",width=1200, height=900, res=150)# save plot
# par(mfrow=c(1,2))
# cols<-brewer.pal(n=3,name="Set2")
# par(mar=c(5,4,1,0)+0.1)
# with(M_m,plot(new.shoots~rownames(M_m), ylim=c(0,1),
#               ylab = "new shoot proportion",
#               xlab = "other M buds",
#               pch=19,
#               col=cols[1]))
# #predict fateM:m with nb_v=0
# with(M_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(model,
#                          newdata = data.frame("fate"="M", "m"=seq(0,5),
#                                               "v"=0),
#                          type="response"),col="red"))
# #predict fateM:m with nb_v=mean(v)
# with(M_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(model,
#                          newdata = data.frame("fate"="M", "m"=seq(0,5),
#                                               "v"=mean(M$v)),
#                          type="response"),col="blue"))
# #predict fateM:m with a glm that not considers nb_v
# with(M_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(mod1,
#                          newdata = data.frame("fate"="M", "m"=seq(0,5)),
#                          type="response"),col="black"))
# with(V_m,points(V_m$new.shoots~rownames(V_m), pch=19,
#                 ylim=c(0,1),
#                 col=cols[2]))
# #predict fateV:m with nb_v=0
# with(V_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(model,
#                          newdata = data.frame("fate"="V", "m"=seq(0,5),
#                                               "v"=0),
#                          type="response"),col="red"))
# #predict fateV:m with nb_v=mean(v)
# with(V_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(model,
#                          newdata = data.frame("fate"="V", "m"=seq(0,5),
#                                               "v"=mean(V$v)),
#                          type="response"),col="blue"))
# #predict fateV:m with a glm that not considers nb_v
# with(V_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(mod1,
#                          newdata = data.frame("fate"="V", "m"=seq(0,5)),
#                          type="response"),col="black"))
# legend("top", horiz = T, legend=c("fate M", "fate V"), pch=c(19,19),col=cols[c(1,2)], cex=0.8)
# with(M_m, legend("bottomleft", horiz = F,
#                  legend=c("otherV=0", "otherV=mean(otherV)", "newglm !othrerV"),
#                  pch=c(19,19,19),col=c("red", "blue", "black"), cex=0.5))
# par(mar=c(5,0,1,4)+0.1)
# with(M_v,plot(new.shoots~rownames(M_v), ylim=c(0,1),
#               ylab=NA,
#               xlab="other V buds",pch=19,
#               yaxt='n',
#               col=cols[1]))
# # predict fateM:v with nb_m=0
# with(M_v,lines(x=seq(0,5),
#                lwd=2,
#                col="red",
#                y=predict(model, newdata = data.frame("fate"="M", "m"=0, "v"=seq(0,5)),
#                          type="response")))
# # predict fateM:v with nb_m=mean(m)
# with(M_v,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(model,
#                          newdata = data.frame("fate"="M", "v"=seq(0,5),
#                                               "m"=mean(M$m)),
#                          type="response"),col="blue"))
# # predict fateM:v with a glm that not considers nb_m
# with(M_v,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(mod2,
#                          newdata = data.frame("fate"="M", "v"=seq(0,5)),
#                          type="response"),col="black"))
# with(V_v,points(new.shoots~rownames(V_v), pch=19,
#                 col=cols[2],
#                 ylim=c(0,1)))
# # predict fateV:v with nb_m=0
# with(V_v,lines(x=seq(0,5),
#                lwd=2,
#                col="red",
#                y=predict(model, newdata = data.frame("fate"="V", "m"=0, "v"=seq(0,5)),
#                          type="response")))
# # predict fateV:v with nb_m=mean(m)
# with(V_v,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(model,
#                          newdata = data.frame("fate"="V", "v"=seq(0,5),
#                                               "m"=mean(V$m)),
#                          type="response"),col="blue"))
# # predict fateV:v with a glm that not considers nb_m
# with(V_v,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(mod2,
#                          newdata = data.frame("fate"="V", "v"=seq(0,5)),
#                          type="response"),col="black"))
# with(M_m, legend("bottomleft", horiz = F,
#                  legend=c("otherM=0", "otherM=mean(otherM)", "newglm !othrerM"),
#                  pch=c(19,19,19),col=c("red", "blue", "black"), cex=0.5))
# legend("top", horiz = T, legend=c("fate M", "fate V"), pch=c(19,19),col=cols[c(1,2)], cex=0.8)
# dev.off()
