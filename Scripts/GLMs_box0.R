#box 0 and interaction
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

ann=read.csv(paste0(wd,"DF/auto/mtp use/2020shoot_level_DEVELOPED_fin.csv"))

#relation:length_cm~length_nodes####
glm_box1=nls(node~a*(Length^b),data = ann,start = c(a=1, b=0.5))
summary(glm_box1)
#real
df=data.frame(Length=seq(1,max(ann$Length),by=1))
#predicted
df$pred=predict(glm_box1,
                newdata = df,
                type="response")
#graph
png("0a.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(ann, plot(ann$node~ann$Length,
                          pch=19,col = cols[1],
                          main="length(cm) vs length(node)",
                          xlab= "length(cm)",
                          ylab="length(node)"),)
with(df,lines(df$pred~df$Length,col=cols[2], lwd=5))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()
#relation:length_cm~diameter####
glm_box1=nls(Diam~a*(Length^b),data = ann,start = c(a=1, b=0.5))
summary(glm_box1)
#real
df=data.frame(Length=seq(1,max(ann$Length),by=1))
#predicted
df$pred=predict(glm_box1,
                newdata = df,
                type="response")
#graph
png("0b.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(ann, plot(ann$Diam~ann$Length,
               pch=19,col = cols[1],
               main="length(cm) vs diam(mm)",
               xlab= "length(cm)",
               ylab="diam(mm)"),)
with(df,lines(df$pred~df$Length,col=cols[2], lwd=5))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()