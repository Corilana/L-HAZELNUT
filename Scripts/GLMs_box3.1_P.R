      #box3.1_P:GLM: #how many V and M buds in proleptic?
      #set working directory
      #wd="C:/Users/franc/Google Drive/PhD/Deruta/"
      # Parent directory of "Scripts", in principle HazelnutFSPM/Scripts
      parent.path = dirname(getwd())
      wd=parent.path
      setwd(wd)
      #setwd(paste0(wd, "R/auto/Lateral/glm"))
      #import library
      library(stats)
      library(dplyr)
      library(RColorBrewer)
      library(plotrix)
      library(plyr)
      library(nnet)
      library(effects)
      #import dataset FROM .../GitHub/HazelnutFSPM/Dataframes/bud_level_LATERALS.csv
      # bud=read.csv(paste0(wd, "DF/auto/mtp  use/bud_level_LATERALS.csv"))
      bud=read.csv(paste0(wd, "/Dataframes/bud_level_LATERALS.csv"))
      #subset for proleptic
      PROL_bud_scale=bud[bud$from_=="PROL",]
      PROL_bud_scale$fate=factor(PROL_bud_scale$fate)
      
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
      #transform as factor "fate"
      PROL_bud_scale$fate=as.factor(PROL_bud_scale$fate)
      #set "V" as the base factor
      PROL_bud_scale$fate=relevel(PROL_bud_scale$fate, "V")
      #CREATE MEDIAN DISTANCE NORMALIZED
      PROL_bud_scale$median_distance=round((PROL_bud_scale$median_distance/PROL_bud_scale$length_nodes),1)
      
      # The dependent variable is "fate"
      #1: parent length(cm)+median_distance+ rank_node+length(node)####
      #multinomial function
      test=multinom(fate~length_cm+median_distance+rank_node+length_nodes,data = PROL_bud_scale)
      summary(test)$AIC
      exp(coef(test))
      #computing p value
      z <- summary(test)$coefficients/summary(test)$standard.errors
      p <- (1 - pnorm(abs(z), 0, 1)) * 2
      p #--> eliminate length_nodes
      
      #2: parent lenght(cm)+median_distance+ rank_node####
      test=multinom(fate~length_cm+median_distance+rank_node,data = PROL_bud_scale)
      summary(test)
      exp(coef(test))
      #computing p value
      z <- summary(test)$coefficients/summary(test)$standard.errors
      p <- (1 - pnorm(abs(z), 0, 1)) * 2
      p
      
      #permutati LENGTH_CM
      null_1=multinom(fate~median_distance+rank_node+1,data = PROL_bud_scale)
      dif=test$AIC-null_1$AIC
      met_nul=PROL_bud_scale
      
      df=data.frame(matrix(nrow=0, ncol=0))
      for (i in 1:1000) {
        met_nul$length_cm=sample(PROL_bud_scale$length_cm)
        perm=multinom(fate~median_distance+rank_node+length_cm,data = met_nul)
        a=perm$AIC-null_1$AIC
        b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
        r=cbind(i,a, b)
        df=rbind(df,r)
      }
      
      better_perm=length(which(df$b==1))#times better perm!!!
      
      #permutati RANK
      null_1=multinom(fate~median_distance+length_cm+1,data = PROL_bud_scale)
      dif=test$AIC-null_1$AIC
      met_nul=PROL_bud_scale
        
        df=data.frame(matrix(nrow=0, ncol=0))
        for (i in 1:1000) {
          met_nul$rank_node=sample(PROL_bud_scale$rank_node)
          perm=multinom(fate~median_distance+rank_node+length_cm,data = met_nul)
          a=perm$AIC-null_1$AIC
          b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
          r=cbind(i,a, b)
          df=rbind(df,r)
        }
        
        better_perm=length(which(df$b==1))#times better perm!!!
      #remove rank
      
      #3: parent length(cm)+median_distance####
      test=multinom(fate~length_cm+median_distance,data = PROL_bud_scale)
      summary(test)
      exp(coef(test))
      #computing p value
      z <- summary(test)$coefficients/summary(test)$standard.errors
      p <- (1 - pnorm(abs(z), 0, 1)) * 2
      p 
      
      #permutati median_distance
        null_1=multinom(fate~length_cm+1,data = PROL_bud_scale)
        dif=test$AIC-null_1$AIC
        met_nul=PROL_bud_scale
        
        df=data.frame(matrix(nrow=0, ncol=0))
        for (i in 1:1000) {
          met_nul$median_distance=sample(PROL_bud_scale$median_distance)
          perm=multinom(fate~median_distance+length_cm,data = met_nul)
          a=perm$AIC-null_1$AIC
          b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
          r=cbind(i,a, b)
          df=rbind(df,r)
        }
        
        better_perm=length(which(df$b==1))#times better perm!!!
        #graph
        png("3S_1.png",width=1200, height=900, res=150)
        plot(allEffects(test))
        dev.off()
      
      #i want to keep rank and distance because it makes more sense biologically
      #4: median_distance + rank node####
      test=multinom(fate~median_distance+rank_node,data = PROL_bud_scale)
      summary(test)
      exp(coef(test))
      #computing p value
      z <- summary(test)$coefficients/summary(test)$standard.errors
      p <- (1 - pnorm(abs(z), 0, 1)) * 2
      p 
      
      #permutati median_distance
      null_1=multinom(fate~rank_node+1,data = PROL_bud_scale)
      dif=test$AIC-null_1$AIC
      met_nul=PROL_bud_scale
      
      df=data.frame(matrix(nrow=0, ncol=0))
      for (i in 1:1000) {
        met_nul$median_distance=sample(PROL_bud_scale$median_distance)
        perm=multinom(fate~median_distance+rank_node,data = met_nul)
        a=perm$AIC-null_1$AIC
        b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
        r=cbind(i,a, b)
        df=rbind(df,r)
      }
      better_perm=length(which(df$b==1))#times better perm!!!
      #graph
      png("3S_2.png",width=1200, height=900, res=150)
      plot(allEffects(test))
      dev.off()
      
      #keeping just node rank
      #5: rank_node####
      datas=PROL_bud_scale[PROL_bud_scale$rank_node<=16,]
      test=multinom(fate~rank_node,data = datas)
      summary(test)
      exp(coef(test))
      #computing p value
      z <- summary(test)$coefficients/summary(test)$standard.errors
      p <- (1 - pnorm(abs(z), 0, 1)) * 2
      p 
      
      #preficted value
      df=data.frame(rank_node=(seq(0,max(datas$rank_node),
                                   length.out=length(unique(datas$rank_node)))))
      pred=cbind(df,predict(test, newdata = df, "probs", se=T))
      pred
      #real value
      tab=as.matrix(prop.table(table(datas$rank_node,datas$fate),1))
      tab
      #graph
      png("3S_rank.png",width=1200, height=900, res=150)# save plot
      par(mar=c(5,5,5,5))
      cols<-brewer.pal(n=3,name="Set2")
      r=barplot(t(tab),
                col = cols, names.arg = row.names(tab),
                xlab = "rank node",
                ylab="proportion of B,M,V", ylim = c(0,1))
      with(pred, lines(x = r,pred$V,lwd=5, col="green"))
      with(pred, lines(x = r,pred$B,lwd=5, col="orange"))
      with(pred, lines(x = r,pred$M,lwd=5, col="blue"))
      legend("topright",
             inset=c(-0.13,0),xpd = TRUE,
             title = "bud fate",
             legend = c(colnames(tab),paste0("predict",colnames(tab))),
             lty=c(NA,NA,NA,1,1,1),fill = c(cols, c("green","orange","blue")), cex=0.8)
      dev.off()
      
      #6: median distance####
      test=multinom(fate~median_distance,data = PROL_bud_scale)
      summary(test)
      exp(coef(test))
      #computing p value
      z <- summary(test)$coefficients/summary(test)$standard.errors
      p <- (1 - pnorm(abs(z), 0, 1)) * 2
      p 
      
      #preficted value
      df=data.frame(median_distance=(seq(min(PROL_bud_scale$median_distance),max(PROL_bud_scale$median_distance),
                                         length.out=length(unique(PROL_bud_scale$median_distance)))))
      pred=cbind(df,predict(test, newdata = df, "probs", se=T))
      pred
      #real value
      tab=as.matrix(prop.table(table(PROL_bud_scale$median_distance,PROL_bud_scale$fate),1))
      tab
      #graph
      png("3S_distace.png",width=1200, height=900, res=150)# save plot
      par(mar=c(5,5,5,5))
      cols<-brewer.pal(n=3,name="Set2")
      r=barplot(t(tab),
                col = cols, names.arg = row.names(tab),
                xlab = "median distance",
                ylab="proportion of B,M,V", ylim = c(0,1))
      with(pred, lines(x = r,pred$V,lwd=5, col="green"))
      with(pred, lines(x = r,pred$B,lwd=5, col="orange"))
      with(pred, lines(x = r,pred$M,lwd=5, col="blue"))
      legend("topright",
             inset=c(-0.13,0),xpd = TRUE,
             title = "bud fate",
             legend = c(colnames(tab),paste0("predict",colnames(tab))),
             lty=c(NA,NA,NA,1,1,1),fill = c(cols, c("green","orange","blue")), cex=0.8)
      dev.off()
      
      #7: not happy with the graphs sq.root of rank####
      
      data.poly = PROL_bud_scale[PROL_bud_scale$rank_node<=16,]
      rank_node = data.poly$rank_node
      data.poly = cbind(data.poly, rank_node**0.5, rank_node**2, rank_node**3, rank_node**4)
      ndata.poly = dim(data.poly)[2]
      names(data.poly)[(ndata.poly-3):ndata.poly] = c("rank_node0.5", "rank_node2", "rank_node3", "rank_node4")
      test=multinom(fate~rank_node+rank_node0.5+rank_node2+rank_node3+rank_node4,data = data.poly)
      summary(test)
      exp(coef(test))
      #computing p value
      z <- summary(test)$coefficients/summary(test)$standard.errors
      p <- (1 - pnorm(abs(z), 0, 1)) * 2
      p 
      
      #preficted value
                         #length.out=length(unique(data.poly$rank_node)))
      p_rank_node=barplot(t(tab),
               col = cols, names.arg = row.names(tab),
               xlab = "rank node",
               ylab="proportion of B,M,V", ylim = c(0,1))
      #  p_rank_node = seq(1,max(d  ata.poly$rank_node))
      
      df=data.frame(rank_node=p_rank_node,
                    rank_node0.5=p_rank_node**0.5,
                    rank_node2=p_rank_node**2,
                    rank_node3=p_rank_node**3,
                    rank_node4=p_rank_node**4)
      pred=cbind(df,predict(test, newdata = df, "probs", se=T))
      pred
      tab=as.matrix(prop.table(table(data.poly$rank_node,data.poly$fate),1))
      tab
      #graph
      png("3S_sqr.rank.png",width=1200, height=900, res=150)# save plot
      par(mar=c(5,5,5,5))
      cols<-brewer.pal(n=3,name="Set2")
      r=barplot(t(tab),
                col = cols, names.arg = row.names(tab),
                xlab = "rank node",
                ylab="proportion of B,M,V", ylim = c(0,1))
      with(pred, lines(x = p_rank_node,pred$V,lwd=5, col="green"))
      with(pred, lines(x = p_rank_node,pred$B,lwd=5, col="orange"))
      with(pred, lines(x = p_rank_node,pred$M,lwd=5, col="blue"))
      legend("topright",
             inset=c(-0.13,0),xpd = TRUE,
             title = "bud fate",
             legend = c(colnames(tab),paste0("predict",colnames(tab))),
             lty=c(NA,NA,NA,1,1,1),fill = c(cols, c("green","orange","blue")), cex=0.8)
      dev.off()
      
