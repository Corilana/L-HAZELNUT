#confronto tra dati simulati e dati reali:
#-lunghezza germoglio parentale e numero nodi
#2023
#francesca Grisafi

library(plotrix);library(data.table)

#importiamo REAL dataset
real_shoot=data.frame(fread("data/real_annual_shoot_deruta.csv",stringsAsFactors = T))
head(real_shoot)
#il dataset è su scala GERMOGLIO

#importiamo SIMULATED dataset (campionato come Deruta)
all_sim=data.frame(fread("data/parent_sampled_as_deruta.csv",stringsAsFactors = T))
head(all_sim)
#il dataset è su scala GEMMA

#unifichiamo dataset SIMULATED in modo da portarlo alla stessa scala del real
start_col = grep("shoot_id", names(all_sim))
fin_col = grep("^nodes$", names(all_sim))
all_sim= droplevels(unique(all_sim[c(start_col : fin_col,ncol(all_sim))]))
head(all_sim)

#chategorycal variable to parent shoot class
cats = c("Sh", "Me", "Lo", "VLo")
real_shoot$class=factor(real_shoot$class, levels = cats)
all_sim$cat=factor(all_sim$cat, levels = cats)

#check normality 
plot(density(real_shoot$node))
plot(density(all_sim$nodes))

# boxplot -----------------------------------------------------
x = c(1.5,3.5,5.5,7.5)
cols=palette()[2]
bx = boxplot(real_shoot$node~real_shoot$class,
             ylim = c(0,25),xlab = "length category",ylab = "nb nodes", col="grey",at = x)
boxplot(all_sim$nodes~all_sim$cat,add = T, col = cols[1],at =x+0.3, axes = FALSE)
#t-test
names(real_shoot)[c(4,7,10)] = names(all_sim)[c(2,3,4)]
real_shoot$sim = "real"
all_sim$sim = "simulated"
unique_df = rbind(real_shoot[c(4,7,10,ncol(real_shoot))], all_sim[c(2,3,4,ncol(all_sim))])
unique_df$sim = as.factor(unique_df$sim)
unique_df$cat = factor(unique_df$cat, levels = cats)
for (i in 1:length(cats)) {
  c = cats[i]
  test = unique_df[unique_df$cat == c,]
  mod = t.test(nodes~sim, data = test)
  if (mod$p.value<=0.05) {
    lab = "*"
    print(mod)
  }
  if (mod$p.value<=0.01){
    lab = "**"
    print(mod)
  }
  if (mod$p.value<=0.001){
    lab = "***"
    print(mod)
  }
  if (mod$p.value>0.05) {
    lab = NA
  }
  text(x = mean(c(x[i],(x[i]+0.3))),y = max(test$nodes)+1,labels = lab, cex = 1.5)

}
legend(1,28,legend = c("real", "simulated"), fill=c("grey",cols), cex=0.8,xpd=NA)


# scatter plot -----------------------------------------------------
cols=palette()[2]
plot(all_sim$nodes~all_sim$length_cm,col = cols[1],ylim = c(0,25),xlab = "length (cm)",ylab = "nodes (number)",pch = 19)
points(real_shoot$node~real_shoot$length,col="black",pch = 19)
legend(1,28,legend = c("Observation", "576 simulations"), fill=c("black",cols), cex=1,xpd=NA)

#sim
a = all_sim$length_cm>10 & all_sim$length_cm<=25

mean(all_sim[a,"nodes"]) #8.71
sd(all_sim[a,"nodes"])/sqrt(length(all_sim[a,"nodes"])) #0.02

#obs
b = real_shoot$length_cm>10 & real_shoot$length_cm<=25

mean(real_shoot[b,"nodes"]) #8.00
sd(real_shoot[b,"nodes"])/sqrt(length(real_shoot[b,"nodes"])) #0.31

t.test(all_sim[a,"nodes"],real_shoot[b,"nodes"])


#sim
a = all_sim$length_cm>40 & all_sim$length_cm<=50

mean(all_sim[a,"nodes"]) #16.36
sd(all_sim[a,"nodes"])/sqrt(length(all_sim[a,"nodes"])) #0.02

#obs
b = real_shoot$length_cm>40 & real_shoot$length_cm<=50

mean(real_shoot[b,"nodes"]) #17.12
sd(real_shoot[b,"nodes"])/sqrt(length(real_shoot[b,"nodes"])) #0.53

t.test(all_sim[a,"nodes"],real_shoot[b,"nodes"])
