#confronto tra dati simulati e dati reali:
#-lunghezza germoglio e numero nodi nei nuovi germogli
#2023
#francesca Grisafi

library(plotrix);library(data.table)

real_new_shoot=data.frame(fread("data/real_prol_buds_deruta.csv",stringsAsFactors = T))
#take just vegetative buds
real_new_shoot=droplevels(real_new_shoot[real_new_shoot$fate=="V",])
real_new_shoot= droplevels(real_new_shoot[!is.na(real_new_shoot$length2yo),])

#dataset where, for each simulation, without sampling 26sh, 25me, 28lo,25vlo shoots 
all_sim=data.frame(fread("outputs/simulations/parent_success.csv",stringsAsFactors = T))
#take just vegetative buds
all_sim=droplevels(all_sim[all_sim$fate=="V",])
all_sim= droplevels(all_sim[!is.na(all_sim$new_length_cm),])

#chategorycal variable to parent shoot class
cats = c("Sh", "Me", "Lo", "VLo")

real_new_shoot$length_newshoots=factor(real_new_shoot$length_newshoots, levels = cats)
all_sim$new_cat=factor(all_sim$new_cat, levels =cats)

# all SIMULATED -----------------------------------------------------
x = c(1.5,3.5,5.5,7.5)
cols=palette()[2]
bx = boxplot(real_new_shoot$nb_nodes_new~real_new_shoot$length_newshoots,
             ylim = c(0,25),xlab = "length category",ylab = "nb nodes", col="grey",at = x)
boxplot(all_sim$new_nodes~all_sim$new_cat,add = T, col = cols[1],at =x+0.3, axes = FALSE)
legend(1,28,legend = c("real", "simulated"), fill=c("grey",cols), cex=0.8,xpd=NA)

