#box1: does that rank bear a sylleptic?
#objectiv: using GLM to understand the % of syllepotic shoots

source("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral/glm/init_metlev.R")
source("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral/glm/permutation_glm.R")

#parameters: length(cm), length(node), rank_node, distance, fate
#every time 1 parameter is not sig. or permutation shows difference >1% i discard it

#1: sylleptic~length(cm)+rank+distance####
glm_box1=glm(is_sylleptic~parent_length_cm+parent_rank_node+distance_abs,family = "binomial",data = met)
summary(glm_box1)

#permut rank
permutation(dip="is_sylleptic",
            predictors = c("parent_length_cm","distance_abs"),
            perm = "parent_rank_node",data = met,family = "binomial")

#2: sylleptic~length(cm)+distance####
glm_box1=glm(is_sylleptic~parent_length_cm+distance_abs,family = "binomial",data = met)
summary(glm_box1)

#permut parent_length
permutation(dip="is_sylleptic",
            predictors = c("distance_abs"),
            perm = "parent_length_cm",data = met,family = "binomial")

#3: sylleptic~distance####
glm_box1=glm(is_sylleptic~distance_abs,family = "binomial",data = met)
summary(glm_box1)

#permut distance
permutation(dip="is_sylleptic",
            perm = "distance_abs",data = met,family = "binomial")

#real_data_#proportion sylleptic
prop=as.data.frame.matrix(prop.table(table(met$distance_abs,met$is_sylleptic),1))
colnames(prop)[1:2]=c("no_syl","si_syl")
#new column with the distance
prop$dist=as.numeric(rownames(prop))
head(prop)
#predicted
prop$pred=predict(glm_box1,
                  newdata = data.frame(distance_abs=seq(0, max(prop$dist), length.out = length(prop$dist))),
                  type="response")
#graph
png("1.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(prop, plot(prop$ratio~prop$dist,
          col = cols[1],
          main="%sylleptic (#sylleptic/tot_nodes) vs distance from median node(node)",
          xlab= "distance from median node(node)",
          ylab="%sylleptic",
          ylim=c(0,1),
          type="h",
          lwd=4))
with(prop, lines(prop$pred~prop$dist,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

# #1: sylleptic~length(cm)+rank+distance__same analysis but with distance normalized (distance/tot nodes)####
# glm_box1=glm(is_sylleptic~parent_length_cm+parent_rank_node+normal_distance,family = "binomial",data = met)
# summary(glm_box1)
# #2: sylleptic~length(cm)+distance####
# glm_box1=glm(is_sylleptic~parent_length_cm+normal_distance,family = "binomial",data = met)
# summary(glm_box1)#yes
# #3: sylleptic~distance####
# glm_box1=glm(is_sylleptic~normal_distance,family = "binomial",data = met)
# summary(glm_box1)
# #real data_#proportion sylleptic
# prop=as.data.frame.matrix(prop.table(table(met$normal_distance,met$is_sylleptic),1))
# colnames(prop)[1:2]=c("no_syl","si_syl")
# #new column with distance
# prop$dist=as.numeric(rownames(prop))
# head(prop)
# #predicted
# prop$pred=predict(glm_box1,
#                   newdata = data.frame(normal_distance=seq(0, max(prop$dist), length.out = length(prop$dist))),
#                   type="response")
# #graph
# png("1a.png",width=1200, height=900, res=150)# save plot
# cols<-brewer.pal(n=4,name="Set2")[3:4]
# with(prop, plot(prop$ratio~prop$dist,
#                 col = cols[1],
#                 main="%sylleptic (#sylleptic/tot_nodes) vs distance from median node normalized",
#                 xlab= "distance from median node normalized",
#                 ylab="%sylleptic",
#                 ylim=c(0,1),
#                 type="h",
#                 lwd=4))
# with(prop, lines(prop$pred~prop$dist,lwd=5, col=cols[2]))
# legend("top",
#        horiz=T,
#        xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
# dev.off()