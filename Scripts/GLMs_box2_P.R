#box2_proleptic: how many buds??
#objectiv: how many buds in that node?

source("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral/glm/init_metlev_proleptic.R")
source("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral/glm/permutation_glm.R")

#parameters: length(cm), length(node), rank_node, distance, fate
#every time 1 parameter is not sig. or permutation shows difference >1% i discard it

#1: tot_buds ~ parent lenght(cm)+rank_node+length(node)?####
glm_box1=glm(tot_buds~rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)
#real
nline=length(unique(sort(PROL_met_scale$rank_node)))
#store the column
mv=grep("tot_buds", colnames(PROL_met_scale))
#empty df
dt=met[0,0]
#for loop to have the average and se of tot buds
for (i in 1:nline) {
  L=unique(sort(PROL_met_scale$rank_node))[i]
  av=round(mean(PROL_met_scale[PROL_met_scale$rank_node==L,mv]), digits = 2)
  n=length(PROL_met_scale[PROL_met_scale$rank_node==L,mv])
  se=round(std.error(PROL_met_scale[PROL_met_scale$rank_node==L,mv]), digits=2)
  if(n==1){
    se=0
    marg=0}
  else{
    t=qt(0.975,df=n-1)
    #confidence interval = coef * se
    marg=se*t
  }
  d=cbind(L,av,se, marg)
  dt=rbind(dt,d)
}
#graph
png("2m_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(dt,plot(dt$av~dt$L,
             col = cols[1],
             main="average #buds(b,v,m) vs rank node",
             xlab= "rank node",
             ylab="#buds(b,v,m)",
             pch=19,
             ylim=c(0,3.5)))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
dev.off()

#average mv
av=round(mean(PROL_met_scale[,mv]), digits = 2)
#number of buds
n=length(PROL_met_scale[,mv])
#standard error
se=round(std.error(PROL_met_scale[,mv]), digits=2)

#1: tot_buds ~ parent #null model
glm_box1=glm(tot_buds~1,family = "poisson",data = PROL_met_scale)
summary(glm_box1)

plot(glm_box1)

