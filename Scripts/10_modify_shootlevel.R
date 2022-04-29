#modify shoot level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/")
library(dplyr)

#adding number of laterals and apicals buds per each shoot
ann=read.csv("auto/2020shoot_level_DEVELOPED.csv")
bud=read.csv("auto/bud_level_FINAL.csv")
ann=dplyr::mutate(ann, n_lateral_buds=NA, .after = found_next_yea)
ann=dplyr::mutate(ann, n_apical_buds=NA, .after = n_lateral_buds)
ann=dplyr::mutate(ann, fate_apicals=NA, .after = n_apical_buds)
ann=dplyr::mutate(ann, buds_in_sylleptic=NA, .after = fate_apicals)

nline=length(ann$shoot)
po=grep("posi", colnames(bud))
fa=grep("fate", colnames(bud))
fa_a=grep("fate", colnames(ann))
sy=grep("from_", colnames(bud))
n_l=grep("n_l", colnames(ann))
n_a=grep("n_a", colnames(ann))
n_s=grep("n_s", colnames(ann))
for (i in 1:nline) {
  s=ann$shoot[i]#identify the shoot
  l=length(bud[bud$shoot==s&bud$position=="LATERAL",po])#how many laterals
  a=length(bud[bud$shoot==s&bud$position=="AP",po])#many apicals
  syl=length(bud[bud$shoot==s&bud$position=="LATERAL"&bud$from_=="SYL",sy])#many sylleptic
  if (a==1) {
    f=bud[bud$shoot==s&bud$position=="AP",fa]
    ann[ann$shoot==s,fa_a]=f
    }
  ann[ann$shoot==s,n_l]=l
  ann[ann$shoot==s,n_a]=a
  ann[ann$shoot==s,n_s]=syl
}

ann=ann[2:19]
write.csv(ann, "auto/mtp use/2020shoot_level_DEVELOPED_fin.csv", row.names = F)

#modify the annual df to remove the apical buds from the laterals count
ann=read.csv("auto/mtp use/2020shoot_level_DEVELOPED_fin.csv")
bud=read.csv("auto/bud_level_FINAL.csv")
fate=c("M","V","B","C")
nline=length(ann$tesi)

#loop to delete apical bud if "fate_apicals" is that type of bud
for (i in 1:nline) {
  f=ann$fate_apicals[i]
  if (length(f)!=0) {
    ap=length(grep(paste(fate,collapse="|"),f, value = T))
    col=which(colnames(ann)== casefold(f))
    ann[i,col]=ann[i,col]-1
  }
}

ann=ann[ann$n_lateral_buds!=0,]

write.csv(ann,"auto/shoot_level_develop_lateralbuds.csv", row.names = F)
