#AIM: split the dataframes into proleptic and sylleptic & modify proleptic
#data=shoots found in 2021 (Deruta)
#Phd=Francesca Grisafi

library(janitor);library(readxl);library(dplyr);library(data.table)

source("Cultivars/TondaGiffoniOR/Scripts/Modify_dataset/5_modify_shoot&met_level_OR.R")

met.all.proleptic=met
rm(bud,shoot,met)
bud=foundnxtyr_bud_lev
shoot=foundnxtyr_shoot_lev
met=foundnxtyr_met_lev
rm(foundnxtyr_bud_lev,foundnxtyr_shoot_lev,foundnxtyr_met_lev)

# sylleptic ---------------------------------------------------------------
#met level
met.all.sylleptic=droplevels(met.all.proleptic[met.all.proleptic$shoot_type=="SYLLEPTIC",])
names(met.all.sylleptic)
length=grep("length$",names(met.all.sylleptic))
length_no=grep("Length",names(met.all.sylleptic))
rank=grep("rank",names(met.all.sylleptic))
colnames(met.all.sylleptic)[c(length,length_no,rank)] = c("parent_length_cm",
                                                      "parent_length_node",
                                                      "parent_rank_node")


met.sylleptic=droplevels(met.all.sylleptic[!(is.na(met.all.sylleptic$nb_new_shoots)), ])
#bud level
bud.sylleptic=droplevels(bud[bud$shoot_type=="SYLLEPTIC",])
names(bud.sylleptic)
bud.sylleptic = dplyr::mutate(bud.sylleptic, "norm_median_distance" = NA, .after = median_distance)
bud.sylleptic$norm_median_distance = round((bud.sylleptic$median_distance /
                                              bud.sylleptic$`Length(node)`),
                                           1)
names(bud.sylleptic)
length=grep("length$",names(bud.sylleptic))
length_no=grep("Length",names(bud.sylleptic))
rank=grep("rank",names(bud.sylleptic))
colnames(bud.sylleptic)[c(length,length_no,rank)] = c("parent_length_cm",
                                        "parent_length_node",
                                        "parent_rank_node")

MV.bud.SYL = droplevels(bud.sylleptic[bud.sylleptic$fate == "M" |
                                        bud.sylleptic$fate == "V", ])
MV.bud.SYL$fate = factor(MV.bud.SYL$fate, c("V", "M"))

# proleptic ---------------------------------------------------------------
#met level
met.proleptic=met

tot=grep("tot_buds_mvb",names(met))
c=grep("c_syl",names(met))
mv_s=grep("tot_buds_syl_m_v",names(met))
met$tot_buds_m_v_b_c=rowSums(met[c(tot,c,mv_s)],na.rm = T)

#bud level
bud.proleptic = droplevels(bud[bud$shoot_type == "PROLEPTIC", ])
colnames(bud.proleptic)
bud.proleptic$fate = factor(bud.proleptic$fate, c("V", "B", "M"))

str(bud.proleptic)
bud.proleptic$norm_median_distance = round((bud.proleptic$median_distance /
                                              bud.proleptic$`Length(node)`),
                                           1)

MV.bud.PRO = droplevels(bud.proleptic[bud.proleptic$fate == "M" |
                                bud.proleptic$fate == "V", ])
MV.bud.PRO$siblings_mvb = MV.bud.PRO$m + MV.bud.PRO$v + MV.bud.PRO$b

toremove <- grep("proleptic$|sylleptic$|PRO$|SYL$|shoot|^bud$|^met$", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))

#write
fwrite(bud, "Cultivars/TondaGiffoniOR/Data/bud.csv")
fwrite(bud.proleptic, "Cultivars/TondaGiffoniOR/Data/bud_proleptic.csv")
fwrite(bud.sylleptic, "Cultivars/TondaGiffoniOR/Data/bud_sylleptic.csv")
fwrite(met, "Cultivars/TondaGiffoniOR/Data/met_proleptic.csv")
fwrite(met.sylleptic, "Cultivars/TondaGiffoniOR/Data/met_sylleptic.csv")
fwrite(met.all.sylleptic, "Cultivars/TondaGiffoniOR/Data/all_met_sylleptic.csv")
fwrite(met.all.proleptic, "Cultivars/TondaGiffoniOR/Data/all_met_proleptic.csv")
fwrite(MV.bud.PRO, "Cultivars/TondaGiffoniOR/Data/MV_bud_pro.csv")
fwrite(MV.bud.SYL, "Cultivars/TondaGiffoniOR/Data/MV_bud_syl.csv")
fwrite(shoot, "Cultivars/TondaGiffoniOR/Data/shoot.csv")
