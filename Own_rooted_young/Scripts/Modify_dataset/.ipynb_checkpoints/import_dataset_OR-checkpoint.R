#AIM: split the dataframes into proleptic and sylleptic & modify proleptic
#data=shoots found in 2021 (Deruta)
#Phd=Francesca Grisafi

library(data.table)

bud = fread("Own_rooted_young/Data/bud.csv")
bud.proleptic = fread("Own_rooted_young/Data/bud_proleptic.csv")
bud.sylleptic = fread("Own_rooted_young/Data/bud_sylleptic.csv")

met = fread("Own_rooted_young/Data/met_proleptic.csv")
met.proleptic = fread("Own_rooted_young/Data/met_proleptic.csv")
met.sylleptic = fread("Own_rooted_young/Data/met_sylleptic.csv")
met.all.proleptic = fread("Own_rooted_young/Data/all_met_proleptic.csv")
met.all.sylleptic = fread("Own_rooted_young/Data/all_met_sylleptic.csv")

MV.bud.PRO = fread("Own_rooted_young/Data/MV_bud_pro.csv")
MV.bud.SYL = fread("Own_rooted_young/Data/MV_bud_syl.csv")

shoot = fread("Own_rooted_young/Data/shoot.csv")


