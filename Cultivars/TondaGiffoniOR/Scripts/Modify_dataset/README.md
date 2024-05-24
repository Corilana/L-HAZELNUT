The original dataset was an EXCELL file.
Due to my inexperties in Tree Architecture the ways data were collected were not in the best format to be analysed.

Here are the scripts to thange the formats of the dataset

|scripts |what does it do|
|---|---|
|1_create_shoot_level_OR.R| create a dataframe where each row is a 1 year old shoot|
|2_create_metamer_level_OR.R| create a dataframe where each row is a metamer of each 1 year old shoot|
|3_modify_shoot_level_OR.R|addin the info, at shoot level, if the shoot was found IN 2021 or not|
|4_create_bud_level_OR.R|create a dataframe at bud level JUST FOR THE SHOOTS found in 2021|
|5_modify_shoot&met_level_OR.R|add the nb of buds in SYLLEPTIC at shoot level|
|5_modify_shoot&met_level_OR.R|adjust nb nuts, in met and shoot level, according to bud level|
|6_WRITE_dataset_OR.R| split the dataframes into proleptic and sylleptic & modify proleptic|
|import_OR.R|read the csv files originated from the 1-6 scripts|
