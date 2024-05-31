# AIM: split the dataframes into proleptic and sylleptic & modify proleptic
import pandas as pd


def importdataset(bud, bud_proleptic, bud_sylleptic, MV_bud_pro, MV_bud_syl, met_proleptic,
                  met_sylleptic, all_met_proleptic, all_met_sylleptic, shoot):
    '''
    Import multiple CSV files and store them in a dictionary.

    Parameters:
    bud (str): Path to bud.csv
    bud_proleptic (str): Path to bud_proleptic.csv
    bud_sylleptic (str): Path to bud_sylleptic.csv
    MV_bud_pro (str): Path to MV_bud_pro.csv
    MV_bud_syl (str): Path to MV_bud_syl.csv
    met_proleptic (str): Path to met_proleptic.csv
    met_sylleptic (str): Path to met_sylleptic.csv
    all_met_proleptic (str): Path to all_met_proleptic.csv
    all_met_sylleptic (str): Path to all_met_sylleptic.csv
    shoot (str): Path to shoot.csv

    Returns:
    dict: Dictionary containing all imported DataFrames.
    '''
    data = {
        'bud': pd.read_csv(bud),
        'bud_proleptic': pd.read_csv(bud_proleptic),
        'bud_sylleptic': pd.read_csv(bud_sylleptic),
        'MV_bud_PRO': pd.read_csv(MV_bud_pro),
        'MV_bud_SYL': pd.read_csv(MV_bud_syl),
        'met_proleptic': pd.read_csv(met_proleptic),
        'met_sylleptic': pd.read_csv(met_sylleptic),
        'met_all_proleptic': pd.read_csv(all_met_proleptic),
        'met_all_sylleptic': pd.read_csv(all_met_sylleptic),
        'shoot': pd.read_csv(shoot)
    }
    return data