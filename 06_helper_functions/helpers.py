import pandas as pd
import os
from pathlib import Path

def get_prov_code(province):
    script_location = Path(__file__).absolute().parent
    file_location = script_location / '../01_data/00_raw/02_psgc_codes/PH_Adm2_ProvDists.csv'

    # load csv file
    df_psgc_prov = pd.read_csv(file_location)

    # get code for province
    prov_psgc = df_psgc_prov.loc[df_psgc_prov['adm2_en'] == province].iloc[0]['adm2_psgc']

    # delete dataframe
    del df_psgc_prov

    return prov_psgc

def get_mun_codes(prov_psgc):
    script_location = Path(__file__).absolute().parent
    file_location = script_location / '../01_data/00_raw/02_psgc_codes/PH_Adm3_MuniCities.csv'

    # load csv file
    df_psgc_mun = pd.read_csv(file_location)

    # filter dataframe
    df_psgc_mun = df_psgc_mun[df_psgc_mun['adm2_psgc'] == prov_psgc]
    mun_pgsc = df_psgc_mun[['adm3_en', 'adm3_psgc']].copy()

    # delete dataframe
    del df_psgc_mun

    return mun_pgsc