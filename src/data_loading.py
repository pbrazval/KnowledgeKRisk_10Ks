import pandas as pd, os, pyreadr
import warnings
import os
import time
import numpy as  np
import pandas as pd
import random
import pandas as pd
import visualization as viz
import data_loading as dl
import risk_pricing as rp


def clean_patent_ik_orig(patent_ik_orig, linkt):
    patent_ik = patent_ik_orig.merge(linkt, left_on='permno', right_on='LPERMNO')
    patent_ik['year'] = patent_ik['issue_date'].str[6:10].astype(float)
    patent_ik = patent_ik.sort_values(by=['gvkey', 'year'])
    patent_ik['xi_real'] = patent_ik['xi_real'].fillna(0)
    patent_ik = patent_ik.groupby(['gvkey', 'year']).agg(xi_yeartotal=('xi_real', 'sum')).reset_index()
    patent_ik['xi_cumsum'] = patent_ik.groupby('gvkey').apply(lambda x: x['xi_yeartotal'].cumsum()).reset_index(drop=True)
    return patent_ik

def clean_linkt_orig(linkt_orig, start_time):
    # Convert 'naics' to string first to handle 'substr' in Python
    linkt_orig['naics'] = linkt_orig['naics'].astype(str)
    
    # Group by 'gvkey' and fill missing 'LPERMNO' values downwards and upwards
    linkt_orig['LPERMNO'] = linkt_orig.groupby('gvkey')['LPERMNO'].transform(lambda x: x.ffill().bfill())

    #linkt = linkt_orig.groupby('gvkey').apply(lambda group: group.assign(LPERMNO=group['LPERMNO'].ffill().bfill())).reset_index(drop=True)
    
    # Creating 'CUSIP8' by slicing 'cusip'
    linkt_orig['CUSIP8'] = linkt_orig['cusip'].str.slice(0, 8)
    #linkt['CUSIP8'] = linkt['cusip'].str.slice(0, 8)

    # Selecting specific columns and creating 'naics4'
    #linkt = linkt[['CUSIP8', 'LPERMNO', 'sic', 'cik', 'gvkey', 'conm', 'naics']]
    linkt_orig['naics4'] = linkt_orig['naics'].str.slice(0, 4)
    linkt = linkt_orig[['CUSIP8', 'LPERMNO', 'sic', 'cik', 'gvkey', 'conm', 'naics4']]

    linkt = linkt.drop_duplicates().dropna(subset=['LPERMNO', 'cik'])

    # Apply custom functions 'create_ind12' and 'add_hi_tech_column' for each group
    linkt = create_ind12(linkt)
    linkt = add_hi_tech_column(linkt)
    
    # Remove duplicate rows and drop rows with NA in 'LPERMNO' and 'cik'
    # Arrange by 'LPERMNO' and 'cik', then take the first row of each 'LPERMNO' group
    linkt = linkt.sort_values(by=['LPERMNO', 'cik'])
    linkt = linkt.groupby('LPERMNO').head(1)

    return linkt

def load_dataframes(modelname, figfolder, start_time, clean_again=False):
    if clean_again:
        
        patent_ik_orig = pd.read_csv("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/KPSS_2020_public.csv")
        amazon_nov01_short = pd.read_csv("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/amazon_nov01_short.csv")
        cequity_mapper = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/cequity_mapper.csv")
        ff3fw_orig = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/ff3fw.csv")
        ff5fw_orig = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/ff5fw.csv")
        ff3fm_orig = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/FF3F.csv")
        ff5fm_orig = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/FF5F.csv")
        linkt_orig = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/CRSP-Compustat Merged Database - Linking Table.csv")
        ff5fm_orig = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/FF5F.csv")
        comp_funda2 = pd.read_pickle("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/comp_funda2.pkl")
        peterstaylor = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/peterstaylor.csv")
        skilldata_orig = pd.read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/belo_labor_skill_data.csv")
        topic_map_orig = pd.read_csv(f"~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/{modelname}/topic_map_2006_2022.csv")

        stoxwe_post2005short_dict = pyreadr.read_r("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxwe_post2005short.Rdata")
        stoxwe_post2005short = stoxwe_post2005short_dict["stoxwe_post2005short"]

        stoxda_post2005veryshort_dict = pyreadr.read_r("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxda_post2005veryshort.Rdata")
        stoxda_post2005veryshort = stoxda_post2005veryshort_dict["stoxwe_post2005short"]

        stoxmo_post2000short_dict = pyreadr.read_r("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxmo_post2000short.Rdata")
        stoxmo_post2000short = stoxmo_post2000short_dict["stoxmo_post2000short"]

        comparison_measures = pd.read_csv("~/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/comparison_measures.csv")
        #stoxda_orig = stoxwe_post2005short
        stoxmo_orig = stoxmo_post2000short ## Defining source of stocks information
        
        # I commented the line below because the number of rows diverged from the R version when redoing in Python:
        # cequity_mapper = redo_equity_mapper(stoxmo_post2000short, comp_funda2, figfolder)
        # Save all dataframes above to a pkl in sister folder "data":
        
        linkt = clean_linkt_orig(linkt_orig, start_time)
        print("Tempo de execução:", time.time() - start_time)
        
        patent_ik = clean_patent_ik_orig(patent_ik_orig, linkt)
        print("Tempo de execução:", time.time() - start_time)
        
        ff3fm, ff5fm, ff3fw, ff5fw = cleanff_all(ff3fm_orig, ff5fm_orig, ff3fw_orig, ff5fw_orig)
        print("Tempo de execução:", time.time() - start_time)
        
        skilldata = clean_skilldata(skilldata_orig)
        print("Tempo de execução:", time.time() - start_time)

        compustat_pt = clean_compustat(comp_funda2, peterstaylor)
        print("Tempo de execução:", time.time() - start_time)

        topic_map_unlabeled = create_topic_map_unlabeled(topic_map_orig, linkt, skilldata, patent_ik, compustat_pt)
        print("Tempo de execução:", time.time() - start_time)

        
        variable_names = ['amazon_nov01_short', 'ff3fw', 'ff5fw',
            'ff3fm', 'ff5fm', 'topic_map_unlabeled', 'stoxmo_orig', 'comparison_measures', 'cequity_mapper'
        ]

        for var_name in variable_names:
            # Get the actual variable object using globals() and save it as a pickle
            # Replace 'globals()' with 'locals()' if these are local variables
            locals()[var_name].to_pickle(f"../data/{var_name}.pkl")

        
    else:
        
        #Load dataframes from pkl:
        #patent_ik = pd.read_pickle(f"../data/patent_ik.pkl")
        amazon_nov01_short = pd.read_pickle(f"../data/amazon_nov01_short.pkl")
        cequity_mapper = pd.read_pickle(f"../data/cequity_mapper.pkl")
        ff3fw = pd.read_pickle(f"../data/ff3fw.pkl")
        ff5fw = pd.read_pickle(f"../data/ff5fw.pkl")
        ff3fm = pd.read_pickle(f"../data/ff3fm.pkl")
        ff5fm = pd.read_pickle(f"../data/ff5fm.pkl")
        #linkt = pd.read_pickle(f"../data/linkt.pkl")
        # Load dataframes from pkl:
        topic_map_unlabeled = pd.read_pickle(f"../data/topic_map_unlabeled.pkl")
        #stoxda_orig = pd.read_pickle(f"../data/stoxda_orig.pkl")
        stoxmo_orig = pd.read_pickle(f"../data/stoxmo_orig.pkl")
        comparison_measures = pd.read_pickle(f"../data/comparison_measures.pkl")
        #comp_funda2 = pd.read_pickle(f"../data/comp_funda2.pkl")
        #peterstaylor = pd.read_pickle(f"../data/peterstaylor.pkl")
        #skilldata_orig = pd.read_pickle(f"../data/skilldata_orig.pkl")
        stoxwe_post2005short = pd.read_pickle(f"../data/stoxwe_post2005short.pkl")
        print("Tempo de execução depois de carregar os dados:", time.time() - start_time)

    return amazon_nov01_short, cequity_mapper, ff3fw, \
        ff5fw, ff3fm, ff5fm, \
            topic_map_unlabeled, stoxmo_orig,\
                comparison_measures, stoxwe_post2005short

def create_ind12(df):
    # Add a new column 'ind12' with NaN values
    df['ind12'] = np.nan

    # Define SIC code sequences
    seq1 = list(range(100, 1000)) + list(range(2000, 2400)) + list(range(2700, 2750)) + list(range(2770, 2800)) + list(range(3100, 3200)) + list(range(3940, 3990))
    seq2 = list(range(2500, 2520)) + list(range(2590, 2600)) + list(range(3630, 3660)) + list(range(3710, 3712)) + [3714, 3716] + list(range(3750, 3752)) + [3792] + list(range(3900, 3940)) + list(range(3990, 4000))
    seq3 = list(range(2520, 2590)) + list(range(2600, 2700)) + list(range(2750, 2770)) + list(range(3000, 3100)) + list(range(3200, 3570)) + list(range(3580, 3630)) + list(range(3700, 3710)) + list(range(3712, 3714)) + [3715] + list(range(3717, 3750)) + list(range(3752, 3792)) + list(range(3793, 3800)) + list(range(3830, 3840)) + list(range(3860, 3900))
    seq4 = list(range(1200, 1400)) + list(range(2900, 3000))
    seq5 = list(range(2800, 2830)) + list(range(2840, 2900))
    seq6 = list(range(3570, 3580)) + list(range(3660, 3693)) + list(range(3694, 3700)) + list(range(3810, 3830)) + list(range(7370, 7380))
    seq7 = list(range(4800, 4900))
    seq8 = list(range(4900, 4950))
    seq9 = list(range(5000, 6000)) + list(range(7200, 7300)) + list(range(7600, 7700))
    seq10 = list(range(2830, 2840)) + [3693] + list(range(3840, 3860)) + list(range(8000, 8100))
    seq11 = list(range(6000, 7000))

    # Assign values to 'ind12' based on 'sic' values
    df.loc[df['sic'].isin(seq1), 'ind12'] = 1
    df.loc[df['sic'].isin(seq2), 'ind12'] = 2
    df.loc[df['sic'].isin(seq3), 'ind12'] = 3
    df.loc[df['sic'].isin(seq4), 'ind12'] = 4
    df.loc[df['sic'].isin(seq5), 'ind12'] = 5
    df.loc[df['sic'].isin(seq6), 'ind12'] = 6
    df.loc[df['sic'].isin(seq7), 'ind12'] = 7
    df.loc[df['sic'].isin(seq8), 'ind12'] = 8
    df.loc[df['sic'].isin(seq9), 'ind12'] = 9
    df.loc[df['sic'].isin(seq10), 'ind12'] = 10
    df.loc[df['sic'].isin(seq11), 'ind12'] = 11

    # Assign 12 to 'ind12' where it is NaN
    df['ind12'].fillna(12, inplace=True)

    return df

def add_hi_tech_column(df):
    hi_tech_sic = [283, 357, 366, 367, 382, 384, 737]
    df['sic3'] = df['sic'].astype(str).str[:3].astype(float)
    df['hi_tech'] = df['sic3'].isin(hi_tech_sic).astype(int)
  
    return df

def redo_equity_mapper(stoxmo_post2000short, comp_funda2, textfolder):
    # Load data
    cpi_orig = pd.read_csv("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/CPIAUCSL.csv") # Adjust the path as needed

    # Data manipulation similar to dplyr in R
    crit_exchg = comp_funda2[['cik', 'LPERMNO', 'exchg', 'fyear']]
    crit_exchg['crit_EXCHG'] = crit_exchg['exchg'].isin([11, 12, 14])

    cpi = cpi_orig.rename(columns={'CPIAUCSL': 'cpi'})
    cpi['DATE'] = pd.to_datetime(cpi['DATE'])

    cpi['ym'] = cpi['DATE'].dt.strftime('%Y%m').astype(int)
    cpi['m'] = cpi['DATE'].dt.month
    cpi['y'] = cpi['DATE'].dt.year

    ref_cpi = cpi.loc[cpi['ym'] == 201501, 'cpi'].iloc[0]

    # Data merging and manipulation
    cequity_mapper = stoxmo_post2000short.copy()
    cequity_mapper['ym'] = cequity_mapper['date'] // 100
    cequity_mapper = cequity_mapper.merge(cpi, on='ym', how='left')
    cequity_mapper = cequity_mapper.merge(crit_exchg, left_on=['PERMNO', 'y'], right_on=['LPERMNO', 'fyear'], how='left')
    cequity_mapper = cequity_mapper[cequity_mapper['m'] == 6]
    cequity_mapper['constp_PRC'] = cequity_mapper['PRC'] * ref_cpi / cequity_mapper['cpi']
    cequity_mapper['crit_PRC'] = cequity_mapper['constp_PRC'] > 5
    cequity_mapper['crit_SHRCD'] = cequity_mapper['SHRCD'].isin([10, 11])
    cequity_mapper['crit_EXCHG'] = cequity_mapper['crit_EXCHG'].astype(float)
    cequity_mapper.sort_values(by='y', inplace=True)
     
    cequity_mapper['crit_EXCHG'] = cequity_mapper['crit_EXCHG'].fillna(method='ffill').astype(bool)
    cequity_mapper['crit_EXCHG'] = cequity_mapper['crit_EXCHG'].fillna(False)
    cequity_mapper['crit_ALL'] = cequity_mapper['crit_PRC'] & cequity_mapper['crit_SHRCD'] & cequity_mapper['crit_EXCHG']
    cequity_mapper = cequity_mapper.dropna(subset=['crit_ALL'])
    cequity_mapper = cequity_mapper[['PERMNO', 'y'] + [col for col in cequity_mapper.columns if col.startswith('crit_')]]

    # Group by and summarization
    # Filter only rows of cequity_mapper where cequity_mapper['y'] >= 2013:
    
    
    mean_groups = cequity_mapper[cequity_mapper['y'] >= 2013]
    mean_groups = mean_groups.groupby('y').agg(
        mean_EXCHG=('crit_EXCHG', lambda x: round(x.mean(), 3)),
        mean_COMEQ=('crit_SHRCD', lambda x: round(x.mean(), 3)),
        mean_PRC=('crit_PRC', lambda x: round(x.mean(), 3)),
        mean_ALL=('crit_ALL', lambda x: round(x.mean(), 3))
    ).reset_index()

    # Exporting to LaTeX table - requires additional package, e.g., tabulate
    # Improve below:
    tex_table = mean_groups.to_latex(index=False, label='tab:stocks_filtering_criteria', caption='Stocks filtering criteria')
    with open(f"{textfolder}/stocks_filtering_criteria.tex", "w") as file:
         file.write(tex_table)
    
    print("Acabei o redo_equity_mapper")

    return cequity_mapper

def cleanff(ffdf):
    outdf = ffdf.copy()
    outdf.rename(columns={outdf.columns[0]: 'ym'}, inplace=True)
    outdf = outdf.apply(lambda x: np.log(1 + x / 100) if x.name != "ym" else x)
    return outdf

def cleanffw(ffdf):
    # Rename the first column of ffdf to 'date' and save the result to outdf:
    outdf = ffdf.rename(columns={"Unnamed: 0": "date"})
    outdf["date"] = pd.to_datetime(outdf["date"], errors="coerce", format="%Y%m%d")
    outdf["yw"] = outdf["date"].dt.year * 100 + outdf["date"].dt.isocalendar().week
    outdf = outdf.drop(columns=["date"])
    outdf = outdf.apply(lambda x: np.log(1 + x / 100) if x.name in ["Mkt-RF", "SMB", "HML", "RF", "CMA", "RMW"] else x)
    # Drop row of outdf if there is any NA in it:
    outdf = outdf.dropna()
    return outdf

def cleanff_all(ff3fm_orig, ff5fm_orig, ff3fw_orig, ff5fw_orig):
    ff3fm = cleanff(ff3fm_orig).dropna()
    ff5fm = cleanff(ff5fm_orig).dropna()
    ff3fw = cleanffw(ff3fw_orig).dropna()
    ff5fw = cleanffw(ff5fw_orig).groupby("yw").sum().reset_index()
    return ff3fm, ff5fm, ff3fw, ff5fw

def clean_skilldata(skilldata_orig):
    unique_naics4 = skilldata_orig[skilldata_orig['YEAR'] == 2013]['ind'].unique()

    # Create a DataFrame with all combinations of YEAR and naics4
    years = range(2014, 2023)
    expandgrid = pd.DataFrame([(year, naics4) for year in years for naics4 in unique_naics4], columns=['YEAR', 'naics4'])

    # Rename 'YEAR' to 'year'
    expandgrid.rename(columns={'YEAR': 'year'}, inplace=True)
        
    skilldata = skilldata_orig.rename(columns={'ind': 'naics4', 'YEAR': 'year'}).merge(expandgrid, on=['naics4', 'year'], how='outer').sort_values(['year', 'naics4'])
    skilldata['Skill'] = skilldata.groupby('naics4')['Skill'].fillna(method='ffill')
    return skilldata

def clean_compustat(comp_funda2, peterstaylor):
    comp_funda2 = comp_funda2.loc[:, ["at", "GVKEY", "fyear", "prcc_f", "prcc_c", "ppegt", "csho", "ceq", "cusip", "exchg"]]
    comp_funda2.dropna(subset=['GVKEY', 'fyear'], inplace=True)
    comp_funda2['fyear'] = comp_funda2['fyear'].astype(int)
    comp_funda2['GVKEY'] = comp_funda2['GVKEY'].astype(int)
    compustat_pt = comp_funda2.merge(peterstaylor, left_on=["fyear", "GVKEY"], right_on=["fyear", "gvkey"]) \
        .rename(columns={"fyear": "year"}) \
        .loc[:, ["K_int_Know", "K_int", "at", "gvkey", "year", "prcc_f", "prcc_c", "ppegt", "csho", "ceq", "cusip", "exchg"]]
    return compustat_pt

def create_topic_map_unlabeled(topic_map_orig, linkt, skilldata, patent_ik, compustat_pt):
    # CIKs are not unique -- they may map to multiple PERMNOs. May need to consolidate later.
    topic_map_unlabeled = topic_map_orig.merge(linkt, left_on="CIK", right_on="cik", how="left") 
    topic_map_unlabeled = topic_map_unlabeled.astype({"naics4": float})
    topic_map_unlabeled = topic_map_unlabeled.merge(skilldata, on=["naics4", "year"], how="left")
    topic_map_unlabeled = topic_map_unlabeled.merge(patent_ik, on=["gvkey", "year"], how="left")
    topic_map_unlabeled = topic_map_unlabeled.sort_values(["gvkey", "year"])
    # group by gvkey and fill missing values of xi_cumsum downwards
    topic_map_unlabeled["xi_cumsum"] = topic_map_unlabeled.sort_values(["gvkey", "year"]).groupby("gvkey")["xi_cumsum"].fillna(method="ffill")
    topic_map_unlabeled["xi_cumsum"] = topic_map_unlabeled["xi_cumsum"].fillna(0)
    topic_map_unlabeled["xi_yeartotal"] = topic_map_unlabeled["xi_yeartotal"].fillna(0)
    topic_map_unlabeled = topic_map_unlabeled.merge(compustat_pt, on=['gvkey', 'year'], how='left')
    topic_map_unlabeled['xir_cumsum'] = np.where(topic_map_unlabeled['xi_cumsum'].isna(), 0, topic_map_unlabeled['xi_cumsum'] / topic_map_unlabeled['at'])
    topic_map_unlabeled['xir_total'] = np.where(topic_map_unlabeled['xi_yeartotal'].isna(), 0, topic_map_unlabeled['xi_yeartotal'] / topic_map_unlabeled['at'])
    input("Press Enter to continue...")
    columns_to_fill = ['K_int_Know', 'K_int', 'at', 'Skill']
    for col in columns_to_fill:
        topic_map_unlabeled[col] = topic_map_unlabeled.groupby(['gvkey', 'year'])[col].transform(lambda x: x.fillna(method='ffill').fillna(method='bfill'))

        #topic_map_unlabeled[col] = topic_map_unlabeled.groupby(['gvkey', 'year'])[col].fillna(method='ffill')
        #topic_map_unlabeled[col] = topic_map_unlabeled.groupby(['gvkey', 'year'])[col].fillna(method='bfill')

    # Taking the first row of each group
    topic_map_unlabeled = topic_map_unlabeled.groupby(['gvkey', 'year']).head(1).reset_index(drop=True)
    
    industry_names = ["Cnsmr non-dur.", "Cnsmr durbl", "Manuf", "Enrgy", "Chems", "BusEq", "Telcm", 
                    "Utils", "Whlsl/Retail", "Hlth", "Other", "NoDef"]
    industry_map = {i+1: name for i, name in enumerate(industry_names)}
    topic_map_unlabeled['ind12'] = topic_map_unlabeled['ind12'].map(industry_map)
    topic_map_unlabeled['ind12'] = topic_map_unlabeled['ind12'].astype('category')
    
    return topic_map_unlabeled

def clean_stoxmo_ff5(stoxmo_orig, cequity_mapper, topic_map, ff5fm):
    # Convert 'RET' to numeric and select necessary columns
    stoxmo = (stoxmo_orig.copy().
              assign(retm=pd.to_numeric(stoxmo_orig['RET'], errors='coerce')).
              loc[:, ['date', 'PERMNO', 'retm']].
              assign(date = lambda x: x['date'].astype(int)).
              dropna(subset=['retm', 'date']).
              assign(date = lambda x: pd.to_datetime(x['date'], format='%Y%m%d', errors = "coerce"),
                     y = lambda x: x['date'].dt.year,
                     ym = lambda x: x['y']*100 + x['date'].dt.month).
              merge(cequity_mapper, left_on=['PERMNO', 'y'], right_on=['PERMNO', 'year'], how='left', validate="m:m").
              drop(columns=['gvkey', 'cik', 'year']).
              query('crit_ALL == 1').
              merge(topic_map, left_on=['PERMNO', 'y'], right_on=['LPERMNO', 'year'], how='left', validate="m:m").
              pipe(lambda df: df[df['y'] >= topic_map['year'].min()]).
              merge(ff5fm, on='ym', how='left').
              dropna(subset=['retm']).
              assign(eretm = lambda x: x['retm'] - x['RF'])
              )
    
# Example usage:
    return stoxmo

# Assuming stoxmo_orig, cequity_mapper, topic_map, and ff5fm are defined and loaded as pandas DataFrames
# cleaned_stoxmo = clean_stoxmo_ff5(stoxmo_orig, cequity_mapper, topic_map, ff5fm)
