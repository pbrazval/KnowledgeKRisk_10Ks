#%% Importing libraries and setting up the environment
import pandas as pd, os, pyreadr
import warnings
import time
import numpy as  np
import pandas as pd
import random
import pandas as pd
import visualization as viz
import data_loading as dl
import risk_pricing as rp

print(pd.__version__)

#% PARAMETERS

source_folder_path = "/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/src/"
modelname = "dicfullmc10thr10defnob40noa0_8_4t"
quantiles = 4
clean_again = True
warnings.filterwarnings("ignore")
print_kurtosis = False
pfn = "pf36_name"
mo_window = 12*5

#% PARAMETERS

start_time = time.time()
# print the current working directory (for reference)
print("Current Working Directory:", os.getcwd())
os.chdir(source_folder_path)
print("New Working Directory:", os.getcwd())



# Creating the directory
base_path = "/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/text/"
dir_path = os.path.join(base_path, modelname)
os.makedirs(dir_path, exist_ok=True)

# Setting up the figure folder path
figfolder = os.path.join(dir_path, "")

# Loading the data from load_dataframes:
amazon_nov01_short, cequity_mapper, ff3fw, ff5fw, \
  ff3fm, ff5fm, topic_map_unlabeled, \
    stoxmo_orig, comparison_measures, \
      stoxwe_post2005short = dl.load_dataframes(modelname, \
    source_folder_path, start_time, clean_again)
# Pause execution of code and return to the command line:


topic_map, labels = viz.label_dicfullmc10thr10defnob40noa1_4t(topic_map_unlabeled)
print("Running time:", time.time() - start_time)

topic_kk = labels[0]

topic_map = viz.generate_plots(topic_map, figfolder, start_time, quantiles)

print("Running time:", time.time() - start_time)
stoxmo = dl.clean_stoxmo_ff5(stoxmo_orig, cequity_mapper, topic_map, ff5fm)  # Assuming this is already converted to Python
stoxmo_with_pfs = rp.attribute_portfolios_mo(stoxmo)  # Adjusted the function name to Python convention

# Calculate portfolio returns
pf_ret = (stoxmo_with_pfs.dropna(subset=['eretm', 'me'])
          .groupby(['ym', pfn])
          .agg(eret=('eretm', lambda x: np.average(x, weights=stoxmo_with_pfs.loc[x.index, 'me'])),
                Mkt_RF=('Mkt-RF', 'mean'),
                SMB=('SMB', 'mean'),
                HML=('HML', 'mean'),
                RMW=('RMW', 'mean'),
                CMA=('CMA', 'mean'),
                RF=('RF', 'mean'))
          .reset_index())

# Calculate kkrhml returns
def calc_returns(group):
    weights = group['me']
    return np.nansum(group['eretm'] * weights) / np.sum(weights)

#%% Next steps
kkrhml_ret = (stoxmo_with_pfs
              .dropna(subset=['topic_kk'])
              .groupby(['ym', 'ntile_topic_kk'])
              .apply(calc_returns)
              .unstack(level='ntile_topic_kk')
              .rename(columns=lambda x: f'kk{x}')
              .assign(kkrhml=lambda x: x['kk4'] - x['kk1'])
              .reset_index()[['ym', 'kkrhml']])

# Assume kkpt_ntile calculation is similar to kkrhml_ret
# This section would be adjusted based on actual logic for kkpt_ntile

# Join and finalize eret_mo dataframe
eret_mo = pf_ret.merge(kkrhml_ret, on='ym', how='inner')  # Assuming kkpthml_ret joins similarly
eret_mo = eret_mo.rename(columns={'eret': 'eretm'}).dropna().reset_index(drop=True)
    
#eret_mo, stoxmo_with_pfs = rp.create_eret_mo_panel_ff5(stoxmo_orig, cequity_mapper, topic_map, ff5fm, pfn)

#%% Next steps
eret_we, stoxwe_with_pfs = rp.create_eret_we_panel_ff5(stoxwe_post2005short, cequity_mapper, topic_map, pfn)
first_stage_rollwin_mo, second_stage_rollwin_mo = rp.fama_macbeth(mo_window, pfn)

first_stage2 = rp.first_stage(first_stage_rollwin_mo, pfn)

rp.run_regression_models(first_stage2)

viz.plot_returns()

viz.descriptive_statistics(topic_map, figfolder)

viz.amazon_graph(amazon_nov01_short, figfolder)

viz.filecounter(figfolder)

viz.stargaze_comparison(comparison_measures, figfolder)


if print_kurtosis:
  viz.plot_kurtosis(stoxda_orig, cequity_mapper, topic_map)
# %%
