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

def get_fiscal_year_mo(ym):
    year = ym // 100  # Extract the year part
    month_num = ym % 100  # Extract the month part
    
    if month_num > 6:
        year += 1
    return year

def attribute_portfolios_mo(stoxmo):
    # Add additional attributes
    
    stoxmo_add = (stoxmo
                .assign(fiscalyear=lambda x: x['ym'].apply(get_fiscal_year_mo),  # Apply getFiscalYearMo to each 'ym'
                        mb=lambda x: (x['csho'] * x['prcc_f']) / x['ceq'],
                        me=lambda x: x['csho'] * x['prcc_f'],
                        kk_share=lambda x: x['K_int_Know'] / x['ppegt'],
                        CUSIP8=lambda x: x['cusip'].str.slice(0, -2))  # Slice the 'cusip' strings
                )
    pfs = (stoxmo_add
       [stoxmo_add['ym'] % 100 == 6]  # Filter rows where 'ym' ends with 06, indicating June
       .drop(columns=['cusip'])  # Drop the 'cusip' column
       .dropna(subset=['me', 'mb'])  # Drop rows with NA in 'me' or 'mb' columns
       .groupby('y', group_keys=False)  # Group by 'y' without preserving group keys
       .apply(lambda group: group.assign(
           med_NYSE_me=group.loc[group['exchg'] == 11, 'me'].median(),  # Calculate median of 'me' for 'exchg' == 11
           med_NYSE_mb70p=group.loc[group['exchg'] == 11, 'mb'].quantile(0.7),  # 70th percentile of 'mb'
           med_NYSE_mb30p=group.loc[group['exchg'] == 11, 'mb'].quantile(0.3)  # 30th percentile of 'mb'
       ))
      )
    
# Assuming pfs is your DataFrame
    pfs = (pfs.assign(
            me_group=np.where(pfs['me'] < pfs['med_NYSE_me'], 1, 2),
            mb_group=np.select(
                [
                    pfs['mb'] < pfs['med_NYSE_mb30p'],
                    pfs['mb'] >= pfs['med_NYSE_mb30p'],
                    pfs['mb'] <= pfs['med_NYSE_mb70p'],
                    pfs['mb'] > pfs['med_NYSE_mb70p']
                ],
                [
                    1, 2, 2, 3
                ],
                default=np.nan
            )
                    )
            .drop(columns=['med_NYSE_me', 'med_NYSE_mb30p', 'med_NYSE_mb70p'])
            .assign(pf6_name=lambda x: 10*x['me_group'] + x['mb_group'])
            .groupby('y', group_keys=False)
            .apply(lambda x: x.assign(
                me_3tile=pd.qcut(x['me'], 3, labels=False) + 1,
                mb_3tile=pd.qcut(x['mb'], 3, labels=False) + 1,
                pf25_name=lambda x: 10*(pd.qcut(x['me'], 5, labels=False) + 1) + (pd.qcut(x['mb'], 5, labels=False) + 1)
            ))
            .assign(pf36_name=lambda x: 100*x['ntile_topic_kk'] + 10*x['me_3tile'] + x['mb_3tile'])
            .groupby(['me_3tile', 'mb_3tile'], group_keys=False)
            .apply(lambda x: x.assign(kkr_3tile_inner=pd.qcut(x['topic_kk'], [0, 0.8, 0.9, 0.95,  1], duplicates = 'drop', labels=False) + 1))
            .assign(pf27_name=lambda x: 100*x['kkr_3tile_inner'] + 10*x['me_3tile'] + x['mb_3tile'])
            .assign(fiscalyear=lambda x: x['fiscalyear'] + 1)
            .loc[:, ['fiscalyear', 'gvkey', 'pf6_name', 'pf25_name', 'pf36_name', 'pf27_name']]
            )
    
    stoxmo_add = pd.merge(stoxmo_add, pfs, on=["fiscalyear", "gvkey"], how="inner")
# List indices of pfs:

    return stoxmo_add

def create_eret_mo_panel_ff5(stoxmo_orig, cequity_mapper, topic_map, ff5fm, pfn):    

    stoxmo = dl.clean_stoxmo_ff5(stoxmo_orig, cequity_mapper, topic_map, ff5fm)  # Assuming this is already converted to Python
    stoxmo_with_pfs = attribute_portfolios_mo(stoxmo)  # Adjusted the function name to Python convention
    
    
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
    
    return eret_mo, stoxmo_with_pfs

def create_eret_we_panel_ff5(stoxwe_orig, cequity_mapper, topic_map, pfn, ff5fw):
    # Prepare the data with additional columns and joins
    stoxwe = stoxwe_orig.copy()
    stoxwe['y'] = stoxwe['yw'] // 100
    stoxwe = stoxwe.merge(cequity_mapper, on=['PERMNO', 'y'], how='inner')
    stoxwe = stoxwe[stoxwe['crit_ALL'] == 1]
    stoxwe = stoxwe.merge(topic_map, left_on=['PERMNO', 'y'], right_on=['LPERMNO', 'year'], how='inner')
    stoxwe = stoxwe[stoxwe['y'] >= topic_map['year'].min()]
    stoxwe = stoxwe.merge(ff5fw, on='yw', how='left')
    stoxwe['eretw'] = stoxwe['retw'] - stoxwe['RF']
    stoxwe = stoxwe.dropna(subset=['retw'])

    # Attribute portfolios
    stoxwe_with_pfs = attribute_portfolios_we(stoxwe)

    # Calculate portfolio returns
    pf_ret = stoxwe_with_pfs.dropna(subset=['eretw', 'me']).groupby(['yw', pfn]).apply(
        lambda x: pd.Series({
            'eret': (x['eretw'] * x['me']).sum() / x['me'].sum(),
            'Mkt.RF': x['Mkt.RF'].mean(),
            'SMB': x['SMB'].mean(),
            'HML': x['HML'].mean(),
            'CMA': x['CMA'].mean(),
            'RMW': x['RMW'].mean(),
            'RF': x['RF'].mean()
        })
    ).reset_index()

    # Calculate kkrhml returns
    def calc_kkrhml_ret(group):
        return (group['eretw'] * group['me']).sum() / group['me'].sum()

    kkrhml_ret = stoxwe_with_pfs.dropna(subset=['topic_kk']).groupby(['yw', 'ntile_topic_kk']).apply(calc_kkrhml_ret).unstack().reset_index()
    kkrhml_ret['kkrhml'] = kkrhml_ret[4] - kkrhml_ret[1]
    kkrhml_ret = kkrhml_ret[['yw', 'kkrhml']]

    # Assuming similar logic for kkpthml_ret as for kkrhml_ret
    # This example doesn't implement the pivot_wider equivalent directly due to pandas' handling differences

    # Merge and finalize the eret_we DataFrame
    eret_we = pf_ret.merge(kkrhml_ret, on='yw', how='inner')
    eret_we = eret_we.rename(columns={'eret': 'eretw'}).dropna().reset_index(drop=True)

    return [eret_we, stoxwe_with_pfs]

# Note: This example assumes the existence of a function attributePortfoliosWe, and similar setup/dataframes as in the R code.
# Adjustments may be needed to match the exact functionality, especially for complex data manipulation and pivot operations.

def attribute_portfolios_we(stoxwe):
    # Adding additional columns
    stoxwe['fiscalyear'] = stoxwe['yw'].apply(get_fiscal_year_we)
    stoxwe['mb'] = (stoxwe['csho'] * stoxwe['prcc_f']) / stoxwe['ceq']
    stoxwe['me'] = stoxwe['csho'] * stoxwe['prcc_f']
    stoxwe['kk_share'] = stoxwe['K_int_Know'] / stoxwe['ppegt']
    stoxwe['CUSIP8'] = stoxwe['cusip'].str[:-1]

    # Filtering and selecting data for portfolio assignment
    pfs = stoxwe[stoxwe['yw'] % 100 == 26].copy()
    pfs = pfs.drop(columns=['cusip'])
    pfs['med_NYSE_me'] = pfs.groupby('y')['me'].transform(lambda x: x[x.index[pfs['exchg'] == 11].median()])
    pfs['med_NYSE_mb70p'] = pfs.groupby('y')['mb'].transform(lambda x: np.quantile(x[pfs['exchg'] == 11], 0.7))
    pfs['med_NYSE_mb30p'] = pfs.groupby('y')['mb'].transform(lambda x: np.quantile(x[pfs['exchg'] == 11], 0.3))

    # Portfolio classification
    pfs['me_group'] = np.where(pfs['me'] < pfs['med_NYSE_me'], 1, 2)
    
    def mb_group(row):
        if row['mb'] < row['med_NYSE_mb30p']:
            return 1
        elif row['mb'] <= row['med_NYSE_mb70p']:
            return 2
        else:
            return 3
    pfs['mb_group'] = pfs.apply(mb_group, axis=1)
    
    pfs['pf6_name'] = 10 * pfs['me_group'] + pfs['mb_group']
    
    # More classifications
    for col in ['me', 'mb']:
        pfs[f'{col}_3tile'] = pd.qcut(pfs[col], 3, labels=False) + 1
        pfs[f'{col}_5tile'] = pd.qcut(pfs[col], 5, labels=False) + 1
    
    pfs['pf25_name'] = 10 * pfs['me_5tile'] + pfs['mb_5tile']
    # Assuming ntile_topic_kk and topic_kk need to be defined or calculated before this step
    # This example does not implement these due to missing context

    # Joining back to original DataFrame
    stoxwe_add = pd.merge(stoxwe, pfs[['gvkey', 'pf6_name', 'pf25_name', 'fiscalyear']], on=['fiscalyear', 'gvkey'], how='inner')
    
    return stoxwe_add

# Note: This example assumes the existence and correct setup of the DataFrame stoxwe and the necessary variables.
# It also assumes that certain columns (e.g., 'K_int_Know', 'ppegt') and logic (e.g., ntile_topic_kk) are appropriately defined.

def get_fiscal_year_we(yw):
    # Placeholder for the actual fiscal year calculation logic
    # Assuming yw is a year-week format integer
    year = yw // 100
    week = yw % 100
    # Adjust based on fiscal year logic
    if week > 26:
        return year + 1
    else:
        return year

import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.formula.api import ols
from statsmodels.regression.linear_model import OLS

def fama_macbeth(mo_window, pfn):
    # Placeholder for the first_stage_rollwin_mo calculation
    # Assuming `rwin_coefs_mo_mult` is equivalent in Python and returns a DataFrame
    first_stage_rollwin_mo = rwin_coefs_mo_mult(mo_window, mo_window)
    
    # Second stage regression for each `ym`
    # Assuming the DataFrame is already prepared similarly to R's output
    results = []

    for ym, group in first_stage_rollwin_mo.groupby('ym'):
        formula = 'eretm ~ kkrhml + HML + SMB + CMA + RMW + Mkt_RF - 1'
        model = ols(formula, data=group).fit()
        params = model.params.reset_index()
        params.columns = ['term', 'estimate']
        params['ym'] = ym
        results.append(params)

    second_stage_rollwin_mo = pd.concat(results).pivot(index='ym', columns='term', values='estimate')

    return first_stage_rollwin_mo, second_stage_rollwin_mo

import pandas as pd
import numpy as np
from statsmodels.regression.linear_model import OLS
from statsmodels.tools.tools import add_constant

def roll_reg_full5ff(subdf):
    # Placeholder for the regression function that should be applied on each window
    # Assuming it returns regression coefficients or another summary statistic
    # Adjust according to what roll_reg_full5ff actually does in your R code
    pass

def rwin_coefs_mo_mult(eret_mo, window_size, pfn):
    output = pd.DataFrame()
    pfnames = eret_mo[pfn].unique()
    
    for pfname in pfnames:
        subdf = eret_mo[eret_mo[pfn] == pfname]
        subdf = subdf[subdf['ym'] < 202000]  # Filter based on 'ym'

        # Preallocate a DataFrame to hold the results for this pfname
        results = pd.DataFrame(index=subdf.index, columns=['eret', 'ym', 'pfname'])

        # Loop over each window
        for start in range(len(subdf) - window_size + 1):
            window = subdf.iloc[start:start + window_size]
            # Apply your regression or other operation within this window
            # For example, placeholder for applying OLS regression on each window
            # Adjust this to use your actual regression or operation
            result = roll_reg_full5ff(window)  # Adjust this call to match your needs

            # Store the result in the preallocated DataFrame
            results.iloc[start + window_size - 1] = result

        # Add metadata columns
        results['eretm'] = subdf['eretm'].values  # Assuming this is the correct column to carry over
        results['ym'] = subdf['ym'].values
        results['pfname'] = pfname

        # Append to the output DataFrame
        output = pd.concat([output, results])

    # Drop rows with NA values, assuming similar to `drop_na()` in R
    output.dropna(inplace=True)

    return output

def roll_reg_full5ff(z):
    # Assuming z is a DataFrame with the necessary columns
    # Prepare the independent variables (add a constant for the intercept)
    X = sm.add_constant(z[['Mkt.RF', 'SMB', 'HML', 'CMA', 'RMW', 'kkrhml']])
    # The dependent variable
    y = z['eret']
    
    # Fit the model
    model = sm.OLS(y, X).fit()
    
    # Return the coefficients
    return model.params

import pandas as pd
import numpy as np
from statsmodels.formula.api import ols
from patsy import dmatrices
import statsmodels.formula.api as smf

def first_stage(formula, eretdf, pfn):
    # Group by pfn and perform regression for each group
    grouped = eretdf.groupby(pfn)
    results_list = []

    for name, group in grouped:
        model = ols(formula, data=group).fit()
        summary_df = pd.DataFrame({
            'estimate': model.params,
            'std.error': model.bse,
            'statistic': model.tvalues,
            'p.value': model.pvalues,
            pfn: name,
            'eretw': np.mean(group['eretw']),
            't': len(group['eretw'])
        }).reset_index().rename(columns={'index': 'term'})
        results_list.append(summary_df)

    first_stage1 = pd.concat(results_list, ignore_index=True)

    # Calculate sigmae
    first_stage1['sigmae'] = first_stage1['t'] * first_stage1['std.error']**2
    get_sigmae = first_stage1[first_stage1['term'] == "(Intercept)"][[pfn, 'sigmae']].dropna()

    # Prepare the output dataframe
    # Filter and reshape the dataframe
    first_stage2 = first_stage1[first_stage1['term'] != "(Intercept)"]
    pivot_df = first_stage2.pivot(index=pfn, columns='term', values='estimate').reset_index()

    # Merge sigmae
    first_stage2 = pivot_df.merge(get_sigmae, on=pfn, how='left').dropna()

    return first_stage2

def run_regression_models(first_stage2):
    # OLS Regression
    formula_ols = 'eretw ~ kkrhml + HML + SMB + CMA + RMW + Mkt_RF'
    model_ols = smf.ols(formula=formula_ols, data=first_stage2).fit()
    
    # WLS Regression without kkrhml
    formula_wls_nokk = 'eretw ~ HML + SMB + CMA + RMW + Mkt_RF'
    model_wls_nokk = smf.wls(formula=formula_wls_nokk, data=first_stage2, weights=1 / first_stage2['sigmae']).fit()
    
    # WLS Regression with kkrhml
    formula_wls = 'eretw ~ kkrhml + HML + SMB + CMA + RMW + Mkt_RF'
    model_wls = smf.wls(formula=formula_wls, data=first_stage2, weights=1 / first_stage2['sigmae']).fit()
    
    return model_ols, model_wls_nokk, model_wls


# Note: This function assumes 'formula' is a string that statsmodels can interpret,
# 'eretdf' is a pandas DataFrame containing the data, and 'pfn' is the name of the column
# by which to group the data before fitting the model.


