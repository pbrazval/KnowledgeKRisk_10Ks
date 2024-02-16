# Data manipulation and numerical operations
import pandas as pd
import numpy as np

# Date and time operations
from datetime import datetime

# Plotting and visualization
import matplotlib.pyplot as plt
import seaborn as sns
from plotnine import ggplot, aes, geom_tile, geom_text, scale_fill_gradient2, labs, theme, element_text
import matplotlib.dates as mdates

# Utilities and performance optimization
import pyreadr
import time
import numba
import concurrent.futures

def label_dicfullmc10thr10defnob40noa1_4t(topic_map_unlabeled):
    k = 0
    labels = ["topic_" + str(k), str(k)]
    
    topic_map_labeled = topic_map_unlabeled.copy()
    
    topic_map_labeled.rename(columns={"topic_0": "topic_kk", "topic_1": "topic_finl", "topic_2": "topic_sw", "topic_3": "topic_rawm"}, inplace=True)
    
    return topic_map_labeled, labels

def personal_stargazer(mytable, textfolder, file_name, label, caption):
    latex_output = (mytable.
                to_latex(index=False, header=True, label = label, caption = caption, decimal = "."))
    file_name = textfolder + "tpcavg_tech.tex"
    with open(file_name, 'w') as f:
        f.write(latex_output)
    return None
# Data Manipulation using Pandas
def generate_plots(topic_map, figfolder, start_time, nt = 4):
    topic_map['ntile_topic_kk'] = (topic_map.
                                groupby('year')['topic_kk'].
                                transform(lambda x: pd.qcut(x, [0, 0.8, 0.9, 0.95,  1], labels=False, duplicates='raise')))
    
    create_topic_plots(topic_map, figfolder)
    
    topic_map_positiveikpt = topic_map.dropna(subset = ["K_int_Know", "at", "K_int"]).loc[(topic_map["K_int"] > 0), :]
    topic_map_positivekkpt = (topic_map
                              .dropna(subset = ["K_int_Know", "at", "K_int"])
                              .loc[(topic_map["K_int_Know"] > 0), :])
    topic_map_positivekkpt['kkpt_ntile'] = topic_map_positivekkpt.groupby('year')["K_int_Know"].transform(lambda x: pd.qcut(x, nt, labels=False, duplicates='drop'))
    topic_map_positiveikpt['ikpt_ntile'] = topic_map_positiveikpt.groupby('year')["K_int"].transform(lambda x: pd.qcut(x, nt, labels=False, duplicates='drop'))

    bytech = topic_map.dropna(subset = "hi_tech")
            
    bytech = (bytech.
            filter([col for col in bytech.columns if col.startswith("topic")]+["hi_tech"]).
            groupby("hi_tech").
            agg(lambda x: round(x.mean(), 3) if x.name.startswith('topic') else x))

    personal_stargazer(bytech, figfolder, "bytech_py", "tab:bytech", "Average Topic Loadings by High Tech")

    np.random.seed(139)
    sample_topics = (topic_map.
                    filter([col for col in topic_map.columns if col.startswith("topic")]+["conm", "year"]).
                    sample(n = 10).
                    sort_values(by = "conm").
                    rename(columns = {"conm": "Company_Name"}).
                    apply(lambda x: round(x, 3) if x.name.startswith('topic') else x, axis = 0))

    personal_stargazer(sample_topics, figfolder, "sample_topics_py", "tab:sample_topics", "Sample of Topic Loadings")    

    skillcor = (topic_map.
                filter([col for col in topic_map.columns if col.startswith("topic")]+["Skill"]))
    # Create an array with the correlation of each column that starts with "topic" with column "Skill":
    skill_correlations = skillcor.corr(method = "spearman").loc["Skill", [col for col in skillcor.columns if col.startswith("topic")]]

    patentcor = (topic_map.
                filter([col for col in topic_map.columns if col.startswith("topic")]+["xir_cumsum"]))
    patent_correlations = patentcor.corr(method = "spearman").loc["xir_cumsum", [col for col in patentcor.columns if col.startswith("topic")]]

    firms_by_ind = (topic_map
                    .loc[topic_map["ntile_topic_kk"]==3]
                    .groupby(["year", "ind12"])
                    .agg(count = ("year", "size"),
                        totalat = ("at", "sum"))
                    .reset_index(inplace = False))
    # Convert year and ind12 from index to columns:
    
    stackedplot_n = sns.barplot(data=firms_by_ind, x='year', y='count', hue='ind12', dodge=False)
    stackedplot_n.set_ylabel('Share of all dominant-KK firms')
    stackedplot_n.set_xlabel('Year')
    stackedplot_n.set_title('Share of all dominant-KK firms by Industry')
    stackedplot_n.legend(title='Industry', bbox_to_anchor=(1, 1))
    plt.xticks(rotation=45)

    # Save plot above to "stackedplot_at_py.png" inside the "figfolder" directory:
    stackedplot_n.figure.savefig(figfolder + "stackedplot_n_py.png", bbox_inches='tight', dpi=300)
    
    firms_by_kk = (topic_map_positivekkpt
                .groupby(['ntile_topic_kk', 'kkpt_ntile'])
                .size()
                .reset_index(name='count'))

    plot = (ggplot(firms_by_kk, aes(x='ntile_topic_kk', y='kkpt_ntile', fill='count')) +
            geom_tile(aes(fill='count')) +  # Use geom_tile for heatmap squares
            geom_text(aes(label='round(count, 2)'), size=8) +  # Add text labels
            scale_fill_gradient2(low="white", high="red", mid="pink", midpoint=firms_by_kk['count'].mean()) +  # Gradient fill
            labs(x="Quartiles of Knowledge Capital Risk measured by Topic_kk",
                y="Quartiles of Knowledge Capital Intensity",
                fill='Count') +  # Labels
            theme(legend_title=element_text(size=14),  # Adjust legend title font size
                legend_text=element_text(size=14),  # Adjust legend text font size
                axis_title=element_text(size=14),  # Adjust axis titles font size
                axis_text=element_text(size=14))  # Adjust axis texts font size
        )
    
    # Save the plot
    plot.save(figfolder + "topicvskkpt_hm_py.png", dpi=600, width=10, height=8, verbose=False)

    firms_by_ik = (topic_map_positiveikpt
                .groupby(['ntile_topic_kk', 'ikpt_ntile'])
                .size()
                .reset_index(name='count'))

    plot = (ggplot(firms_by_ik, aes(x='ntile_topic_kk', y='ikpt_ntile', fill='count')) +
            geom_tile(aes(fill='count')) +  # Use geom_tile for heatmap squares
            geom_text(aes(label='round(count, 2)'), size=8) +  # Add text labels
            scale_fill_gradient2(low="white", high="red", mid="pink", midpoint=firms_by_ik['count'].mean()) +  # Gradient fill
            labs(x="Quartiles of Intangible Capital Risk measured by Topic_kk",
                y="Quartiles of Intangible Capital Intensity",
                fill='Count') +  # Labels
            theme(legend_title=element_text(size=14),  # Adjust legend title font size
                legend_text=element_text(size=14),  # Adjust legend text font size
                axis_title=element_text(size=14),  # Adjust axis titles font size
                axis_text=element_text(size=14))  # Adjust axis texts font size
        )


    plot.save(figfolder + "topicvsikpt_hm_py.png", dpi=600, width=10, height=8, verbose=False)
    print("running time:", time.time() - start_time)
    print("Finished!")
    return topic_map

def amazon_graph(amazon_nov01_short, figfolder):
    # Convert 'Date' column to datetime format
    amazon_nov01_short['Date'] = pd.to_datetime(amazon_nov01_short['Date'])
    
    # Find the specific 'nasdaq' and 'amazon' values for November 13, 2001
    specific_date_values = amazon_nov01_short[amazon_nov01_short['Date'] == pd.to_datetime("2001-11-13")][['nasdaq', 'amazon']]
    
    # Index 'nasdaq' and 'amazon' to 100 based on their values on November 13, 2001
    amazon_nov01_short['nasdaq'] = 100 * amazon_nov01_short['nasdaq'] / specific_date_values['nasdaq'].values[0]
    amazon_nov01_short['amazon'] = 100 * amazon_nov01_short['amazon'] / specific_date_values['amazon'].values[0]
    
    # Plotting
    plt.figure(figsize=(10, 6))
    plt.plot('Date', 'nasdaq', data=amazon_nov01_short, color='blue', label='NASDAQ')
    plt.plot('Date', 'amazon', data=amazon_nov01_short, color='red', label='Amazon')
    plt.axvline(x=pd.to_datetime("2001-11-13"), linestyle='--', color='black')
    
    # Setting the title, labels, legend, and formatting the x-axis dates
    plt.title("NASDAQ vs Amazon Stock Prices in November 2001 (11/13/01 = 100)")
    plt.xlabel("Date")
    plt.ylabel("Stock Price")
    plt.legend()
    plt.xticks(rotation=45)
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    plt.gca().xaxis.set_major_locator(mdates.DayLocator(interval=1))
    plt.grid(True)
    
    # Save the plot
    plt.savefig(figfolder + "amazon_nov01_py.png", dpi=600)

def create_topic_plots(df, figfolder):
    # Select columns 'year' and those starting with 'topic_'
    df_filtered = df.filter(regex='^year|topic_.*')
    
    # Calculate the average topic intensity for each year
    avg_df = df_filtered.groupby('year').mean().reset_index()
    
    # Reshape the DataFrame from wide to long format
    long_df = pd.melt(avg_df, id_vars=['year'], var_name='topic', value_name='intensity')
    
    # Create the plot
    plt.figure(figsize=(10, 6))
    sns.lineplot(data=long_df, x='year', y='intensity', hue='topic')
    
    # Setting labels and title
    plt.xlabel("Year")
    plt.ylabel("Topic Intensity")
    plt.title("Mean Topic Intensity by Year")
    plt.legend(title='Topic', bbox_to_anchor=(1.05, 1), loc='upper left')
    
    # Adjust font sizes for readability
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.legend(fontsize=14, title_fontsize='13')
    
    # Save the plot
    plt.tight_layout()
    plt.savefig(figfolder + "mean_tiy_py.jpg", dpi=600)
    print("Create_topic_plots finished!")
    return None

def filecounter(textfolder):
    path = "/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/1A files/"
    year_file_counts = {}
    for year in range(2006, 2023):
        folder_path = os.path.join(path, str(year))
        if os.path.exists(folder_path):
            subfolder_names = ["Q1", "Q2", "Q3", "Q4"]
            year_file_count = 0

            for subfolder in subfolder_names:
                subfolder_path = os.path.join(folder_path, subfolder)
                if os.path.exists(subfolder_path):
                    year_file_count += len(os.listdir(subfolder_path))
            
            year_file_counts[year] = year_file_count

    # Load the CSV file
    lemmat_counts = pd.read_csv("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/descriptive/lemmat_counts.csv")
    # Create a DataFrame from year_file_counts
    file_counts_df = pd.DataFrame(list(year_file_counts.items()), columns=['Year', 'Total_1As'])
    # Merge or update lemmat_counts with the new file counts
    lemmat_counts = pd.merge(lemmat_counts, file_counts_df, on='Year', how='left')
    lemmat_counts.rename(columns={'Count': 'Filtered'}, inplace=True)
    lemmat_counts = lemmat_counts[['Year', 'Total_1As', 'Filtered']]

    # Generate LaTeX table (manually or using a library like pylatex)
    # For simplicity, we're saving the DataFrame to a CSV file
    # You can manually convert this CSV to a LaTeX table or use Python libraries if needed
    output_filepath = os.path.join(textfolder, "file_counts.csv")
    lemmat_counts.to_csv(output_filepath, index=False)

def stargaze_comparison(comparison_measures, figfolder):
    """
    Generate a LaTeX table from comparison_measures DataFrame and save it.

    Parameters:
    comparison_measures (pd.DataFrame): DataFrame containing the data to be tabled.
    figfolder (str): Folder path to save the LaTeX file.
    """
    # Convert DataFrame to LaTeX
    latex_content = comparison_measures.to_latex(index=False, header=True)

    # Additional LaTeX table customizations can be done here by manipulating latex_content string
    
    # Define file path
    file_path = f"{figfolder}/corr_measures.tex"
    
    # Save LaTeX table to file
    with open(file_path, "w") as latex_file:
        latex_file.write(latex_content)
    return None

def plot_kurtosis(stoxda, cequity_mapper, topic_map, figfolder):
    print("Plotting kurtosis graph...")
    
    # Format date and create 'y' and 'ym' columns
    stoxda['date'] = pd.to_datetime(stoxda['date'])
    stoxda['y'] = stoxda['date'].dt.year
    stoxda['ym'] = stoxda['y']*100 + stoxda['date'].dt.month

    # Perform merges
    stox = pd.merge(stoxda, cequity_mapper, left_on=['PERMNO', 'y'], right_on=['PERMNO', 'y'], how='left')
    stox = stox[stox['crit_ALL'] == 1]
    stox = pd.merge(stox, topic_map, left_on=['PERMNO', 'y'], right_on=['LPERMNO', 'year'], how='left')
    stox_by_kk = (stox.groupby(['ym', 'PERMNO'])
                .agg(moment=('RET', lambda x: kurtosis(x, fisher=False, nan_policy='omit')),
                    group=('max_topic', 'mean'))
                .reset_index())
    stox_by_kk = stox_by_kk.dropna(subset=['group']).copy()
    stox_by_kk['group'] = stox_by_kk['group'].apply(lambda x: int(x) if x - int(x) == 0 else x)
    stox_by_kk['group'] = stox_by_kk['group'].astype('category')

    # Further grouping and averaging by 'group' and 'ym'
    stox_by_kk = (stox_by_kk.groupby(['group', 'ym'])
                    .agg(moment=('moment', 'mean'))
                    .reset_index())
        # Plot using seaborn or matplotlib
    sns.lineplot(data=stox_by_kk, x='ym', y='moment', hue='group')
    plt.title("Kurtosis over time by group")
    plt.xlabel("Year-Month")
    plt.ylabel("Kurtosis")
    plt.legend(title='Group', fontsize='small')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{figfolder}/kurtosis_by_group.jpg", dpi=600)
    plt.clf()
    return None

def plot_returns():
    we_ret_bybin = stoxwe_with_pfs.groupby(['yw', 'ntile_topic_kk']).agg(
        eret=('eretw', lambda x: np.sum(x * me) / np.sum(me)),
        sderet=('eretw', 'std')
    ).reset_index().groupby('ntile_topic_kk').apply(lambda x: x.assign(eret_accum=x['eret'].cumsum())).reset_index(drop=True)
    
    qt_ret_bygroup = we_ret_bybin.groupby('ntile_topic_kk').apply(lambda x: x.assign(eret3ma=x['eret'].rolling(window=13, min_periods=1).mean())).reset_index(drop=True)
    
    plt.figure()
    sns.lineplot(data=we_ret_bybin, x='yw', y='eret', hue='ntile_topic_kk')
    plt.xlabel("Year-month")
    plt.ylabel("Asset-weighted weekly returns")
    plt.legend(fontsize=14)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.savefig(figfolder + "awwr.jpg", dpi=600)
    plt.clf()
    
    plt.figure()
    sns.lineplot(data=qt_ret_bygroup, x='yw', y='eret3ma', hue='ntile_topic_kk')
    plt.xlabel("Year-month")
    plt.ylabel("Asset-weighted weekly returns, 3MA")
    plt.legend(fontsize=14)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.savefig(figfolder + "awwr3ma.jpg", dpi=600)
    plt.clf()
    
    plt.figure()
    sns.lineplot(data=we_ret_bybin, x='yw', y='eret_accum', hue='ntile_topic_kk')
    plt.xlabel("Year-month")
    plt.ylabel("Asset-weighted accumulated weekly returns")
    plt.legend(fontsize=14)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.savefig(figfolder + "awawr.jpg", dpi=600)
    plt.clf()
    
    we_ret_bybin_filt = we_ret_bybin[we_ret_bybin['ntile_topic_kk'].isin([1, 4])].groupby('ntile_topic_kk').apply(
        lambda x: x.assign(moving_average=(x['sderet'].shift() + x['sderet'].shift(2) + x['sderet'].shift(3) + x['sderet']) / 4)
    ).reset_index(drop=True)
    
    plt.figure()
    sns.lineplot(data=we_ret_bybin_filt, x='yw', y='moving_average', hue='ntile_topic_kk')
    plt.xlabel("Year-month")
    plt.ylabel("Weekly standard deviation of returns by n-tile, four-week MA")
    plt.legend(fontsize=14)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.savefig(figfolder + "wsdr.jpg", dpi=600)
    plt.clf()
    
    we_ret_bygroup = stoxwe_with_pfs.groupby(['yw', 'max_topic']).agg(
        eret=('eretw', lambda x: np.sum(x * me) / np.sum(me)),
        sderet=('eretw', 'std')
    ).reset_index().groupby('max_topic').apply(lambda x: x.assign(eret_accum=x['eret'].cumsum())).reset_index(drop=True)
    
    qt_ret_bygroup = we_ret_bygroup.groupby('max_topic').apply(lambda x: x.assign(eret3ma=x['eret'].rolling(window=13, min_periods=1).mean())).reset_index(drop=True)
    
    plt.figure()
    sns.lineplot(data=we_ret_bygroup, x='yw', y='eret', hue='max_topic')
    plt.xlabel("Year-month")
    plt.ylabel("Asset-weighted weekly returns")
    plt.legend(fontsize=14)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.savefig(figfolder + "awwr_byg.jpg", dpi=600)
    plt.clf()
    
    plt.figure()
    sns.lineplot(data=qt_ret_bygroup, x='yw', y='eret3ma', hue='max_topic')
    plt.xlabel("Year-month")
    plt.ylabel("Asset-weighted weekly returns, 3MA")
    plt.legend(fontsize=14)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.savefig(figfolder + "awwr3ma_byg.jpg", dpi=600)
    plt.clf()
    
    plt.figure()
    sns.lineplot(data=we_ret_bygroup, x='yw', y='eret_accum', hue='max_topic')
    plt.xlabel("Year-month")
    plt.ylabel("Asset-weighted accumulated weekly returns")
    plt.legend(fontsize=14)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.savefig(figfolder + "awawr_byg.jpg", dpi=600)
    plt.clf()
    
    plt.figure()
    sns.lineplot(data=we_ret_bygroup, x='yw', y='sderet', hue='max_topic')
    plt.xlabel("Year-month")
    plt.ylabel("(Non-asset-weighted) weekly standard deviation of returns")
    plt.legend(fontsize=14)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.savefig(figfolder + "wsdr_byg.jpg", dpi=600)
    plt.clf()
