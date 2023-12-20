import csv, gensim, glob, importlib, json, mpfiles, os, pickle, pyLDAvis, re, spacy, string, sys, utilities, warnings
import numpy as np, pandas as pd, gensim.corpora as corpora
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel, LdaModel, TfidfModel
from gensim.test.utils import datapath
from nltk.corpus import stopwords
import pyLDAvis.gensim_models
from gensim.test.utils import datapath
import matplotlib.pyplot as plt
import multiprocessing as mp
from functools import partial
from gensim.corpora import MmCorpus
from gensim.models.coherencemodel import CoherenceModel
import pickle
import numpy as np
import pickle


def gen_words(texts):
    final = []
    for text in texts:
        new = gensim.utils.simple_preprocess(text, deacc=True)
        final.append(new)
    return (final)

def gen_corpus(id2word, data_words):
    corpus = []
    i = 1
    for text in data_words:
        new = id2word.doc2bow(text)
        corpus.append(new)
        i = i+1
        if i % 500 == 0:
            print('Text entered corpus:', i)
    return (corpus)

def create_crosswalks(filename_list, yr):
    print(f"Creating cross walks for year {yr}")
    fn_list = [fn.split('/')[-1] for fn in filename_list]
    idx_list = list(range(len(filename_list)))
    fn2idx = pd.DataFrame({"idx": idx_list, "filename": fn_list})
    fn2cp = []
    for qtr in [1,2,3,4]:
        fn2cp.append(pd.read_csv(f'/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/firmdict/{yr}Q{qtr}.csv'))
    fn2cp = pd.concat(fn2cp)
    merged_df = pd.merge(fn2idx, fn2cp, on="filename", how="outer")
    merged_df.to_csv(f'/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/cp2idx/{yr}.csv')
    return None    

def nwords_cdf(texts):
    len_vec = [len(text) for text in texts]
    sorted_len_vec = np.sort(len_vec)
    y = np.arange(1, len(sorted_len_vec) + 1) / len(sorted_len_vec)

    # Plot with xlim between 0 and 1000
    plt.plot(sorted_len_vec, y)
    plt.xlabel('Number of Words')
    plt.ylabel('Cumulative Probability')
    plt.title('Cumulative Distribution of Number of Words in each 1A for 2022, detail')
    plt.xlim(0, 1000)
    plt.grid(True)
    plt.savefig('/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/text/cdf_words_zoom.png')
    plt.clf()  # Clear the plot

    # Plot without restriction for xlim
    plt.plot(sorted_len_vec, y)
    plt.xlabel('Number of Words')
    plt.ylabel('Cumulative Probability')
    plt.title('Cumulative Distribution of Number of Words in each 1A for 2022')
    plt.grid(True)
    plt.savefig('/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/text/cdf_words.png')
    plt.clf()  # Clear the plot
    return None

def remove_stopwords(texts):
    return [[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]

def make_multigrams(lemmatized_texts, min_count, threshold, scoring):
    print("Generating words using gensim.utils.simple_preprocess...")
    data_words = gen_words(lemmatized_texts)
    print("Creating bigram phrases...")
    bigram_phrases = gensim.models.Phrases(data_words, min_count=min_count, threshold=threshold, scoring = scoring,  connector_words=gensim.models.phrases.ENGLISH_CONNECTOR_WORDS) # higher threshold fewer phrases.
    print("Creating trigram phrases...")
    trigram_phrases = gensim.models.Phrases(bigram_phrases[data_words], min_count=min_count, threshold=threshold, scoring = scoring, connector_words=gensim.models.phrases.ENGLISH_CONNECTOR_WORDS)  

    # Faster way to get a sentence clubbed as a trigram/bigram
    bigram = gensim.models.phrases.Phraser(bigram_phrases)
    trigram = gensim.models.phrases.Phraser(trigram_phrases)
    
    print("Making bigrams and trigrams...")
    data_bigrams = [bigram[doc] for doc in data_words]
    data_bigrams_trigrams = [trigram[bigram[doc]] for doc in data_bigrams]

    print('Bigrams and Trigrams created')
    
    return data_bigrams_trigrams

def make_id2word(data_bigrams_trigrams, pathname, no_below, no_above, keep_n):
    id2word = corpora.Dictionary(data_bigrams_trigrams)
    id2word.filter_extremes(no_below=no_below, no_above=no_above, keep_n=keep_n)
    id2word.save_as_text(pathname)
    return id2word

def bow_texts(texts, id2word):
    corpus = [id2word.doc2bow(text) for text in texts]
    tfidf = TfidfModel(corpus, id2word = id2word)
    print('Corpus and Tfidf created')
    return corpus, tfidf



def make_topicmap2(lda_model, topics_per_doc, yr, ciks_to_keep, modelname):
    k = lda_model.num_topics
    cik_list = ciks_to_keep.values
    topic_probs = {f"topic_{i}": [] for i in range(k)}
    max_topic = [max(doc, key = lambda x:x[1])[0] for doc in topics_per_doc]
    # Iterate over each list of tuples in the topic list
    for doc in topics_per_doc:
        # Initialize a dictionary to hold the probabilities for this topic
        topic_dict = {f"topic_{i}": 0.0 for i in range(k)}
        for tup in doc:
            topic_dict[f"topic_{tup[0]}"] = tup[1]
        # Append the topic probabilities to the overall topic_probs dictionary
        for i in range(k):
            topic_probs[f"topic_{i}"].append(topic_dict[f"topic_{i}"])

    # Create a Pandas DataFrame using the topic_probs dictionary and the cik_list
    df = pd.DataFrame.from_dict(topic_probs)
    df['max_topic'] = max_topic
    df['CIK'] = cik_list
    df['year'] = yr
    
    if not os.path.exists(f"/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/{modelname}"):
        os.makedirs(f"/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/{modelname}")
    if isinstance(yr, list):
        df.to_csv(f"/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/{modelname}/topic_map_{min(yr)}_{max(yr)}.csv", index=False)
    else:
        df.to_csv(f"/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/{modelname}/topic_map_{yr}.csv", index=False)
    
    return df
    
    
def concatenate_topic_maps(myfolder):
    topic_map_files = [f for f in os.listdir(myfolder) if f.startswith('topic_map') and f.endswith('.csv')]
    dfs = [pd.read_csv(os.path.join(myfolder, f)) for f in topic_map_files]
    concatenated_df = pd.concat(dfs)
    return concatenated_df    
# import importlib
# importlib.reload(mpfiles)
    
def filter_corpus(texts, filename_list, cequity_mapper, yr, min10kwords):
    text_length = [len(text) for text in texts]
    cik = [int(re.search(r'/(\d+)_', fn).group(1)) for fn in filename_list]
    if yr <= 2020:
        cequity_mapper = cequity_mapper[cequity_mapper['year'] == yr]
    else:
        cequity_mapper = cequity_mapper[cequity_mapper['year'] == 2020]        
    order_in_cik = list(range(len(cik)))
    stats_texts = pd.DataFrame({"order_in_cik": order_in_cik, "cik": cik, "text_length": text_length})
    fullfilter = pd.merge(stats_texts, cequity_mapper, on="cik", how="inner")
    fullfilter['crit_LEN'] = fullfilter['text_length'] > min10kwords
    fullfilter['crit_ALL'] = fullfilter['crit_ALL'] == 1
    fullfilter['crit_ALL2'] = list(np.logical_and(np.array(fullfilter['crit_ALL']),np.array(fullfilter['crit_LEN'])))
    selection = fullfilter[fullfilter['crit_ALL2']]
    selection = selection.drop_duplicates(subset = "cik", keep = "first")
    idxs_to_keep = selection['order_in_cik']
    ciks_to_keep = selection['cik']    
    
    return selection, idxs_to_keep, ciks_to_keep

def print_coherence(lda_model, corpus, num_topics):
    cm = CoherenceModel(model=lda_model, corpus=corpus, coherence='u_mass')
    coherence = cm.get_coherence()
    print(f"Number of topics: {num_topics}. Coherence: {coherence}")
    return None


def lemmat_counts():
    years = list(range(2006, 2023))
    vec = []
    for yr in range(2006,2023):
        file_path = f"/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/lemmatized_texts/{yr}/lemmatized_texts{yr}.pkl"
        with open(file_path, 'rb') as f:
            lemmatized_texts = pickle.load(f)
        vec.append(len(lemmatized_texts))
    df = pd.DataFrame({'Year': years, 'Count': vec})
    # Save the dataframe to CSV
    df.to_csv('/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/descriptive/lemmat_counts.csv', index=False)
    return None

def count_lemmatized_texts():
    vec = []
    for yr in range(2006,2023):
        file_path = f"/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/lemmatized_texts/{yr}/lemmatized_texts{yr}.pkl"
        with open(file_path, 'rb') as f:
            lemmatized_texts = pickle.load(f)
            vec.append(len(lemmatized_texts))
    return vec

import matplotlib.pyplot as plt

def plot_word_distribution(yr):
    file_path = f"/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/lemmatized_texts/{yr}/lemmatized_texts{yr}.pkl"
    with open(file_path, 'rb') as f:
        texts = pickle.load(f)

    len_vec = [len(text) for text in texts]
    sorted_len_vec = np.sort(len_vec)
    y = np.arange(1, len(sorted_len_vec) + 1) / len(sorted_len_vec)

    plt.plot(sorted_len_vec, y)
    plt.xlabel('Number of Words')
    plt.ylabel('Cumulative Probability')
    plt.title(f'Cumulative Distribution of Number of Words in each 1A ({yr})')
    plt.xlim(0,1000)
    plt.grid(True)
    plt.show()
