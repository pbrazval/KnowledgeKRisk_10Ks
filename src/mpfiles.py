import string
import spacy
import os 
from bs4 import BeautifulSoup
import re, pandas as pd

def process_file(filename):
    with open(filename) as f:
        doc = f.read().splitlines() 
    doc = filter(None, doc) # remove empty string
    doc = '. '.join(doc)
    doc = doc.replace("\\n", " ")
    doc = doc.translate(str.maketrans('','',string.punctuation))
    doc = doc.translate(str.maketrans('','','1234567890'))      
    doc = doc.encode('ascii', 'ignore') # ignore fancy unicode chars
    doc = str(doc)
    return (filename, doc)
    
def lemmatize_text(text):
    if text is None:
        return None
    else:
        text = text.replace("\\n", "  ")
        allowed_postags=["NOUN", "ADJ", "VERB", "ADV"]
        nlp = spacy.load("en_core_web_sm", disable=["parser", "ner"])
        nlp.max_length = 2000000 
        doc = nlp(text)
        new_text = []
        for token in doc:
            if token.pos_ in allowed_postags:
                new_text.append(token.lemma_)
        return " ".join(new_text)

def process_bow(i):
    bow = corpus[i]
    low_value_words = []

    tfidf_ids = [id for id, value in tfidf[bow]]
    bow_ids = [id for id, value in bow]

    low_value_words = [id for id, value in tfidf[bow] if value < low_value]
    drops = low_value_words + words_missing_in_tfidf
    for item in drops:
        words.append(id2word[item])

    words_missing_in_tfidf = [id for id in bow_ids if id not in tfidf_ids]
    new_bow = [b for b in bow if b[0] not in low_value_words and b[0] not in words_missing_in_tfidf]
    if i % 500 == 0:
        print('Parsing text ', i)

    return new_bow


def return1atext(fileaddress):
    file = open(fileaddress, "r")
    raw_10k = file.read()
    doc_start_pattern = re.compile(r'<SEC-DOCUMENT>')
    doc_end_pattern = re.compile(r'</SEC-DOCUMENT>')
    # Regex to find <TYPE> tag prceeding any characters, terminating at new line
    type_pattern = re.compile(r'<TYPE>[^\n]+')

    doc_start_is = [x.end() for x in doc_start_pattern.finditer(raw_10k)]
    doc_end_is = [x.start() for x in doc_end_pattern.finditer(raw_10k)]


    ### Type filter is interesting, it looks for <TYPE> with Not flag as new line, ie terminare there, with + sign
    ### to look for any char afterwards until new line \n. This will give us <TYPE> followed Section Name like '10-K'
    ### Once we have have this, it returns String Array, below line will with find content after <TYPE> ie, '10-K' 
    ### as section names
    doc_types = [x[len('<TYPE>'):] for x in type_pattern.findall(raw_10k)]

    document = {}
    doc_start = doc_start_is[0]
    doc_end = doc_end_is[0]
    document['10-K'] = raw_10k[doc_start:doc_end]
    # Create a loop to go through each section type and save only the 10-K section in the dictionary
    # for doc_type, doc_start, doc_end in zip(doc_types, doc_start_is, doc_end_is):
    #     if doc_type == '10-K':
    #         document[doc_type] = raw_10k[doc_start:doc_end]
    #regex = re.compile(r'>(?im).*Item(\s|&#160;|&nbsp;|\\n)*(1A|1B|7A|7|8)\.{0,1}')
    regex = re.compile(r'>\s*[Ii][Tt][Ee][Mm](\s|&#160;|&nbsp;|\\n)*[12]\s*[AaBb]?\.{0,1}')
    #regex = re.compile(r'>(Item(\s|&#160;|&nbsp;)(1A|1B|7A|7|8)\.{0,1})|(ITEM\s(1A|1B|7A|7|8))')         
    # Use finditer to math the regex
    matches = regex.finditer(document['10-K'])
    test_df = pd.DataFrame([(x.group(), x.start(), x.end()) for x in matches])

    test_df.columns = ['item', 'start', 'end']
    test_df['item'] = test_df.item.str.lower()

    # Display the dataframe

    test_df.replace('&#160;',' ',regex=True,inplace=True)
    test_df.replace('&nbsp;',' ',regex=True,inplace=True)
    test_df.replace('\\\\n',' ',regex=True,inplace=True)
    test_df.replace('\\n',' ',regex=True,inplace=True)
    test_df.replace(' ','',regex=True,inplace=True)
    test_df.replace('\.','',regex=True,inplace=True)
    test_df.replace('>','',regex=True,inplace=True)


    if any(test_df['item'] == 'item1a'):
        pass
        #print("The column 'item' contains the value 'item1a'")
    else:
        raise AssertionError("There's no 1a here.")


    if any(test_df['item'] == 'item1b'):
        after1a = 'item1b'
    elif any(test_df['item'] == 'item2'):
        after1a = 'item2'
    else:
        raise AssertionError("There's an 1a here, but no 1b or 2 here.")

        

    pos_dat = test_df.sort_values('start', ascending=True).drop_duplicates(subset=['item'], keep='last')

    pos_dat.set_index('item', inplace=True)

    # # Get Item 1a
    item_1a_raw = document['10-K'][pos_dat['start'].loc['item1a']:pos_dat['start'].loc[after1a]]

    # #     # Get Item 7
    # #     item_7_raw = document['10-K'][pos_dat['start'].loc['item7']:pos_dat['start'].loc['item7a']]

    # #     # Get Item 7a
    # #     item_7a_raw = document['10-K'][pos_dat['start'].loc['item7a']:pos_dat['start'].loc['item8']]

    # #     item_1a_raw[0:1000]

    item_1a_content = BeautifulSoup(item_1a_raw,  "html.parser")

    text = item_1a_content.get_text()
    return text

def convertto1a(shortfile, mypath, my1apath):
    file = shortfile
    try:
        thistext = return1atext(mypath+file)
        print("Success")
    except Exception as e:
        print(f'Exception in file {file} occurred.')
        print("An error occurred: {}".format(e))
        return None
    else:
        if not os.path.exists(my1apath):
            os.makedirs(my1apath)
        with open(my1apath+file, 'w') as f:
            f.write(thistext)
        return None
    
    
def droplowvwords_chunk(chunk, tfidf, low_value, words_missing_in_tfidf):
        new_corpus = []
        for i in range(len(chunk)):
            bow = chunk[i]
            low_value_words = [] #reinitialize to be safe. You can skip this.

            tfidf_ids = [id for id, value in tfidf[bow]]
            bow_ids = [id for id, value in bow]

            low_value_words = [id for id, value in tfidf[bow] if value < low_value]

            words_missing_in_tfidf = [id for id in bow_ids if id not in tfidf_ids]
            new_bow = [b for b in bow if b[0] not in low_value_words and b[0] not in words_missing_in_tfidf]
            if i % 500 == 0:
                print('Dropping low value words from text ', i)
            new_corpus.append(new_bow)
        return new_corpus