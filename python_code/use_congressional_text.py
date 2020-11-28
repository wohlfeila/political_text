import tensorflow_hub as hub
import os
import pandas as pd
import numpy as np

embedding_path = "/Volumes/TOSHIBA EXT/gitrepo_data/universal_sentence_encoder_embeddings/"

os.getcwd()
#%%
df = pd.read_csv("~/Documents/gitrepos/politicalblogclassification/use_df.csv", encoding='latin-1')
df.head()
#%%
tmp = df['text'].astype(str).values.tolist()
#%%
module_url = "https://tfhub.dev/google/universal-sentence-encoder/4" 
model = hub.load(module_url)

print ("module %s loaded" % module_url)

def embed(input):
  return model(input)
#%%
text_embeddings = embed(tmp)
#%%
num = str(1)
np.savetxt(embedding_path+"embedding_"+num+".csv", text_embeddings, delimiter=",")
#%%
df.head()