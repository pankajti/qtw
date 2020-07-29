import tensorflow_datasets as tfds
import pandas as pd

higgs_data = tfds.load("higgs")
higgs_train = higgs_data['train']
h_ds = higgs_train.take(2)

for hd in h_ds :
    print(hd['class_label'].numpy())
    print(hd['jet_1_b-tag'].numpy())
    pd.DataFrame()