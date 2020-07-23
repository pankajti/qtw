import gzip
import pandas as pd
from io import StringIO

columns =  ["label", "epton pT", "lepton eta", "lepton phi", "missing energy magnitude", "missing energy phi", "jet 1 pt", "jet 1 eta", "jet 1 phi", "jet 1 b-tag", "jet 2 pt", "jet 2 eta", "jet 2 phi", "jet 2 b-tag", "jet 3 pt", "jet 3 eta", "jet 3 phi", "jet 3 b-tag", "jet 4 pt", "jet 4 eta", "jet 4 phi", "jet 4 b-tag", "m_jj", "m_jjj", "m_lv", "m_jlv", "m_bb", "m_wbb", "m_wwbb"]
with gzip.open('/Users/pankaj/dev/data/HIGGS.csv.gz', 'rt') as f:
    for line in f:
        print(line)
        df = pd.read_csv(StringIO(line), names=columns)
        print(df)
        break
