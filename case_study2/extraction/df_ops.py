import re
import pandas as pd
import os
data_home = r'/Users/pankaj/dev/git/smu/qtw/case_study2/data'
from io import StringIO

columns_dict = {'AG': 'AGE',
'Ag':'AGE',

'GUN': 'GUN_TIME',
'GUN TIM':'GUN_TIME',
'Gun':'GUN_TIME',
'Gun Tim':'GUN_TIME',

'HOMETOWN': 'HOMETOWN',
'Hometown':'HOMETOWN',

'NAME': 'NAME',
'Name': 'NAME',

'NET':'TIME',
'NET TIM':'TIME',
'Net':'TIME',
'Net Tim':'TIME',
'Net Tim  ':'TIME',
'TIME':'TIME',
'Time':'TIME'}


def create_df(gender):
    all_columns = set()
    all_dfs = {}
    for year in range(1999, 2013):
        with open(os.path.join(data_home, '{}_{}.txt'.format(gender, year)), 'r') as f:
            lines = f.readlines()
            for idx, line in enumerate(lines):
                if line.startswith('='):
                    print(year, line)
                    # split =- line to determine width of data
                    w = [len(s) + 1 for s in line.split(" ")]
                    df = pd.read_fwf(StringIO("\n".join(lines[(idx - 1):])), widths=w)
                    df.dropna(axis=0, how='all', inplace=True)
                    all_dfs[year] = df
                    all_columns.update(df.columns)
                    if gender =='women' and year ==2011:
                        df.drop('Time', axis=1, inplace=True)
                    print(df.columns)
    for col in sorted(all_columns):
        print(col)
    cleaned_df = []
    for key in all_dfs.keys():
        df = all_dfs[key]
        adf = df.rename(dict(zip(df.columns, [x.strip() for x in df.columns])), axis=1)
        cols = {x: columns_dict[x] for x in adf.columns if x in columns_dict}
        adf = adf.rename(cols, axis=1)
        adf = adf[~adf['NAME'].str.startswith('==')]
        adf['YEAR'] = int(key)
        cleaned_df.append(adf)
    final_df = pd.concat(cleaned_df)[['YEAR', 'NAME', 'AGE', 'TIME', 'GUN_TIME', 'HOMETOWN']]
    final_df.to_csv('final_{}.csv'.format(gender))
    print(final_df)

gender = 'women'
create_df(gender)
