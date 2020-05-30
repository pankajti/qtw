import pandas as pd
file_path = r'/Users/pankaj/dev/git/smu/qtw/case_study2/extraction/final_women.csv'

def convert_to_float (str):
    try:
        return float(str)
    except :
        return 0


df = pd.read_csv(file_path)
df['AGE'] =  df.AGE.apply(convert_to_float)
df['AGE'].fillna(0, inplace=True)

df['TOTAL_TIME'] =  df['TIME'].fillna(df['GUN_TIME'])

df.to_csv('processed_female.csv')

print(df)

