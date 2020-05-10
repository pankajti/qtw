import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt
with open ('../data/offline.final.trace.txt') as f :
    txt = f.readlines()

names  = ["time", "scanMac", "posX", "posY", "posZ",
"orientation", "mac", "signal",
"channel", "type"]

class Rec :
    def __init__(self,t, id,pos,orientation, mac,signal,channel,type):
        self.time = t
        self.scanMac = id
        self.posX = pos[0]
        self.posY = pos[1]
        self.posZ = pos[2]
        self.orientation = orientation
        self.mac = mac
        self.signal = signal
        self.channel = channel
        self.type = type

uncommented_texts = [x for x in txt if x[0]!="#"]
recList = []
totalRec= len(uncommented_texts)
for idx, ut in enumerate(uncommented_texts):
    print("processed {} of {}".format(idx, totalRec))
    rec  = ut.split(";")
    #for rec in records :
    t = rec[0].split("=")[1]
    id = rec[1].split("=")[1]
    pos = rec[2].split("=")[1].split(",")
    degree = rec[3].split("=")[1]
    for macs in rec[4:]:
        m= macs.split("=")
        mac  = m[0]
        r= m[1].split(",")
        signal=r[0]
        channel = r[1]
        type= r[2]
        r = Rec(t,id,pos,degree,mac,signal,channel,type)
        recList.append(r)

df = pd.DataFrame([x.__dict__ for x in recList])

## filter adhoc data points

df1 = df[df ['type'].str.strip()=='3']
from collections import Counter
import math
df1['time'] = df1['time'].apply(lambda x: dt.datetime.utcfromtimestamp(int(x.strip())/1000))
df1['orientation'] = pd.cut(df['orientation'].astype(float),8).apply(lambda x: math.ceil((x.left)))
l = len(set(df['orientation']))
df1 = df1[df1['mac']!='00:e0:63:82:8b:a9']

c = Counter(df1[['posX', 'posY']].apply(lambda x : x['posX']+x['posY'], axis =1 ))
x= list(map(lambda x: float(x[0:2]), (c.keys())))
y = list(map(lambda x: float(x[2:4]), (c.keys())))
for k,v  in c.items():
    plt.text(float(k[0:2]), float(k[2:4]), v)
plt.tight_layout(0.5)
plt.show()

fig, ax = plt.subplots()
ax.set_xticks(x)
ax.set_yticks(y)
# ... and label them with the respective list entries
ax.set_xticklabels(x)
ax.set_yticklabels(y)

# Rotate the tick labels and set their alignment.
plt.setp(ax.get_xticklabels(), rotation=45, ha="right",
         rotation_mode="anchor")

# Loop over data dimensions and create text annotations.
for i in x:
    for j in y:
        text = ax.text(j, i, c[str(x)+str(y)],
                       ha="center", va="center", color="w")

ax.set_title("fff")
fig.tight_layout()
plt.show()



df1.groupby('mac').count()

print(df)