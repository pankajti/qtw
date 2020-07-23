from tensorflow_datasets.structured import higgs


data = higgs.Higgs()


for d in data:
    print(d)