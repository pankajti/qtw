file_path = r'/Users/pankaj/dev/git/smu/qtw/case_study2/data/men_2009_raw.txt'


def clean_men_2009_data():
    with open(file_path, 'r') as f:
        lines = f.readlines()
    start = lines[0].upper().index('PLACE')
    step = lines[0].index('=') - start
    i = 0
    with open('men_2009.txt', 'w+') as f:

        while True:
            i = i + 1
            if i * step > len(lines[0]):
                break
            else:
                end = start + step
                str = lines[0][start: end]
                start = end
                f.write(str + "\n")


clean_men_2009_data()
