import requests
from bs4 import BeautifulSoup
import pandas as pd
import urllib3
import os
import urllib
import re
ubase = "http://www.cherryblossom.org/"

#### From text
menURLs = [
    "results/1999/cb99m.html",
    "results/2000/cb003m.htm",
    "results/2001/oof_m.html",
    "results/2002/oofm.htm",
    "results/2003/CB03-M.HTM",
    "results/2004/men.htm",
    "results/2005/CB05-M.htm",
    "results/2006/men.htm",
    "results/2007/men.htm",
    "results/2008/men.htm",
    "results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm",
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm"]


womenURLs = [
    "results/1999/cb99f.html",
    "results/2000/cb003f.htm",
    "results/2001/oof_f.html",
    "results/2002/ooff.htm",
    "results/2003/CB03-F.HTM",
    "results/2004/women.htm",
    "results/2005/CB05-F.htm",
    "results/2006/women.htm",
    "results/2007/women.htm",
    "results/2008/women.htm",
    "results/2009/09cucb-F.htm",
    "results/2010/2010cucb10m-f.htm",
    "results/2011/2011cucb10m-f.htm",
    "results/2012/2012cucb10m-f.htm"]


def scrap_runs(urls, gender):
    for rel_url in urls:
        url = urllib.parse.urljoin(ubase, rel_url)
        year = re.search('/(\d{4})', url)[0][1:]
        print(url)
        response = requests.get(url)
        soup = BeautifulSoup(response.content)
        if year == '2000':
            result = soup.find(face="Courier New").get_text()
        elif year == '2009' and gender == 'men':
            result = soup.find(class_="Section1").get_text()
        else:
            result = soup.find('pre').get_text()
        with open(os.path.join(r'/Users/pankaj/dev/git/smu/qtw/case_study2/data', '{}_{}.txt'.format(gender, year)),
                  'w+') as f:
            f.writelines(result)

        print(result[:100])


#scrap_runs(menURLs, 'men')

scrap_runs(womenURLs, 'women')