import os
import re
import json
from datetime import datetime
from bs4 import BeautifulSoup

time_pattern = re.compile("^\d{2}:\d{2}$")
date_pattern = re.compile("^\d{2}/\d{2}/\d{4}$")

files_dir = 'import/files/'
files = os.listdir(files_dir)

dates = {}

for filename in files:
    f = open(files_dir + filename, "r")

    soup = BeautifulSoup(f.read(), 'html.parser')
    rows = soup.find('tbody').contents

    for i in range(1, len(rows)):
        date = rows[i].contents[1].text

        if not date_pattern.match(date):
            date = date + '/' + filename.split('-')[0]

        date = datetime.strptime(date, '%d/%m/%Y').strftime('%Y-%m-%d')

        punches = []
        for j in range(2, 10):
            punch = rows[i].contents[j].text
            if time_pattern.match(punch):
                punches.append(punch)
            else:
                if punch != '':
                    print("Date: %s - Punch: %s" % (date, punch))

        if len(punches) != 0:
            dates[date] = {
                "punches": punches
            }

result_file = open("dates.json", "w")
result_file.write(json.dumps(dates))
result_file.close()