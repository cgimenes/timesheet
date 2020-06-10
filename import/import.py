import os
import re
import csv
from datetime import datetime

time_pattern = re.compile("^\d{2}:\d{2}$")
date_pattern = re.compile("^\d{2}/\d{2}/\d{4}$")

files_dir = 'import/files/'
files = os.listdir(files_dir)

dates = []

for filename in files:
    f = open(files_dir + filename, "r")

    soup = BeautifulSoup(f.read(), 'html.parser')
    rows = soup.find('tbody').contents

    for i in range(1, len(rows)):
        date = rows[i].contents[1].text

        if not date_pattern.match(date):
            date = date + '/' + filename.split('-')[0]

        parsed_date = datetime.strptime(date, '%d/%m/%Y')
        
        date = parsed_date.strftime('%Y-%m-%d')

        punches = []
        for j in range(2, 10):
            punch = rows[i].contents[j].text
            if time_pattern.match(punch):
                punches.append(punch)
            else:
                if punch != '':
                    print("Date: %s - Punch: %s" % (date, punch))

        date_obj = {
            "date": date
        }

        for k in range(0, 8):
            if (k < len(punches)):
                date_obj["punch"+str(k)] = punches[k]
            else:
                date_obj["punch"+str(k)] = ''

        dates.append(date_obj)


with open('dates.csv', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['Date', 'Punch0', 'Punch1', 'Punch2', 'Punch3', 'Punch4', 'Punch5', 'Punch6', 'Punch7'])
    for date in dates:
        writer.writerow([date["date"], date["punch0"], date["punch1"], date["punch2"], date["punch3"], date["punch4"], date["punch5"], date["punch6"], date["punch7"]])
