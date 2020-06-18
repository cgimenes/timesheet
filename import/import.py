import requests
import csv


with open('import/dates.csv', newline='') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        date = row['Date']

        for i in range(0, 8):
            colname = 'Punch%d' % i
            if row[colname] != '':
                url = "http://localhost:8080/timesheet"

                payload = 'datetime=%sT%s' % (date, row[colname])
                headers = {
                    'Content-Type': 'application/x-www-form-urlencoded'
                }

                response = requests.request("POST", url, headers=headers, data = payload)
