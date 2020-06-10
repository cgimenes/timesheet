import os
import locale
from datetime import datetime

files_dir = 'files/'
files = os.listdir(files_dir)

for filename in files:
    locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

    try:
        parsed = datetime.strptime(filename.split('.')[0], "%B %Y")
    except:
        locale.setlocale(locale.LC_ALL, 'pt_BR.UTF-8')
        parsed = datetime.strptime(filename.split('.')[0], "%B %Y")


    new_filename = parsed.strftime("%Y-%m") + '.html'
    os.rename(files_dir + filename, files_dir + new_filename)