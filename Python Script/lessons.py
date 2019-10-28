# coding=utf-8
import csv
import requests
import json
from requests.auth import HTTPBasicAuth
from getpass import getpass

# With code snippets from Kartik Talwar found at https://github.com/KartikTalwar/duolingo

hostname = 'www.duolingo.com'
session = requests.Session()
headers = {}

# LOGIN
login_url = 'https://' + hostname + '/login'
data = {"login": 'annikab123', "password": getpass()}

req = requests.Request('POST', login_url, json=data, headers=headers, cookies=session.cookies)
request = session.send(req.prepare())
attempt = request.json()

if attempt.get('response') == 'OK':
    jwt = request.headers['jwt']
    content = session.get('https://' + hostname + '/2017-06-30/users/407486877?fields=xpGains').json()

else: raise Exception("Login failed")

# READ LAST FETCHED DATA TIMESTAMP
with open('../LessonTimestamp.txt', 'r') as timestamp:
    last_time_stamp = int(timestamp.read())

# APPEND CSV FILE
with open('../test.csv', 'a') as lessons:
    lesson_writer = csv.writer(lessons, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    for p in content['xpGains']:
        time = p['time']
        xp = p['xp']
        event_type = p['eventType']
        skill = p['skillId']
        if time > last_time_stamp:
            last_time_stamp = time
            lesson_writer.writerow([time, '', xp, event_type, skill, '', ''])

# WRITE TIMESTAMP FOR LAST FETCHED DATA
with open('../LessonTimestamp.txt', 'w') as timestamp:
    timestamp.write(str(last_time_stamp))

print("--- Data Updated ---")