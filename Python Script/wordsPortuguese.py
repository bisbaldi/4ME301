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
    content = session.get('https://' + hostname + '/vocabulary/overview').json()

else: raise Exception("Login failed")

# READ LAST FETCHED DATA TIMESTAMP
with open('../PWordTimestamp.txt', 'r') as timestamp:
    last_time_stamp = int(timestamp.read())

# APPEND CSV FILE
with open('../words.csv', 'a') as lessons:
    word_writer = csv.writer(lessons, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    language = content['language_string']
    max_time = last_time_stamp
    for p in content['vocab_overview']:
        time = p['last_practiced_ms']
        word_string = p['word_string']
        word_id = p['id']
        lexeme_id = p['lexeme_id']
        strength = p['strength']
        strength_bar = p['strength_bars']
        word_type = p['pos']
        gender = p['gender']
        related_lexemes = p['related_lexemes']
        normalized_string = p['normalized_string']
        infinitive = p['infinitive']
        skill = p['skill']
        skill_url = p['skill_url_title']
        if word_type:
            word_type = word_type.encode('utf8')
        if gender:
            gender = gender.encode('utf8')
        if infinitive:
            infinitive = infinitive.encode('utf8')
        if normalized_string:
            normalized_string = normalized_string.encode('utf8')
        if time > last_time_stamp:
            word_writer.writerow([time, language.encode('utf8'), word_string.encode('utf8'), word_id, lexeme_id, strength, strength_bar, word_type,
                                  gender, related_lexemes, normalized_string, infinitive, skill.encode('utf8'), skill_url.encode('utf8')])
            if time > max_time:
                max_time = time

# WRITE TIMESTAMP FOR LAST FETCHED DATA
with open('../PWordTimestamp.txt', 'w') as timestamp:
    timestamp.write(str(max_time))

print("--- Data Updated ---")
