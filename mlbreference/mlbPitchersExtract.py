import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
import time
from datetime import datetime, timedelta, date


curlLink = 'curl "https://www.baseball-reference.com/previews/"'
textFile = commands.getoutput(curlLink)
textFile = textFile[50000:]
bs = BeautifulSoup(textFile)

for hovers in bs.findAll('div', {'class' :'game_summary nohover'}):

	tables = hovers.findAll('table')

	combo = []
	if (len(tables) == 2):
		tds = tables[1].findAll('td')
		# print tds
		combo.append(tds[0].text)
		combo.append(tds[1].text.split('(')[0])
		combo.append(tds[2].text)
		combo.append(tds[3].text.split('(')[0])
	if(len(combo) >= 2):
		print 'Today  = rbind(Today ,TodayData("", "' + combo[0] + '", ' + '"'  + combo[2] + '", ' + '"' + combo[3] + '"))' 
		print 'Today  = rbind(Today ,TodayData("", "' + combo[2] + '", ' + '"'  + combo[0] + '", ' + '"' + combo[1] + '"))' 	



for hovers in bs.findAll('div', {'class' :'game_summary nohover'}):

	tables = hovers.findAll('table')

	combo = []
	if (len(tables) == 2):
		tds = tables[1].findAll('td')
		combo.append(tds[0].text)
		combo.append(tds[1].text.split('(')[0])
		combo.append(tds[2].text)
		combo.append(tds[3].text.split('(')[0])
	if(len(combo) >= 2):	
		print 'PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "' + combo[2] + '", ' + '"'  + combo[0] + '", ' + '"' + combo[1] + '"))' 	
		print 'PitchersToday  = rbind(PitchersToday ,TodayPitcher("", "' + combo[0] + '", ' + '"'  + combo[2] + '", ' + '"' + combo[3] + '"))' 

