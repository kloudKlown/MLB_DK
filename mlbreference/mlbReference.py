import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
import time
from datetime import datetime, timedelta, date


teams = [
'/teams/ARI',
'/teams/ATL',
'/teams/BAL',
'/teams/BOS',
'/teams/LAD',
'/teams/CHC',
'/teams/CHW',
'/teams/CIN',
'/teams/CLE',
'/teams/COL',
'/teams/DET',
'/teams/HOU',
'/teams/OAK',
'/teams/LAA',
'/teams/MIA',
'/teams/KCR',
'/teams/MIL',
'/teams/MIN',
'/teams/WSN',
'/teams/SFG',
'/teams/NYM',
'/teams/NYY',
'/teams/PHI',
'/teams/PIT',
'/teams/SDP',
'/teams/SEA',
'/teams/STL',
'/teams/TBR',
'/teams/TEX',
'/teams/TOR']

os.system('rm "C:\\Users\\suhas\\Documents\\Sports\\MLB\\mlbreference\\All_MLB_Batters_18.csv"')
os.system('rm "C:\\Users\\suhas\\Documents\\Sports\\MLB\\mlbreference\\All_MLB_Pitchers_18.csv"')

batters = open('C:\\Users\\suhas\\Documents\\Sports\\MLB\\mlbreference\\All_MLB_Batters_18.csv','ab+')
pitchers = open('C:\\Users\\suhas\\Documents\\Sports\\MLB\\mlbreference\\All_MLB_Pitchers_18.csv','ab+')



def CleanInput(param, bool):
	if param == '@':
		return '1'

	if bool:		
		param = re.sub('[^A-Za-z0-9.@ ]+', '', param)
	else:
		param = re.sub('[^A-Za-z0-9.@]+', '', param)
	
	return param


def GetPlayer(params):
	position = 'b'
	if (len(params)) > 0:
		if (params[0] == 'SP'):
			position = 'sp'
		if (params[0] == 'RP'):
			position = 'rp'
		if (params[0] == 'CL'):
			position = 'cl'
		if (params[0] == ''):
			position = 'rp'
			params[0] = 'rrp'
	else:
		return
	print position[-1]

	playerURL = 'curl "https://www.baseball-reference.com/players/gl.fcgi?id=' + (params[2].split('/')[-1]).split('.')[0] + '&t=' + position[-1] +'&year=2018"'
	# playerURL = 'curl "https://www.baseball-reference.com/players/gl.fcgi?id=corbipa01&t=p&year=2018"'
	textFile = commands.getoutput(playerURL)
	textFile = textFile[50000:]
	bs = BeautifulSoup(textFile)
	bats = ""
	throws = ""
	if 'Bats' in bs.text:

		bats = bs.text.split('Bats')[1][1:2]
	# print bs.text
	
	if 'throws' in bs.text:
		throws = bs.text.split('throws')[1][1:2]
	# print bs.text

	# input()	

	if (position == 'b'):
		position = 'batting_gamelogs'	
	else:
		position = 'pitching_gamelogs'

	table = bs.find('table', id=position)
	


	if table is None:
			return
	
	name = params[1]
	name = re.split('[0-9]', name)[0]

	for trs in table.findAll('tr'):
		if (trs) is None:
			break
		writeToFile = name + ',' + bats + ',' + throws +','+ params[0] + ','
		dateFound = False

		for tds in trs.findAll('td'):			
			if len(tds.text) > 0:
				if 'href="/boxes' in (str(tds)):
					dateFound = True
					if '(' in str(tds.text):
						temp = tds.text.split('(')[0]
						writeToFile = writeToFile + temp.replace('&nbsp;', ' ') + ' 2018,'
					else:					
						writeToFile = writeToFile + tds.text.replace('&nbsp;', ' ') + ' 2018,'
				else:
					if (len(tds.text) > 10):
						writeToFile = writeToFile + '0,'
					else:
						writeToFile = writeToFile + CleanInput(tds.text, False) + ','
			else:
				writeToFile = writeToFile + 'NULL' + ','

		if (len(writeToFile) < 30) or not dateFound:
			continue
		writeToFile = writeToFile + '\n'
		if position == 'batting_gamelogs':
			batters.write(writeToFile)
		else:
			pitchers.write(writeToFile)
	return


### Get URL of team and extract each player info 1 by 1
def TeamURL(teamName):
	print teamName
	teamURL = 'curl https://www.baseball-reference.com' + teamName + '/2018.shtml'
	textFile = commands.getoutput(teamURL)
	textFile = textFile[100000:]
	bs = BeautifulSoup(textFile)
	battersTable = bs.find('table', id='team_batting')

	for trs in battersTable.findAll('tr'):
		beakat3 = 1
		params = []
		for tds in trs.findAll('td'):
			# If name don't remove spaces		
			if (len(params) == 1):
				params.append(CleanInput(tds.text, True))						
			else:
				params.append(CleanInput(tds.text, False))

			if 'href' in str(tds):
				params.append(tds.find('a').get('href'))
			beakat3 += 1
			if beakat3 == 4:
				print params				
				if (len(params) == 4) and params[0] != 'P':
					GetPlayer(params)
				break


	pitcherTable = bs.find('table', id = "team_pitching")
	for trs in pitcherTable.findAll('tr'):
		beakat3 = 1
		params = []
		# print trs
		for tds in trs.findAll('td'):
			# If name don't remove spaces		
			if (len(params) == 1):
				params.append(CleanInput(tds.text, True))						
			else:
				params.append(CleanInput(tds.text, False))

			if 'href' in str(tds):
				params.append(tds.find('a').get('href'))
			beakat3 += 1
			if beakat3 == 4:
				print params				
				if (len(params) == 4):
					GetPlayer(params)
				break


for team in teams:
	TeamURL(team)
	
batters.close()	
pitchers.close()