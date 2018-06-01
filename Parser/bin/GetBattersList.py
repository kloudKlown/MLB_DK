import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
from datetime import datetime, timedelta, date

os.system('rm 2017_BattersList.csv')
batters = open('2017_BattersList.csv','ab+')

battersList = []

with open('SavantBatters.txt', 'r') as csvFile:
	soup = BeautifulSoup(csvFile)
	findallImg = soup.findAll('img', {'class': 'player-mug'})
	player = "http://m.mlb.com/player/"

	for players in findallImg:
		print players

		# Get last element
		if "60x60" in str(players):
			playerID = str(players).split("60x60/")[-1]

			# Make sure the element has playerID
			if re.match('[0-9]+.*', playerID):
				playerID = playerID.split('.')[0]
				player =  player + str(playerID) + '/'
				print playerID
				# input()

			players = players.findNext('span')			
			if "span" in str(players):
				players = str(players).split('</span>')[0]
				players = str(players).split('<span>')[-1]
				players =  players.replace(',', '')
				players =  players.replace('.', '')
				players =  players.replace('\'', '')
				playersName = players.split(' ')

				if len(playersName) == 3:
					player = player +  playersName[1] + '-' + playersName[-1] + '-' + playersName[0] + '\n'
					battersList.append(player)
					# batters.write(player)
			# player  =  player  + str(players)
				if len(playersName) == 2:
					player = player +  playersName[-1] + '-' + playersName[0] + '\n'
					# batters.write(player)
					battersList.append(player)
				
		player = "http://m.mlb.com/player/"		
battersList = set(battersList)		
for item in battersList:
  batters.write(item)

# batters.write(battersList)
