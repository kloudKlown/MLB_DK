import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
from datetime import datetime, timedelta, date

#C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\
os.system('rm BBSavantPitchers_Chart_2017.csv')

batters = open('BBSavantPitchers_Chart_2017.csv','ab+')

def ParseFile(insertList,ID,page):

	if page == None:
		return insertList
	
	def cleanElement(element):				
		element = element.replace('&nbsp;', '')
		element = element.replace('%', '')		
		return element

	fileInsert = ""
	for t in page.findAll('text'):
		text =  cleanElement(t.text)
		if (text) == '':
			fileInsert = fileInsert + ' '
		fileInsert = fileInsert + ',' + text 
	# print fileInsert
	insertList = insertList + fileInsert
	# print insertList
	# input()

	return insertList
insertList = ''
i = 0

with open('2017_PitchersList.csv', 'r') as csvFile:
	for line in csvFile:		
		#insertList = '\'' + '2017-03-01'+ '\', \'' + line[:-1] + '\', '  + '\', '
		casp  = './casperjs.exe baseballSavant_batters.js "' + line[:-1]	 + '?year=2017&stats=career-r-pitching-mlb"'
		os.system(casp)
		# print insertList
		

		BSBatters = open('BBSavantBatters.html','r+')
		soup = BeautifulSoup(BSBatters)
		name = soup.find('div', {'class': 'player-vitals'})
		name = soup.find('span', {'class': 'player-name'})
		# print name.text
		#insertList = name.text
		# input()
		firstDiv = soup.find('div',id='charts__zone-container1')
		insertList = ParseFile(insertList,line,firstDiv)
		
		firstDiv = soup.find('div',id='charts__zone-container2')
		insertList = ParseFile(insertList,line,firstDiv)

		firstDiv = soup.find('div',id='charts__zone-container3')
		insertList = ParseFile(insertList,line,firstDiv)
		
		firstDiv = soup.find('div',id='charts__zone-container4')
		insertList = ParseFile(insertList,line,firstDiv)
		

		
		insertList = insertList + ',\n'
		print insertList
		# input()
		if (len(insertList.split(',')) > 50):
			batters.write(insertList)		
		BSBatters.close()
		insertList = ''

csvFile.close()
batters.close()
