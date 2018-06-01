import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
from datetime import datetime, timedelta, date


batters = open('C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\BBSavantParsed_2018v2.txt','ab+')

def ParseFile(insertList,ID,page):

	def cleanElement(element):				
		element = element.replace('&nbsp;', '')
		element = element.replace('%', '')		
		return element

	fileInsert = ""
	for t in page.findAll('text'):
		text =  cleanElement(t.text)
		if (text) == '':
			fileInsert = fileInsert + ' '
		fileInsert = fileInsert + '\'' + text + '\','
	# print fileInsert
	insertList = insertList + fileInsert

	return insertList


insertList = '('
i = 0

with open('C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\BaseballSavant_Batters_2017.csv', 'r') as csvFile:
	for line in csvFile:		
		line = line.split(',')		
		insertList = insertList + '\'' + str(datetime.now().strftime("%Y-%m-%d"))+ '\', \'' + line[2] + '\', '
		casp  = 'C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\casperjs.exe C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\baseballSavant_batters.js "' + line[3] + '" ' +  line[4][:-1]
		os.system(casp)
		# input()

		BSBatters = open('C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\BBSavantBatters.html','r+')
		soup = BeautifulSoup(BSBatters)

		firstDiv = soup.find('div',id='zone_chart_pitches')
		insertList = ParseFile(insertList,line[2],firstDiv)
		
		firstDiv = soup.find('div',id='zone_chart_pitch_percent')
		insertList = ParseFile(insertList,line[2],firstDiv)

		firstDiv = soup.find('div',id='zone_chart_ba')
		insertList = ParseFile(insertList,line[2],firstDiv)
		
		firstDiv = soup.find('div',id='zone_chart_exit_velocity')
		insertList = ParseFile(insertList,line[2],firstDiv)
		

		
		insertList = insertList[:-1] + '),\n'
		print insertList
		batters.write(insertList)
		insertList = '(' 
		BSBatters.close()

csvFile.close()
batters.close()
