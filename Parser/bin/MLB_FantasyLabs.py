import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
import time
from datetime import datetime, timedelta, date

batters = open('C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\All_MLB_Batters_18.txt','ab+')
pitchers = open('C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\All_MLB_Pitchers_18.txt','ab+')

def TextCleanup(text):
	text = text + "');\n"	
	text = text.replace("''", ' ')
	text = text.replace(',', '\',\'')
	text = text.replace('%','')
	text = text.replace('@','')	
	text = text.replace('&', '')
	text = text.replace('$', '')
	return text

for each in range(1,7):
	casp  = 'C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\casperjs.exe C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\casp_MLB.js --date=' + str((date(2018,4,3)  - timedelta(days = each*1)).strftime("%m%d%Y"))
	print casp
	os.system(casp)

	time.sleep(10)
	soup = BeautifulSoup(open("C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\Batters.html"))
	# soup = BeautifulSoup(open("Batters.html"))

	day = ""
	for allDivs in soup.findAll('input',{'class': re.compile(r'.*date-picker.*') }):
		day = allDivs['value']


	totalRows = len(soup.findAll('div',{'class': re.compile(r'.*ag-row-level.*') }))
	AllDivs = soup.findAll('div',{'class': re.compile(r'.*ag-row-level.*') })
	print totalRows, day	
	text= "insert into `mlb`.`batters` Values('" + day + ','
	for i in range(0, (totalRows/2)-1):
		for elements in AllDivs[i].findAll('div'):
			#print elements.text,"\n"
			if len(elements.text)> 0:				
				text = text + elements.text + ","
			else:
				text = text + ' ' + ","
		
		index = 0		
		actualIndex = 0
		for elements in AllDivs[i+(totalRows/2)].findAll('div'):

			elements = (elements.text.replace(',','')).replace('\'','')
			if (actualIndex == 10 and each == 0):
				actualIndex += 1
				text = text +  elements +  ',0' + ","
				continue			
			
			actualIndex += 1
			
			#input()
			if re.match('.*[0-9]+-[0-9]+.*', elements):				
				text = text + (elements).replace('-',',') + ","
				index += 2
				continue

			if re.match('.*[0-9]+\+.*', elements):				
				text = text + (elements).replace('+',',') + ","
				index += 2				
				continue
			
			if index == 9:				
				text = text + ' , ' + ","
				index += 1
				# print text
				# input(1)
				continue

			index += 1

			if len(elements) > 0:
				text = text + elements.replace('\'','') + ","
			else:
				text = text + ' ' + ","
			


		text = TextCleanup(text)				
		batters.write(text)
		text= "insert into `mlb`.`batters` Values('" + day + ','

	os.system('del C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\Batters.html')


	# Pitchers
	time.sleep(10)
	soup = BeautifulSoup(open("C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\pitchers.html"))
	# soup = BeautifulSoup(open("pitchers.html"))

	day = ""
	for allDivs in soup.findAll('input',{'class': re.compile(r'.*date-picker.*') }):
		day = allDivs['value']


	totalRows = len(soup.findAll('div',{'class': re.compile(r'.*ag-row-level.*') }))
	AllDivs = soup.findAll('div',{'class': re.compile(r'.*ag-row-level.*') })
	print totalRows, day
	text= "insert into `mlb`.`pitchers` Values('" + day + ','

	for i in range(0, (totalRows/2)):
		for elements in AllDivs[i].findAll('div'):
			#print elements.text,"\n"
			if len(elements.text )> 0:				
				text = text + elements.text + ","
			else:
				text = text + ' ' + ","
		
		index = 0		
		actualIndex = 0
		for elements in AllDivs[i+(totalRows/2)].findAll('div'):
			#print elements.text,' ', actualIndex,' ', each ,"\n"
			elements = (elements.text.replace(',','')).replace('\'','')
			if (actualIndex == 10 and each == 0):
				actualIndex += 1
				text = text +  elements +  ',0' + ","
				continue			
			
			actualIndex += 1
			
			#input()
			if re.match('.*[0-9]+-[0-9]+.*', elements):				
				text = text + (elements).replace('-',',') + ","
				index += 2
				continue

			if re.match('.*[0-9]+\+.*', elements):				
				text = text + (elements).replace('+',',') + ","
				index += 2				
				continue
			
			index += 1

			if len(elements)> 0:
				text = text + elements.replace('\'','') + ","
			else:
				text = text + ' ' + ","
			

		text = TextCleanup(text)
		# print text
		if (len(text.split(',')) == 85):
			a = text.split(',')
			a.insert(10, "' '")
			text = ','.join(a)

		pitchers.write(text)
		text= "insert into `mlb`.`pitchers` Values('" + day + ','
	os.system('del C:\\Users\\suhas\\Documents\\Sports\\MLB\\Parser\\bin\\Pitchers.html')

pitchers.close()
batters.close()
