import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
os.system('rm Batters_182.txt')
batters = open('Batters_182.txt','ab+')

## Batters (`RK`,`Player`,`Team`,`blank`,`PlayerID`,`Pos`,`G`,`AB`,`R`,`H`,`2B`,`3B`,`HR`,`RBI`,`BB`,`SO`,`SB`,`CS`,`AVG`,`OBP`,`SLG`,`OPS`,`IBB`,`HBP`,`SAC`,`SF`,`TB`,`XBH`,`GDP`,`GO`,`AO`,`GO_AO`,`NP`,`PA`)
## Pitchers (`RK`,`Player`,`Team`,``,`deltas`,`Wdeltas`,`Ldeltas`,`ERAdeltas`,`Gdeltas`,`GSdeltas`,`SVdeltas`,`SVOdeltas`,`IPdeltas`,`Hdeltas`,`Rdeltas`,`ERdeltas`,`HRdeltas`,`BBdeltas`,`SOdeltas`,`AVGdeltas`,`WHIPdeltas`,`CGdeltas`,`SHOdeltas`,`HBdeltas`,`IBBdeltas`,`GFdeltas`,`HLDdeltas`,`GIDPdeltas`,`GOdeltas`,`AOdeltas`,`WPdeltas`,`BKdeltas`,`SBdeltas`,`CSdeltas`,`PKdeltas`,`TBFdeltas`,`NPdeltas`,`WPCTdeltas`,`GO_AOdeltas`,`OBPdeltas`,`SLGdeltas`,`OPSdeltas`,`K_9deltas`,`BB_9deltas`,`H_9deltas`,`K_BBdeltas`,`P_IP`)

sqlTableInsert = 'insert into `mlb`.`OriginalList` (`RK`,`Player`,`Team`,`blank`,`PlayerID`,`Pos`,`G`,`AB`,`R`,`H`,`2B`,`3B`,`HR`,`RBI`,`BB`,`SO`,`SB`,`CS`,`AVG`,`OBP`,`SLG`,`OPS`,`IBB`,`HBP`,`SAC`,`SF`,`TB`,`XBH`,`GDP`,`GO`,`AO`,`GO_AO`,`NP`,`PA`) Values '
insertList = '( \''
def ParseFile(page):
	soup = BeautifulSoup(page)
	fileInsert = ""
	def cleanElement(element):		
		element = element.replace('&nbsp;', '')
		element = element.replace('*', '0')		
		return element


	for trs in soup.findAll('tr'):
		element = insertList
		for tds in trs.findAll('td'):						
			element = element + cleanElement(tds.text) + '\', \''			
		if (len(element)>30):
			fileInsert = fileInsert + element[:-3] + '),\n'
	pass
	fileInsert = sqlTableInsert + fileInsert[:-2] +';'	
	batters.write(fileInsert)


FirstPage = open('Pitcher1.txt','r+')
SecondPage = open('Pitcher2.txt','r+')
ThirdPage = open('Pitcher3.txt','r+')
FourthPage = open('Pitcher4.txt','r+')
FifthPage = open('Pitcher5.txt','r+')
SixthPage = open('Pitcher6.txt','r+')
SeventhPage = open('Pitcher7.txt','r+')
EighthPage = open('Pitcher8.txt','r+')
NinthPage = open('Pitcher9.txt','r+')
a10page = open('Pitcher10.txt','r+')
a11page = open('Pitcher11.txt','r+')
a12page = open('Pitcher12.txt','r+')


ParseFile(FirstPage)
ParseFile(SecondPage)
ParseFile(ThirdPage)
ParseFile(FourthPage)
ParseFile(FifthPage)
ParseFile(SixthPage)
ParseFile(SeventhPage)
ParseFile(EighthPage)
ParseFile(NinthPage)
ParseFile(a10page)
ParseFile(a11page)
ParseFile(a12page)
