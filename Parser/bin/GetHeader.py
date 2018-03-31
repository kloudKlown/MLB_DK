import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
import time
from datetime import datetime, timedelta

soup = BeautifulSoup(open("headers.html"))

AllDivs = soup.findAll('div',{'class': 'ag-header-cell-label' })

CreateStatmemt = "CREATE TABLE `players ("

def ColumnFormating(text):
	# text = text.replace('\xce','')
	text = text.replace('%','')
	text = text.replace('/','_')
	text = text.replace('+/-','_')
	text = "`" + text + "`"
	text = text + ' varchar(100) DEFAULT NULL, \n'
	return text	

for each in AllDivs:
	CreateStatmemt = CreateStatmemt + ColumnFormating(each.text)

print CreateStatmemt	