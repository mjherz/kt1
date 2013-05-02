names=[] 
fullnames=[]
sex=[]
age=[]
sibsp=[]
parch=[]
title=[]
pclass=[]
ticket=[]
fare=[]
cabin=[]
embarked=[]

i=0
import csv

with open('test.csv', 'rb') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        i=i+1
        if (i>1):
            names.append((row[1].partition(', '))[0])
            fullnames.append(row[1])
	    sex.append(row[2])
            age.append(row[3])
            sibsp.append(int(row[4]))
            parch.append(int(row[5]))
            titlehold=row[1].partition(', ')[2]
            title.append(titlehold.partition('.')[0])
	    pclass.append(row[0])
	    ticket.append(row[6])
	    fare.append(row[7])
	    cabin.append(row[8])
	    embarked.append(row[9])


#convert age

for i in range(len(age)):
    if age[i]!='':
        age[i]=int(float(age[i]))


#develop a table of unique names

countnames=[]

for i in range(len(names)):
    if not (names[i] in countnames):
        countnames.append(names[i])


#determine mother status 

countmother=['no mother']*len(countnames)
countmotherage=[0]*len(countnames)
mother=['FALSE']*len(names)

countmotherparch=[0]*len(countnames)

for i in range(len(names)):
    if ((sex[i]=='female') and (parch[i]>=1) and (title[i]=='Mrs')):
        mother[i]='TRUE'

        for j in range(len(countnames)): 
            if countnames[j]==names[i]: 
                countmother[j]='has mother'
                countmotherage[j]=age[i]
		countmotherparch[j]=parch[i]

countdaughter=['no daughter']*len(countnames)
daughter=['FALSE']*len(names)

countson=['no son']*len(countnames)
son=['FALSE']*len(names)

counthusband=['no husband']*len(countnames)
husband=['FALSE']*len(names)

countwife=['no wife']*len(countnames)
wife=['FALSE']*len(names)

countfather=['no father']*len(countnames)
father=['FALSE']*len(names)

#base family structure on mother
#determine other family members

for i in range(len(countnames)):
    if countmother[i]=='has mother':
        for j in range(len(names)):
            if (names[j]==countnames[i]):
                if (mother[j]!='TRUE' and parch[j]>=1 and title[j]=='Miss'):
                    countdaughter[i]='has daughter' 
                    daughter[j]='TRUE'

                elif (title[j]=='Master'):
                    countson[i]='has son'
                    son[j]='TRUE'

                elif (title[j]=='Mr' and age[j]==''):
                    countson[i]='unknown'
                    son[j]='unknown'  

                elif (title[j]=='Mr' and age[j]<(countmotherage[i]-15)):
                    countson[i]='has son'
                    son[j]='TRUE'

for i in range(len(countnames)):
        for j in range(len(names)):
               	if (title[j]=='Mr' and age[j]>=countmotherage[i] and countmotherparch[i]==parch[j] and countnames[i]==names[j]):
                 	counthusband[i]='has husband'
                    	husband[j]='TRUE'
		    	if (parch[j]>=1 and countmother[i]=='has mother'):
				countfather[i]='has father'
				father[j]='TRUE'

for i in range(len(countnames)):
	for j in range(len(names)):
		if (countnames[i]==names[j] and counthusband[i]=='has husband' and title[j]=='Mrs'):
			countwife[i]='has wife'
			wife[j]='TRUE'


import csv

headerfile=['pclass','name','sex','age','sibsp','parch','ticket','fare','cabin','embarked','husband','wife','father','mother','son','daughter','title','lastname']

#write 
with open('testp1.csv', 'wb') as tfile:
	file_writer = csv.writer(tfile)
	file_writer.writerow(headerfile)
	for i in range(len(names)):
  		writeline=[pclass[i],fullnames[i],sex[i],age[i],sibsp[i],parch[i],ticket[i],fare[i],cabin[i],embarked[i],husband[i],wife[i],father[i],mother[i],son[i],daughter[i],title[i],names[i]]
		file_writer.writerow(writeline)


