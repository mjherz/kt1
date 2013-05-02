names=[] 
fullnames=[]
sex=[]
age=[]
sibsp=[]
parch=[]
title=[]
survived=[]
pclass=[]
ticket=[]
fare=[]
cabin=[]
embarked=[]

i=0
import csv

with open('train.csv', 'rb') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        i=i+1
        if (i>1):
            names.append((row[2].partition(', '))[0])
            fullnames.append(row[2])
	    sex.append(row[3])
            age.append(row[4])
            sibsp.append(int(row[5]))
            parch.append(int(row[6]))
            titlehold=row[2].partition(', ')[2]
            title.append(titlehold.partition('.')[0])
	    survived.append(row[0])
	    pclass.append(row[1])
	    ticket.append(row[7]
	    fare.append(row[8])
	    cabin.append(row[9])
	    embarked.append(row[10])


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

#test cases



import csv

headerfile=['survived','pclass','name','sex','age','sibsp','parch','ticket','fare','cabin','embarked','husband','wife','father','mother','son','daughter','title']

#write 
with open('trainp1.csv', 'wb') as tfile:
	file_writer = csv.writer(tfile)
	file_writer.writerow(headerfile)
	for i in range(len(names)):
  		writeline=[survived[i],pclass[i],fullnames[i],sex[i],age[i],sibsp[i],parch[i],ticket[i],fare[i],cabin[i],embarked[i],husband[i],wife[i],father[i],mother[i],son[i],daughter[i],title[i]]
		file_writer.writerow(writeline)


parch[0:20]
names[13]
for i in range(len(names)):
	if names[i]==names[13]:
		print i, sex[i],age[i],title[i],parch[i],father[i],mother[i],husband[i],wife[i],son[i],daughter[i]

for i in range(len(countnames)):
	if countnames[i]==countnames[13]:
		print countfather[i],countmother[i],counthusband[i],countwife[i],countson[i],countdaughter[i]

                    
