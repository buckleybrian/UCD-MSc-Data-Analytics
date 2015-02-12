## Brian Buckley 14203480
##
## STAT40730 Assignment 1

#### Task 1: manipulation ####

## 1 Set working directory to the folder that contains the names data and read in a file
setwd("C:/Users/buckleyb/Documents/Personal/Courses/UCD Data Analytics/STAT40730 Data Prog with R/Assignment1/names")
data1950<-read.csv("./yob1950.txt", header=FALSE, stringsAsFactors=F)

## 2 Answer some specific questions about this data set

# (a) How many children were born in 1950
numBorn<-nrow(data1950)
sprintf("According to these data there were %d children born in 1950", numBorn) # numBorn is 10,301

# (b) Which were the 10 most popular names for each sex
popFemales1950<-subset(data1950, data1950[[2]]=='F')[1:10,]
sprintf("10 most popular female names in 1950 were\n")
sprintf("%s", popFemales1950[,1])

popMales1950<-subset(data1950, data1950[[2]]=='M')[1:10,]
sprintf("10 most popular male names in 1950 were\n")
sprintf("%s", popMales1950[,1])

# (c) Are there any names in the data set that were only given once
lessFive<-nrow(subset(data1950, data1950[[3]]<5))
if (lessFive > 0) {
    onceOnly<-nrow(subset(data1950, data1950[[3]] == 1))
    if (onceOnly > 0) {
        sprintf("There are &d names in the data only given once", onceOnly) 
    } else {
        sprintf("There are no names in the data only given once")
    }
} else {
    sprintf("There are no names given less than 5 times")
}

## 3 Which female and male name had the biggest rise/fall compared with 1950
##   I interpret this question as the name that had the most/least difference from
##   their position in 1950 to their position in 2010

data2010<-read.csv("./yob2010.txt", header=FALSE, stringsAsFactors=F)

# Convenience function to do the 1950/2010 comparison
comparePop = function (data1, data2) { # data1 is 1950, data2 is 2010
    for (i in 1:nrow(data1)) {
        for (j in 1:nrow(data2)) {
            
            # Find a matching name in 2010 and add the 2010 value in a new column
            if (data2[j,1] == data1[i,1]) {
                data1[i,4]<-data2[j,3]
                
                # calculate the popularity difference from 1950 to 2010
                data1[i,5]<-data1[i,4]-data1[i,3]
                
                break
            }
        }
    }
    return(data1)
}
femCompare<-comparePop(popFemales1950, data2010)

# Which female name had the biggest rise/fall

femOrdered<-femCompare[with(femCompare, order(-V5)),]
sprintf("The biggest female rise was %s", femOrdered[1]$V1[1])
sprintf("The biggest female fall was %s", femOrdered[1]$V1[10])

# Now males
maleData2010<-subset(data2010, data2010[[2]]=='M')
malCompare<-comparePop(popMales1950, maleData2010)

# Which male name had the biggest rise/fall
maleOrdered<-malCompare[with(malCompare, order(-V5)),]
sprintf("The biggest male rise was %s", maleOrdered[1]$V1[1])
sprintf("The biggest male fall was %s", maleOrdered[1]$V1[10])

## 4 Load all of the files for each year into a single data frame with 4 columns: year, name, sex and number of births

files<-list.files(".", full.names=TRUE, pattern = "\\.txt$")
# convenience function to read a set of files and merge into a single data frame
readData = function (fs) {
    dataFrame<-data.frame(name=character(0), sex=character(0), number=integer(0), year=integer(0))
    for (i in 1:length(fs)) {
        if (exists('dataItem')) rm(dataItem)
        dataItem<-read.csv(fs[i], header=FALSE)
        
        # Get the year from the filename
        yearStr<-substr(fs[i], 6, 9)
        dataItem$year<-as.integer(yearStr)
        
        # merge into a global data frame
        colnames(dataItem)<-c("name", "sex", "number", "year")
        dataFrame<-rbind(dataFrame, dataItem)
    }
    return(dataFrame)
}
dataAll<-readData(files)

# show that we have successfully loaded all of the files into a single data frame
str(dataAll)

#### Task 2: analysis ####

## 1 Produce a table of the popularity of your name over each year
brianData<-subset(dataAll, name == "Brian" & sex == "M", select=c(number, year))
brianPop<-aggregate(number~year, data=brianData, sum)
View(brianPop)

# What year was the maximum for your name
brianOrdered<-brianPop[with(brianPop, order(-number, year)),]
sprintf("%d was the year my name, 'Brian' was most popular", brianOrdered$year[1])

## 2 Create a table showing the total births by sex and year
fems<-subset(dataAll, sex == "F")
femSum<-aggregate(number ~ year, data=fems, sum)
View(femSum)

males<-subset(dataAll, sex == "M")
maleSum<-aggregate(number ~ year, data=males, sum)
View(maleSum)

# Do males or females tend to have higher birth rates
summary(femSum)
summary(maleSum)
print("Male birth rates have both a higher mean and median than female birth rates so overall males tend to have higher birth rates")
sprintf("specifically females have approximately 98% mean and 96% median birth rate as males over the whole data set")

## 3 create a table of the frequency of different last letters in names for years 1910, 1060 and 2010
## for males and females
fs<-c("./yob1910.txt", "./yob1960.txt", "./yob2010.txt")
lastLett<-list(lett=letters, freq=rep(0,26) )
result<-readData(fs)
for (i in 1:length(result$name)) {
    if(exists('name')) rm(name)
    name<-as.vector(result$name[i])
    result$lastLett[i]<-substr(name, nchar(name), nchar(name));
}
fem<-subset(result, sex =="F")
femTable<-table(fem$lastLett)

mal<-subset(result, sex =="M")
malTable<-table(mal$lastLett)

# Which last letter(s) stand out as having the biggest increase/decrease

fmax<-names(which(femTable == max(femTable)))
fmin<-names(which(femTable == min(femTable)))
sprintf("The letter '%s' has the biggest increase for females", fmax)
sprintf("The letter '%s' has the biggest decrease for females", fmin)

mmax<-names(which(malTable == max(malTable)))
mmin<-names(which(malTable == min(malTable)))
sprintf("The letter '%s' has the biggest increase for males", mmax)
sprintf("The letter '%s' has the biggest decrease for males", mmin)

## 4 Which are the most popular palindromic names
palindromes<-list()
pcount = 1
for (i in 1:length(dataAll$name)) {
    if(exists(c))rm(c)
    
    # convert whole name to upper case to make the palindromic test easier
    c<-toupper(as.character(dataAll$name[i]))
    
    # test for palindromic and if true add to the list
    if(identical(strsplit(c, "")[[1]], rev(strsplit(c, "")[[1]])) == TRUE) {
        palindromes$name[pcount]<-as.character(dataAll$name[i])
        palindromes$number[pcount]<-as.numeric(dataAll$number[i])
        palindromes$year[pcount]<-as.numeric(dataAll$year[i])
        pcount = pcount + 1
    }
}
pframe<-as.data.frame(palindromes)
pSum<-aggregate(number ~ name, data=pframe, sum)
pOrdered<-pSum[with(pSum, order(-number)),]
print("The most popular palindromic names are")
head(pOrdered)

# proportion of palindromic names per year
totSum<-aggregate(number ~ year, data=dataAll, sum)
palSum<-aggregate(number ~ year, data=pframe, sum)
pmerge<-cbind(totSum, palSum)
colnames(pmerge)<-c("tyear", "tnumber", "pyear", "pnumber")
plist<-as.list(pmerge)
for (i in 1:length(plist$tyear)) {
    plist$prop[i]<-plist$pnumber[i]/plist$tnumber[i]
}

# Are such names on the increase

# look at a table
View(plist)

# easier to look at a plot
plot(plist$pyear, plist$prop, xlab="Year", ylab="Palindromic proportion")
printf("The plot of the proportion of palidromic names over time shows that such names are not on the increase")

#### Task 3: creativity ####

# plot both to get a finer level of granularity on birth rates than summary data
plot(femSum)
points(maleSum, pch=19)
print("From the plot we see that prior to 1940 females tended to have higher birth rates")
print("but after 1940 males tended to have higher birth rates.")
print("The dramatic rise in both birth rates from ~1940 to ~1960 corresponds to the World War II 'baby bommer years'")

# plot my name's popularity
plot(x=brianPop$year, y=brianPop$number, col="green3", xlab="Year", ylab="number of registrations for 'Brian'")
title("Popularity of the name 'Brian' from 1900 to 2011")
print("My name was very unpopular until ~1940 when it grew significantly until the early 1970s")
print("when it declined markedly in popularity.  However, at the end of the data set (2011) it was still")
print("as popular as in around 1950.  The data set is skewed therefore to the left")
