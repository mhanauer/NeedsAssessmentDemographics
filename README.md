---
title: "Needs General Demographics"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Steps: 
1. Get rid of extra data for each 
2. Combine the relevant data
3. Grab demographics gender, degree, type of teacher, race
4. CCR grab all the and transform to numbers
5. Get an overall average value
6. Export the data 

Probably should get general demographics first.  Need to combine them and then 

Get the variables I want for SEL  

SELQuant1 = SEL first survey quantitative question 
SELQualRes = SEL resources question
SELQualBarr = SEL barriers question

Here is for RBBCSC
```{r}
setwd("~/Desktop/QualData")
rbbcsc = read.csv("RBBCSCStaffSurvey.csv", header = TRUE); head(rbbcsc)

rbbcsc = rbbcscTest[-c(1:7),]; head(rbbcsc, 20)
rbbcsc = rbbcsc[,-c(1:16)]; head(rbbcsc, 20)

rbbcsc = rbbcsc[,-c(38:54)]; head(rbbcsc, 20)
rbbcsc = rbbcsc[,-c(40:44)]; head(rbbcsc, 20)
head(rbbcsc)
rbbcsc = as.data.frame(rbbcsc)

rbbcsc1 = rbbcsc[,1:8]
eth = rbbcsc[c("Q28")]
gender = rbbcsc[c("Q10")]
edu = rbbcsc[c("Q12")]
job = rbbcsc[c("Q15")]
rbbcsc = cbind(rbbcsc1,eth, gender, edu, job)
head(rbbcsc, 20)
# Quantiative, qual questions, and demographics
head(rbbcsc)
colnames(rbbcsc) = c("SELQuant1", "SELQuant2", "SELQuant3", "SELQuant4", "SELQuant5", "SELQuant6", "SELQualRes", "SELQualBarr", "eth", "gender", "edu", "job") 
head(rbbcsc)



```
Now we want to change all of the quantitative variables to these values and then cbind them back.  So first let us create a new data set with only the quantitative values.  Then we need get the other demographic factors and then combine them back.

```{r}
rbbcsc2 = rbbcsc[,1:6] 
rbbcsc2 = apply(rbbcsc2, 2, function(x){ifelse(x == "Strongly agree", 7, x)}); rbbcsc2
rbbcsc2 = as.data.frame(rbbcsc2)
rbbcsc2 = apply(rbbcsc2, 2, function(x){ifelse(x == "Agree", 6, x)}); rbbcsc2
rbbcsc2 = as.data.frame(rbbcsc2)
rbbcsc2 = apply(rbbcsc2, 2, function(x){ifelse(x == "Somewhat agree", 5, x)}); rbbcsc2
rbbcsc2 = as.data.frame(rbbcsc2)
rbbcsc2 = apply(rbbcsc2, 2, function(x){ifelse(x == "Neither agree nor disagree", 4, x)}); 
rbbcsc2 = as.data.frame(rbbcsc2)
rbbcsc2 = apply(rbbcsc2, 2, function(x){ifelse(x == "Somewhat disagree", 3, x)}); rbbcsc2
rbbcsc2 = as.data.frame(rbbcsc2)
rbbcsc2 = apply(rbbcsc2, 2, function(x){ifelse(x == "Disagree", 2, x)}); rbbcsc2
rbbcsc2 = as.data.frame(rbbcsc2)
rbbcsc2 = apply(rbbcsc2, 2, function(x){ifelse(x == "Strongly disagree", 1, x)}); rbbcsc2
rbbcsc2 = as.data.frame(rbbcsc2)
write.csv(rbbcsc2, "rbbcsc2.csv", row.names = FALSE)
rbbcsc2 = read.csv("rbbcsc2.csv", header = TRUE)
head(rbbcsc2)

```
Now MCCSC SEL
```{r}
setwd("~/Desktop/QualData")
mccscTest = read.csv("mccscStaffSurvey.csv", header = TRUE)
head(mccsc)

mccsc1 = mccsc[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q2", "Q3")]
eth = mccsc[c("Q30")]
gender = mccsc[c("Q36")]
edu = mccsc[c("Q38")]
job = mccsc[c("Q44")]
mccsc = cbind(mccsc1,eth, gender, edu, job)
head(mccsc, 20)
# Quantiative, qual questions, and demographics
head(mccsc)
colnames(mccsc) = c("SELQuant1", "SELQuant2", "SELQuant3", "SELQuant4", "SELQuant5", "SELQuant6", "SELQualRes", "SELQualBarr", "eth", "gender", "edu", "job") 
head(mccsc)

mccsc2 = mccsc[,1:6] 
mccsc2 = apply(mccsc2, 2, function(x){ifelse(x == "Strongly agree", 7, x)}); mccsc2
mccsc2 = as.data.frame(mccsc2)
mccsc2 = apply(mccsc2, 2, function(x){ifelse(x == "Agree", 6, x)}); mccsc2
mccsc2 = as.data.frame(mccsc2)
mccsc2 = apply(mccsc2, 2, function(x){ifelse(x == "Somewhat agree", 5, x)}); mccsc2
mccsc2 = as.data.frame(mccsc2)
mccsc2 = apply(mccsc2, 2, function(x){ifelse(x == "Neither agree nor disagree", 4, x)}); 
mccsc2 = as.data.frame(mccsc2)
mccsc2 = apply(mccsc2, 2, function(x){ifelse(x == "Somewhat disagree", 3, x)}); mccsc2
mccsc2 = as.data.frame(mccsc2)
mccsc2 = apply(mccsc2, 2, function(x){ifelse(x == "Disagree", 2, x)}); mccsc2
mccsc2 = as.data.frame(mccsc2)
mccsc2 = apply(mccsc2, 2, function(x){ifelse(x == "Strongly disagree", 1, x)}); mccsc2
mccsc2 = as.data.frame(mccsc2)
write.csv(mccsc2, "mccsc2.csv", row.names = FALSE)
mccsc2 = read.csv("mccsc2.csv", header = TRUE)
head(mccsc2)
```


So grab the ones you are interested in get the length then divide that both the total to get the percentage for the category that you are interested in.
```{r}
both = as.data.frame(rbind(mccsc, rbbcsc))
dim(both)
bothQuan = na.omit(both)
bothQuan = as.data.frame(both)
dim(both)
both = apply(both,2, function(x){ifelse(x == "NA", NA, x)})
both = na.omit(both)
both = as.data.frame(both)
bothN = nrow(both)

## Gender only male rest if female
bothMale = as.data.frame(both[both$gender == "Male",])
bothMaleN = nrow(bothMale)
bothMalePerc = bothMaleN / bothN; bothMalePerc 

bothFemalePerc = 1-bothMalePerc


```
Now we are getting ethnicity white, black, hispanic, and other
Make things add up to one.
```{r}
both = as.data.frame(rbind(mccsc, rbbcsc))
dim(both)
bothQuan = na.omit(both)
bothQuan = as.data.frame(both)
dim(both)
both = apply(both,2, function(x){ifelse(x == "NA", NA, x)})
both = na.omit(both)
both = as.data.frame(both)
bothN = nrow(both)

## Now white people
bothWhite = as.data.frame(both[both$eth == "White",])
bothWhiteN = nrow(bothWhite)
bothWhitePerc = bothWhiteN / bothN; bothWhitePerc 

## Now black people
bothBlack = as.data.frame(both[both$eth == "Black or African American",])
bothBlackN = nrow(bothBlack)
bothBlackPerc = bothBlackN / bothN; bothBlackPerc 

# Now Hispanic people
bothHis = as.data.frame(both[both$eth == "Hispanic, Latino, or Spanish origin",])
bothHisN = nrow(bothHis)
bothHisPerc = bothHisN / bothN; bothHisPerc 

# Now Some other race, ethnicity, or origin (please specify)
bothOther = as.data.frame(both[both$eth == "Some other race, ethnicity, or origin (please specify)",])
bothOtherN = nrow(bothOther)
bothOtherPerc = bothOtherN / bothN; bothOtherPerc




```

