#For BMB meeting Feb 10 2020

#Last we left off:
#Tried to recreate shared users from code from Adrian
#Results looked like a crime scene
#BMB suggested to look through even earlier stuff than the shared users df
#Went right to the scrape and went from therer

#Problems that may not be problems:
#Even though my WNS repo is a "clone" (not gitclone more like stormtrooper type of clone)
#The csv files are different enough that they are causing different results.
#I took the one with more data (the original) and have them both in here

#Solutions:
#Went right to scrape and re-organized all user information in order to create shared user
#Data frame has lots of 3822 users visiting more than 1 county throughout the time in question

#Problems that are actually problems:
#Went through and made counties from GC coords and identified counties.
#Unfortunately Canada does not have any resources for counties (since there are so many types)
#and so many of them (only 33 once I leftjoined all info about CAD counties I could find) had
#to be implented by hand. Should I do something better than this? 
#It was done for USA counties but just can't find CAD data for county level

#Another problem was a gsub for strings of counties. I realized it does not work with 2 name
#states ie New York, New Jersey ect. 

#Making full model is easy after this step. 

#After that it will be scaling the shared users matrix and maybe possibly addressing JD's 
#point about having it as a rixed effect? 

#Code I think we might need to look at
source("scripts/packages.R") #import just in case
#The difference between MS and AF data on shared users
source("scripts/huntingforsharedusers.R")
source("scripts/huntingfordifferentsharedusers.R")
source("scripts/lonlattocountyconversion.R")
#Finally creation of shared users df
source("scripts/creatingsharedusersdf.R")
