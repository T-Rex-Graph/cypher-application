# In this script include packages, functions, datasets and anyting that will be 
# used both by UI and server

############################.
##Packages ----
############################.
library(shiny)
library(rorcid)
library(rcrossref)
library(stringr)
library(readr)
library(uuid)
library(rgoodreads)
#API Access for rGoodReads
access<- "C:/Users/nkell/Desktop/Prototype App - Copyv3/rGoodReadsKey.txt"
key <- read_delim(access, delim = ',')
# set the key to access data - varies with API
Sys.setenv(GOODREADS_KEY = key$key)

#rGoodReads example
Satzinger <- book_by_isbn("1305117204")
View(Satzinger)
#doesnt work with ISBN 978-981-15-9417-5

#CREATE (:Publication {DOI: ' 10.25300/MISQ/2020/14458 ', 
#  citation: ' Pentland, B. T., Liu, P., Kremser, W., & Haerem, T. (2020). 
#  The Dynamics of Drift in Digitized Processes. MIS Quarterly, 44(1), 19-47. doi:10.25300/misq/2020/14458 ', 
#  pubUUID: ' 272a2cb2-27ac-11eb-b80d-71cd613b4c11 '}); 

#"CREATE (:Publication {ISBN: '",parameter$ISBN:,"',
#  citation:'",isbn_pub$authors,".'})"


#APA Citation
