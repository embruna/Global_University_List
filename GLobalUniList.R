
# R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING DATA ON THE INSTITUTIONS OF JOURNAL EDITORS 


# Changes to be made: 
# 1) If possible pull the csv file directly uploaded as "global_uni1" directly from the repo 
# 2) If possible download "world_university_names.sql" directly from the repo
# 3) I did a bunch of hacking around to get the countries and uni names, when should be able to get 
#    much more easily by making quesries using SQL tools. 
# 4) WHY IS IT PUTTING AMERICAN UNIVERSITY - DUBAI in INDIA? Change country code un unis.df

# R & Package versions:
# R version = 3.3.1 (2016-06-21)
# tidyverse = 1.1.1


##############################################################
##############################################################
#
# GETTING STARTED
#
##############################################################
##############################################################

# Load required libraries
library(tidyverse)
library(stringr)
library(stringi)

# Clear the environment 
rm(list=ls())

# Import list of world universities downloaded from this GitHub Repo: https://github.com/endSly/world-universities-csv
global_uni1 = read_csv("https://raw.githubusercontent.com/endSly/world-universities-csv/master/world-universities.csv", col_names=FALSE)
global_uni1<-global_uni1 %>% rename(ISO2=X1,uni.name=X2,website=X3)
global_uni1$uni.name<-as.factor(global_uni1$uni.name)
global_uni1$ISO2<-as.factor(global_uni1$ISO2)
global_uni1$website<-as.factor(global_uni1$website)
str(global_uni1)
summary(global_uni1)


# library("RSQLite")
# library("R.utils")
#download this sql: https://github.com/gedex/World-University-Names-Database/blob/master/world_university_names.sql
numLines <- R.utils::countLines("./Data/world_university_names.sql")
FullUniDB <- readLines("./Data/world_university_names.sql",n=numLines)


# CREATE THE DATABASE OF COUNTRY CODES & NAMES
countries.df<-FullUniDB[34:279]

countries.df<-gsub("(", "", countries.df, fixed=TRUE)
countries.df<-gsub("'),", "", countries.df, fixed=TRUE)
countries.df<-gsub("'", "", countries.df, fixed=TRUE)
countries.df<-gsub(";", "", countries.df, fixed=TRUE)
countries.df<-gsub(")", "", countries.df, fixed=TRUE)
countries.df<-as.data.frame(countries.df)
countries.df$countries.df<-as.character(countries.df$countries.df)
countries.df<-separate(countries.df, countries.df, c("country.ID", "ISO2","ISO3","country","modifier"), sep = ", ", remove = FALSE, convert = FALSE, extra = "merge", fill = "right")
countries.df$country[countries.df$ISO3 == "VGB"]  <- "British Virgin Islands"
countries.df$country[countries.df$ISO3 == "PRK"]  <- "Democratic Peoples Republic of Korea (NK)"
countries.df$country[countries.df$ISO3 == "COD"]  <- "Democratic Republic of the Congo"
countries.df$country[countries.df$ISO3 == "FSM"]  <- "Federated States of Micronesia"
countries.df$country[countries.df$ISO3 == "VIR"]  <- "US Virgin Islands"
countries.df$country[countries.df$ISO3 == "KOR"]  <- "Republic of Korea (SK)"
countries.df$country[countries.df$ISO3 == "PSE"]  <- "Palestinian Territory, Occupied"
countries.df$country[countries.df$ISO3 == "IRN"]  <- "Iran, Islamic Republic of"
countries.df$country[countries.df$ISO3 == "TZA"]  <- "Tanzania, United Republic of"
countries.df<-countries.df %>% select(-countries.df,-modifier)
countries.df$country.ID<-as.factor(countries.df$country.ID)
countries.df<-countries.df %>% rename(ISO2=ISO2 ,ISO3=ISO3 ,country=country )
countries.df$ISO2<-as.factor(countries.df$ISO2)
countries.df$ISO3<-as.factor(countries.df$ISO3)
countries.df$country<-as.factor(countries.df$country)



# CREATE THE UNIVERSITY DATABASE #1
unis.df <-FullUniDB[300:9537]
# NB better to go in 1 comma, 1in one comma, in from last comma (URL and then the rest is inst name)

# Change to Split at first comma, then at second, then done.
unis.df<-gsub("(", "", unis.df, fixed=TRUE)
unis.df<-gsub("(", "", unis.df, fixed=TRUE)
unis.df<-gsub(")", "", unis.df, fixed=TRUE)
unis.df <-gsub(", '", ",", unis.df , fixed=TRUE)
unis.df <-gsub("''", "'", unis.df , fixed=TRUE)
unis.df <-gsub("',", ",", unis.df , fixed=TRUE)
unis.df <-gsub("'", "", unis.df , fixed=TRUE)
unis.df <-unis.df %>% str_split(",",simplify=TRUE) 
unis.df <-as.data.frame(unis.df)
unis.df <- sapply(unis.df[1:ncol(unis.df)],as.character)
unis.df <-as.data.frame(unis.df)

str(unis.df)
unis.df$V3<-trimws(unis.df$V3)
unis.df$V4<-trimws(unis.df$V4)
unis.df$V5<-trimws(unis.df$V5)
unis.df$V6<-trimws(unis.df$V6)

unis.df<-unis.df%>% unite_("uni.name", c("V3","V4","V5","V6"), sep=" ", remove=TRUE)
unis.df<-unis.df %>% rename(uni.ID=V1,country.ID=V2) %>% select(-uni.ID,-V7)

#Remove some extra spaces
unis.df$uni.name <-trimws(unis.df$uni.name, which = "right")
unis.df$uni.name<-gsub("  ", " ", unis.df$uni.name, fixed=TRUE)
#delete some prolematic rows
unis.df<-unis.df[-c(1024, 2061, 2958, 3953,5084,6156,7144,7895,8884),]
unis.df$country.ID<-droplevels(unis.df$country.ID)
unis.df$country.ID<-trimws(unis.df$country.ID)

###################################################################
# CREATE THE UNIVERSITY DATABASE #2 (this one has the web addresses)
unis.web.df <-FullUniDB[9559:26356]
unis.web.df <-gsub("(", "", unis.web.df , fixed=TRUE)
unis.web.df <-gsub(", '", ",", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("',", ",", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("/'),", "", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("''", "'", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("  ", " ", unis.web.df , fixed=TRUE)

unis.web.df <-unis.web.df %>% str_split(",",n=3,simplify=TRUE) 
unis.web.df<-as.data.frame(unis.web.df)
unis.web.df$V3<-as.character(unis.web.df$V3)
str(unis.web.df)
unis.web.df[1:10,]
# https://stackoverflow.com/questions/24938616/string-split-on-last-comma-in-r
uniweb<-str_split(unis.web.df$V3, ",\\s*(?=[^,]+$)", simplify=TRUE)
uniweb<-as.data.frame(uniweb)

unis.web.df<-bind_cols(unis.web.df, uniweb)
unis.web.df<-unis.web.df %>% rename(uni.ID=V1,country.ID=V2,original=V3,uni.name=V11,website=V21)
unis.web.df<-unis.web.df %>% select(-original, -uni.ID)
rm(uniweb)
#Remove asterisk and some extra spaces
unis.web.df$uni.name <-gsub("*", "", unis.web.df$uni.name , fixed=TRUE)
unis.web.df$uni.name <-trimws(unis.web.df$uni.name, which = "right")
# add slash to match website in other df 
unis.web.df$website <- paste(unis.web.df$website, "/", sep="")

unis.web.df$country.ID<-as.factor(unis.web.df$country.ID)


unis.web.df<-unis.web.df[-c(682,1307, 1964, 2648, 3353, 4009, 4673, 5234, 5878, 6524, 7202, 7870, 8598, 9264,9900,10558,11144,11738,12379,13066,13770,14497,15216,15922,16617),]
unis.web.df$country.ID<-droplevels(unis.web.df$country.ID)
unis.web.df$country.ID<-trimws(unis.web.df$country.ID)

## NOTE THAT SOME OF THE UNIVERSITIES HAVE WEBSOTES BUT NO UNINAMES!!!



###################################
#add ISO2 and ISO3 to the uni databases
####################################
# for global_uni1
global_uni1<-left_join(global_uni1,countries.df, by="ISO2")
global_uni1$ISO2<-as.factor(global_uni1$ISO2)
global_uni1<-global_uni1 %>% select(country.ID,country,ISO2,ISO3,uni.name,website)

# for unis.df
unis.df<-left_join(unis.df,countries.df, by="country.ID")

# for unis.web.df
unis.web.df<-left_join(unis.web.df,countries.df, by="country.ID")
unis.web.df<-unis.web.df %>% select(country.ID,country,ISO2,ISO3,uni.name,website)

# Consolidate the three
# First might be easiest to convert the diacritical marks / accents
# use stri_trans_general() from stringi library
unis.web.df$uni.name2<-stri_trans_general(unis.web.df$uni.name, "Latin-ASCII")
unis.df$uni.name2<-stri_trans_general(unis.df$uni.name, "Latin-ASCII")
global_uni1$uni.name2<-stri_trans_general(global_uni1$uni.name, "Latin-ASCII")

# The remove any hyphens or commas
unis.web.df$uni.name2<-gsub(", ", " ", unis.web.df$uni.name2, fixed=TRUE)
unis.df$uni.name2<-gsub(", ", " ", unis.df$uni.name2 , fixed=TRUE)
global_uni1$uni.name2<-gsub(", ", " ", global_uni1$uni.name2 , fixed=TRUE)

# Are there any in unis.df NOT in unis.web.df when searching by uni.name?
foo1<-anti_join(unis.df,unis.web.df,by="uni.name2") #6523 of the 9229 
#do similarity analyses of these with unis.web.df$names2 and after removing any similar add to the master list

# Are there any in global_uni1 NOT in unis.web.df when searching by uni.name?
foo2<-anti_join(global_uni1,unis.web.df,by="website")  
foo3<-anti_join(global_uni1,unis.web.df,by="uni.name2") 
foo4<-bind_rows(foo2,foo3) #put them together
foo5<-unique(foo4,by="uni.name2") #remove the duplicates

#put the two datafiles together 
foo6<-bind_rows(foo1,foo5)
foo7<-unique(foo6)

# Consolidate them with unis.web.df
Consolidated.uni.df<-rbind(foo7,unis.web.df)
Consolidated.uni.df$uni.ID<-1:nrow(Consolidated.uni.df)
rm(foo1,foo2,foo3,foo4,foo5,foo6,foo7,global_uni1,unis.df,unis.web.df)

Consolidated.uni.df<-Consolidated.uni.df %>% select(uni.ID,country.ID,ISO2,ISO3,country,uni.name,uni.name2,website)
# to get the initials of each uni I just deleted lower-case letters http://r.789695.n4.nabble.com/Extract-upper-case-letters-td4634664.html
Consolidated.uni.df$initials<-gsub("[^::A-Z::]","", Consolidated.uni.df$uni.name2)
Consolidated.uni.df$uni.code <- paste(Consolidated.uni.df$ISO3,"-",Consolidated.uni.df$initials,  sep="")

# Now do a similarity analysis of name remove the duplicates
# DO SOME ERROR CORRECTION:
  # 1) SOME ARE MISSING UNI NAMES, EG, INST NAME IS "UNIVERSITY": COUNT CHARACTERS,SORT, AND LOOK AT ONES WITH LEAST CAHARACTERS
