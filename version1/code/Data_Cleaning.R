####################################################################
##### avg_temp.csv, h5n1_cdc_clean.csv, and h5n1_cdc.csv ###########
####################################################################
# This R script generates two important datasets, avg_temp.csv,    #
# h5n1_cdc_clean.csv, and h5n1_cdc.csv for our project.            #
#                                                                  #
# The following script shows how we cleaned and combined several   #
# source datasets.                                                 #
#                                                                  #
# !!! Pay attention to lines 377, 508, and 513 !!!                 #
#                                                                  #
# You need to change the directory path to your local directory to #
# export dataset successfully.                                     #
#                                                                  #
# Copyright 2023                                                   #
# Hengyuan Liu - University of California, Davis                   #
# Weilin Cheng - University of California, Davis                   #
# Kathy Mo     - University of California, Davis                   #
# Sida Tian    - University of Michigan, Ann Arbor                 #
# Li Yuan      - University of Michigan, Ann Arbor                 #
####################################################################

# Github address:
# https://github.com/GitData-GA/iasc2023/blob/main/code/Data_Cleaning.R

# Library
rm(list=ls())
set.seed(77)
library(dplyr)
library(ggplot2)
library(tidyr)
library(usmap)
library(ggpubr)
library(grid)
library(gridExtra)
library(patchwork)
library(sf)
library(knitr)
library("imputeTS")
library(textstem)
library(GGally)
library(pROC)
library(psych)
library(lmtest)
library(car)

#######################################################################################
########## Data 1: us.county ##########################################################
#######################################################################################
# Import data
us.county = read.csv("https://iasc2023.gitdata.ga/dataset/uscounties.csv", 
                     header = TRUE)
# Change column location
us.county = us.county %>%
  relocate(county, .after = state)
# Change state name to abbreviation
for (i in 1:dim(us.county)[1]){
  if (us.county$state[i] != "District of Columbia"){
    abb = state.abb[grep(us.county$state[i], state.name)]
    us.county$state[i] = abb
  }
}
dc.index = which(us.county$state == "District of Columbia")
us.county$state[dc.index] = "DC"


#######################################################################################
########## Data 2: h5n1.poultry.cdc.o #################################################
#######################################################################################
# Import data
h5n1.poultry.cdc.o = read.csv("https://iasc2023.gitdata.ga/dataset/hpai-poultry.csv",
                              header = TRUE)
# Ignore cases after January 31, 2023
h5n1.poultry.cdc = h5n1.poultry.cdc.o %>% 
  separate(Outbreak.Date, sep="-", into = c("Month", "Day", "Year")) %>% 
  unite("Year.Month", c("Year", "Month")) %>%
  filter(Year.Month != "2023_02") %>%
  relocate(County, .after = State)
# Add "County" after the name of each county
h5n1.poultry.cdc$County = paste(h5n1.poultry.cdc$County, "County")
non.poultry = which(h5n1.poultry.cdc$Flock.Type == "WOAH Non-Poultry")
h5n1.poultry.cdc$Flock.Type[non.poultry] = "Non-Poultry"
h5n1.poultry.cdc$Flock.Type[-non.poultry] = "Poultry"
colnames(h5n1.poultry.cdc) = c("state", "county", "year_month", 
                               "day", "type", "cases")


#######################################################################################
########## Data 3: h5n1.wild.cdc.o ####################################################
#######################################################################################
# Import data
h5n1.wild.cdc.o = read.csv("https://iasc2023.gitdata.ga/dataset/hpai-wild-birds.csv",
                           header = TRUE)
# Seprate year month day
h5n1.wild.cdc = h5n1.wild.cdc.o %>% 
  separate(Date.Detected, sep="/", into = c("Month", "Day", "Year"))
# Add 0 in front of months that only has a single
single.num.month = which(as.numeric(h5n1.wild.cdc$Month) < 10)
h5n1.wild.cdc$Month[single.num.month] = paste0("0", 
                                               h5n1.wild.cdc$Month[single.num.month])
# Add 0 in front of days that only has a single
single.num.day = which(as.numeric(h5n1.wild.cdc$Day) < 10)
h5n1.wild.cdc$Day[single.num.day] = paste0("0", 
                                           h5n1.wild.cdc$Day[single.num.day])
# Ignore cases after January 31, 2023
h5n1.wild.cdc = h5n1.wild.cdc %>%
  unite("Year.Month", c("Year", "Month")) %>%
  filter(Year.Month != "2023_02")
# Add "County" after the name of each county
h5n1.wild.cdc$County = paste(h5n1.wild.cdc$County, "County")
h5n1.wild.cdc = h5n1.wild.cdc[, c(1, 2, 3, 4, 7)]
colnames(h5n1.wild.cdc) = c("state", "county", "year_month", 
                            "day", "type")
h5n1.wild.cdc = h5n1.wild.cdc %>%
  count(state, county, year_month, day, type)
colnames(h5n1.wild.cdc) = c("state", "county", "year_month", 
                            "day", "type", "cases")


#######################################################################################
########## Data 4: Average Temperature by Month in each County ########################
#######################################################################################
# JAN 2022
avg_temp_01_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_01_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_01_22 = avg_temp_01_22[-c(1:4),]
avg_temp_01_22 = cbind(avg_temp_01_22, 1)
names(avg_temp_01_22)[7] = "month.index"
# FEB 2022
avg_temp_02_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_02_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_02_22 = avg_temp_02_22[-c(1:4),]
avg_temp_02_22 = cbind(avg_temp_02_22, 2)
names(avg_temp_02_22)[7] = "month.index"
# MAR 2022
avg_temp_03_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_03_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_03_22 = cbind(avg_temp_03_22[-c(1:4),], 3)
names(avg_temp_03_22)[7] = "month.index"
# APR 2022
avg_temp_04_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_04_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_04_22 = cbind(avg_temp_04_22[-c(1:4),], 4)
names(avg_temp_04_22)[7] = "month.index"
# MAY 2022
avg_temp_05_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_05_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_05_22 = cbind(avg_temp_05_22[-c(1:4),], 5)
names(avg_temp_05_22)[7] = "month.index"
# JUN 2022
avg_temp_06_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_06_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_06_22 = cbind(avg_temp_06_22[-c(1:4),], 6)
names(avg_temp_06_22)[7] = "month.index"
# JUL 2022
avg_temp_07_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_07_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_07_22 = cbind(avg_temp_07_22[-c(1:4),], 7)
names(avg_temp_07_22)[7] = "month.index"
# AUG 2022
avg_temp_08_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_08_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_08_22 = cbind(avg_temp_08_22[-c(1:4),], 8)
names(avg_temp_08_22)[7] = "month.index"
# SEP 2022
avg_temp_09_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_09_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_09_22 = cbind(avg_temp_09_22[-c(1:4),], 9)
names(avg_temp_09_22)[7] = "month.index"
# OCT 2022
avg_temp_10_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_10_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_10_22 = cbind(avg_temp_10_22[-c(1:4),], 10)
names(avg_temp_10_22)[7] = "month.index"
# NOV 2022
avg_temp_11_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_11_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_11_22 = cbind(avg_temp_11_22[-c(1:4),], 11)
names(avg_temp_11_22)[7] = "month.index"
# DEC 2022
avg_temp_12_22 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_12_22.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_12_22 = cbind(avg_temp_12_22[-c(1:4),], 12)
names(avg_temp_12_22)[7] = "month.index"
# JAN 2023
avg_temp_01_23 = read.csv("https://iasc2023.gitdata.ga/dataset/avg_temp_01_23.csv",
                          header=F, quote="\"", comment.char="")
avg_temp_01_23 = cbind(avg_temp_01_23[-c(1:4),], 13)
names(avg_temp_01_23)[7] = "month.index"
# Combine
avg.temp = rbind(avg_temp_01_22,avg_temp_02_22,avg_temp_03_22, avg_temp_04_22, 
                 avg_temp_05_22, avg_temp_06_22, avg_temp_07_22, avg_temp_08_22,
                 avg_temp_09_22, avg_temp_10_22, avg_temp_11_22, avg_temp_12_22,
                 avg_temp_01_23)
avg.temp = avg.temp %>% 
  separate(V1, sep="-", into = c("state", "id")) %>%
  rename(county = V2) %>%
  rename(avg.temp = V3) %>%
  select(state, county, month.index, avg.temp)
avg.temp$avg.temp = as.numeric(avg.temp$avg.temp)
# Lower case
avg.temp$county = tolower(avg.temp$county)
# Fix county names
avg.temp$county[which(avg.temp$county == "dona ana county")] = "do??a ana county"
avg.temp$county[which(avg.temp$county == "la salle parish")] = "lasalle parish"
# Fix "lexington city" with the same value as "rockbridge county"
avg.temp.lexington.city = avg.temp[which(avg.temp$county == "rockbridge county"),]
avg.temp.lexington.city$county = rep("lexington city", dim(avg.temp.lexington.city)[1])
avg.temp = rbind(avg.temp, avg.temp.lexington.city)
# Add "district of columbia" avg. temp.
avg.temp.dc = c("DC", "district of columbia", 1, 31.7)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 2, 40)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 3, 48)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 4, 54.7)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 5, 65.7)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 6, 73.3)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 7, 79)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 8, 77.8)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 9, 69.8)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 10, 55.5)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 11, 49.6)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 12, 37.8)
avg.temp = rbind(avg.temp, avg.temp.dc)
avg.temp.dc = c("DC", "district of columbia", 13, 43)
avg.temp = rbind(avg.temp, avg.temp.dc)
# Add "honolulu county" avg. temp.
avg.temp.honolulu = c("HI", "honolulu county", 1, 31.7)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 2, 40)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 3, 48)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 4, 54.7)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 5, 65.7)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 6, 73.3)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 7, 79)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 8, 77.8)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 9, 69.8)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 10, 55.5)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 11, 49.6)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 12, 37.8)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
avg.temp.honolulu = c("HI", "honolulu county", 13, 43)
avg.temp = rbind(avg.temp, avg.temp.honolulu)
# Add "maui county" avg. temp.
avg.temp.maui = c("HI", "maui county", 1, 72)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 2, 72)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 3, 73)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 4, 74)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 5, 76)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 6, 78)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 7, 79)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 8, 79)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 9, 79)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 10, 78)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 11, 76)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 12, 73)
avg.temp = rbind(avg.temp, avg.temp.maui)
avg.temp.maui = c("HI", "maui county", 13, 75)
avg.temp = rbind(avg.temp, avg.temp.maui)
# Add "kauai county" avg. temp.
avg.temp.kauai = c("HI", "kauai county", 1, 72)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 2, 72)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 3, 73)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 4, 74)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 5, 76)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 6, 78)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 7, 79)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 8, 80)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 9, 79)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 10, 78)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 11, 76)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 12, 74)
avg.temp = rbind(avg.temp, avg.temp.kauai)
avg.temp.kauai = c("HI", "kauai county", 13, 75)
avg.temp = rbind(avg.temp, avg.temp.kauai)
# Add "kalawao county" avg. temp.
avg.temp.kalawao = c("HI", "kalawao county", 1, 70)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 2, 70)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 3, 71)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 4, 72)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 5, 73)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 6, 75)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 7, 76)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 8, 76)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 9, 76)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 10, 75)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 11, 73)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 12, 71)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
avg.temp.kalawao = c("HI", "kalawao county", 13,76)
avg.temp = rbind(avg.temp, avg.temp.kalawao)
# Add "hawaii county" avg. temp.
avg.temp.hawaii = c("HI", "hawaii county", 1, 75)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 2, 75)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 3, 75)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 4, 77)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 5, 78)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 6, 79)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 7, 80)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 8, 81)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 9, 81)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 10, 80)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 11, 78)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 12, 76)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
avg.temp.hawaii = c("HI", "hawaii county", 13, 76)
avg.temp = rbind(avg.temp, avg.temp.hawaii)
# Change type
avg.temp$month.index = as.numeric(avg.temp$month.index)
avg.temp$avg.temp = as.numeric(avg.temp$avg.temp)
# Export as CSV
# You may need to change "D:\\avg_temp.csv" to your local directory
write.csv(avg.temp, "D:\\avg_temp.csv", row.names=FALSE)
# Github address:
# https://github.com/GitData-GA/iasc2023/blob/main/dataset/avg_temp.csv


#######################################################################################
########## Data 5: h5n1.cdc.clean derived from data 1 & 2 & 3 & 4 #####################
#######################################################################################
# H5N1 poultry & wild birds before January 31, 2023
h5n1.cdc = bind_rows(h5n1.poultry.cdc, h5n1.wild.cdc)
h5n1.cdc$county = tolower(h5n1.cdc$county)
h5n1.cdc$type = tolower(h5n1.cdc$type)
us.county$county = tolower(us.county$county)
months = c("2022_01", "2022_02", "2022_03", "2022_04", "2022_05", "2022_06",
           "2022_07", "2022_08", "2022_09", "2022_10", "2022_11", "2022_12",
           "2023_01")
# Replace months with index
# 1 = 2022_1, 2 = 2022_02, 3 = 2022_03, ..., 13 = 2023_01
for (i in 1:length(months)) {
  index = which(h5n1.cdc$year_month == months[i])
  h5n1.cdc$year_month[index] = i
}
# Fix county names and fips
h5n1.cdc$state[1163] = "District of Columbia"
h5n1.cdc = h5n1.cdc %>% arrange(state)
h5n1.cdc$county[c(23:24)] = "aleutians east borough"
h5n1.cdc$county[c(25:31)] = "aleutians west census area"
h5n1.cdc$county[c(32:40)] = "anchorage municipality"
h5n1.cdc$county[c(21, 41:44)] = "bethel census area"
h5n1.cdc$county[c(45)] = "chugach census area"
h5n1.cdc$county[c(46:47)] = "fairbanks north star borough"
h5n1.cdc$county[c(48)] = "haines borough"
h5n1.cdc$county[c(49:51)] = "juneau city and borough"
h5n1.cdc$county[c(52:55)] = "kenai peninsula borough"
h5n1.cdc$county[c(56:59)] = "kusilvak census area"
h5n1.cdc$county[c(17:20, 22, 60:64)] = "matanuska-susitna borough"
h5n1.cdc$county[c(65:67)] = "nome census area"
h5n1.cdc$county[c(68:73)] = "north slope borough"
h5n1.cdc$county[c(74)] = "northwest arctic borough"
h5n1.cdc$county[c(75:82)] = "sitka city and borough"
h5n1.cdc$county[c(83:87)] = "southeast fairbanks census area"
h5n1.cdc$county[c(88:89)] = "copper river census area"
h5n1.cdc$county[c(90)] = "yukon-koyukuk census area"
h5n1.cdc$county[c(494)] = "district of columbia"
h5n1.cdc$county[c(1033)] = "avoyelles parish"
h5n1.cdc$county[c(1034)] = "calcasieu parish"
h5n1.cdc$county[c(1035:1039)] = "cameron parish"
h5n1.cdc$county[c(1040)] = "catahoula parish"
h5n1.cdc$county[c(1041)] = "jefferson davis parish"
h5n1.cdc$county[c(1042:1043)] = "lasalle parish"
h5n1.cdc$county[c(1044:1046)] = "morehouse parish"
h5n1.cdc$county[c(1047)] = "ouachita parish"
h5n1.cdc$county[c(1048:1049)] = "richland parish"
h5n1.cdc$county[c(1668)] = "desoto county"
h5n1.cdc$county[c(1895:1896)] = "carson city"
h5n1.cdc$county[c(2047:2049)] = "rensselaer county"
h5n1.cdc$county[c(2864)] = "hampton city"
h5n1.cdc$county[c(2878:2879)] = "norfolk city"
h5n1.cdc$county[c(2863, 2883:2884)] = "virginia beach city"
h5n1.cdc$county[c(3008)] = "st. croix county"
# Change state name to abbreviation
for (i in 1:dim(h5n1.cdc)[1]){
  if (h5n1.cdc$state[i] != "District of Columbia"){
    abb = state.abb[grep(h5n1.cdc$state[i], state.name)]
    h5n1.cdc$state[i] = abb
  }
}
dc.index = which(h5n1.cdc$state == "District of Columbia")
h5n1.cdc$state[dc.index] = "DC"
# Only keep the month level data
options(dplyr.summarise.inform = FALSE)
h5n1.cdc = h5n1.cdc %>%
  group_by(state, county, year_month, type) %>%
  summarise(cases = sum(cases)) %>%
  rename(month.index = year_month) %>%
  left_join(us.county, by = c("state", "county")) %>%
  relocate(fips, .before = state) %>%
  relocate(lat, .after = county) %>%
  relocate(lng, .after = lat)
# Change column types to numeric
h5n1.cdc$month.index = as.numeric(h5n1.cdc$month.index)
h5n1.cdc$cases = as.numeric(h5n1.cdc$cases)
# Change types to numeric values
h5n1.cdc$type[which(h5n1.cdc$type == "wild bird")] = 1001
h5n1.cdc$type[which(h5n1.cdc$type == "non-poultry")] = 1002
h5n1.cdc$type[which(h5n1.cdc$type == "captive wild bird")] = 1003
h5n1.cdc$type[which(h5n1.cdc$type == "poultry")] = 1004
# Create unique id to fill cases
h5n1.cdc$unique.id = h5n1.cdc$fips * h5n1.cdc$lat * 
  h5n1.cdc$lng * h5n1.cdc$month.index * 
  as.numeric(h5n1.cdc$type)
h5n1.cdc = h5n1.cdc %>% relocate(unique.id, .before = fips)
h5n1.cdc.clean = do.call("rbind", replicate(13 * 4, us.county, simplify = FALSE))
h5n1.cdc.clean = h5n1.cdc.clean %>% arrange(fips)
h5n1.cdc.clean$month.index = as.numeric(rep(c(1:13), dim(h5n1.cdc.clean)[1] / 13))
h5n1.cdc.clean$type = rep(c(rep(1001, 13),
                            rep(1002, 13),
                            rep(1003, 13),
                            rep(1004, 13)), 
                          dim(h5n1.cdc.clean)[1] / 52)
h5n1.cdc.clean$unique.id = h5n1.cdc.clean$fips * h5n1.cdc.clean$lat * 
  h5n1.cdc.clean$lng * h5n1.cdc.clean$month.index * 
  as.numeric(h5n1.cdc.clean$type)
h5n1.unique.id = h5n1.cdc[,c(1, 9)]
h5n1.cdc.clean = h5n1.cdc.clean %>%
  left_join(h5n1.unique.id, by = "unique.id")
h5n1.cdc.clean$cases = na_replace(h5n1.cdc.clean$cases, 0)
h5n1.cdc.clean = h5n1.cdc.clean %>% relocate(unique.id, .before = fips)
# Change types back to characters
h5n1.cdc$type[which(h5n1.cdc$type == 1001)] = "wild bird"
h5n1.cdc$type[which(h5n1.cdc$type == 1002)] = "non-poultry"
h5n1.cdc$type[which(h5n1.cdc$type == 1003)] = "captive wild bird"
h5n1.cdc$type[which(h5n1.cdc$type == 1004)] = "poultry"
# Change types back to characters
h5n1.cdc.clean$type[which(h5n1.cdc.clean$type == 1001)] = "wild bird"
h5n1.cdc.clean$type[which(h5n1.cdc.clean$type == 1002)] = "non-poultry"
h5n1.cdc.clean$type[which(h5n1.cdc.clean$type == 1003)] = "captive wild bird"
h5n1.cdc.clean$type[which(h5n1.cdc.clean$type == 1004)] = "poultry"
# Join average temperature in each month by county
h5n1.cdc.clean = h5n1.cdc.clean %>% 
  left_join(avg.temp, by = c("state", "county", "month.index")) %>%
  relocate(avg.temp, .before = cases)
# Delete the unique id
h5n1.cdc.clean = h5n1.cdc.clean[,-1]
h5n1.cdc = h5n1.cdc[,-1]
# Add binary case for future analysis
index.one = which(h5n1.cdc.clean$cases != 0)
h5n1.cdc.clean$binary.case = rep("uninfected", dim(h5n1.cdc.clean)[1])
h5n1.cdc.clean$binary.case[index.one] = "infected"
# Export as CSV
# You may need to change "D:\\h5n1_cdc_clean.csv" to your local directory
write.csv(h5n1.cdc.clean, "D:\\h5n1_cdc_clean.csv", row.names=FALSE)
# Github address:
# https://github.com/GitData-GA/iasc2023/blob/main/dataset/h5n1_cdc_clean.csv
# Export as CSV
# You may need to change "D:\\h5n1_cdc.csv" to your local directory
write.csv(h5n1.cdc, "D:\\h5n1_cdc.csv", row.names=FALSE)
# Github address:
# https://github.com/GitData-GA/iasc2023/blob/main/dataset/h5n1_cdc.csv
