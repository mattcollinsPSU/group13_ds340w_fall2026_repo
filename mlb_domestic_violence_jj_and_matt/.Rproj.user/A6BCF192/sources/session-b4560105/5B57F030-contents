rm(list = ls())
mlb <- read.csv("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/mlbgames2015_2020.csv")
#colmlbaway <- read.csv("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/colaway2015_2020.csv")
#colmlb <- rbind(colmlbhome, colmlbaway)
mlb$starred <- gsub("[^[:alnum:][:blank:]+?&/\\-]","",mlb$PC)
mlb$star <- ifelse(mlb$PC == mlb$starred,0,1)

mlb$PC <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$PC))
mlb$IC <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$IC))
mlb$xIC <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$xIC))
mlb$CC <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$CC))
mlb$xCC <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$xCC))
mlb$CCAx <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$CCAx))
mlb$Acc <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$Acc))
mlb$xAcc <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$xAcc))
mlb$AAx <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$AAx))
mlb$Con <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$Con))
mlb$Fav..H. <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$Fav..H.))
mlb$totRI <- as.numeric(gsub("[][!#$%()*,:;<=>@^_`|~{}]","",mlb$totRI))
mlb$incident_date <- format(strptime(as.character(mlb$Date),"%Y-%m-%d"),"%Y-%m-%d")
summary(mlb$PC)
table(mlb$Fav..H.)
##removing ND rows -- there are only 4 of them ##
mlb <- mlb[!is.na(as.numeric(as.character(mlb$PC))),]
summary(mlb$PC)
mlb$homewin <- ifelse(mlb$R..H.>mlb$R..A.,1,0)
mlb$homeloss <- ifelse(mlb$R..H.<mlb$R..A.,1,0)
summary(mlb$homewin)
summary(mlb$homeloss)
library(lubridate)
mlb$month <- month(mlb$incident_date)
mlb$year <- year(mlb$incident_date)

#removing 2020#
mlb <- mlb[!mlb$year==2020,]

# creating summary statistics
library(modelsummary)
library(dplyr)
mlbdata<-mlb
mlbdata <- mlbdata[,-c(1,10,12,13,15,16,20)]
mlbdata <- mlbdata %>% rename_at(vars(c("R..H.","R..A.","PC","IC","CC","Acc","Con","Fav..H.","totRI")), ~ c("Runs_Home","Runs_Away","Pitches","Incorrect_Calls","Correct_Calls","Accuracy","Consistency","Favors_Home","Total_Run_Impact"))
names(mlbdata)

datasummary(Runs_Home+Runs_Away+Pitches+Incorrect_Calls+Correct_Calls+Accuracy+Consistency+Favors_Home+Total_Run_Impact ~ Mean + SD + Min + Max,
            data=mlbdata,
            title = 'Table 1')

nibrs_batch_header_2015 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_batch_headers_rds/nibrs_batch_header_2015.rds")
table(nibrs_batch_header_2015$state)
subsample <- nibrs_batch_header_2015[nibrs_batch_header_2015$state=="Colorado" & nibrs_batch_header_2015$agency_indicator=="city",]

##test<- dvcount2019[dvcount2019$ori=="CODPD0000",]
##test2<- dvcount2019[dvcount2019$ori=="CO0010000",]

mlb$homecityori <-        ifelse(mlb$Home=="KC","MOKPD0000",
                          ifelse(mlb$Home=="WSH","DCMPD0000",
                          ifelse(mlb$Home=="SF","	CA0380100",
                          ifelse(mlb$Home=="TEX","TX2200100",
                          ifelse(mlb$Home=="NYY","NY0303000",
                          ifelse(mlb$Home=="TOR","Canada",
                          ifelse(mlb$Home=="ARI","AZ0072300",
                          ifelse(mlb$Home=="LAD","CA0194200",
                          ifelse(mlb$Home=="CLE","OHCLP0000",
                          ifelse(mlb$Home=="OAK","CA0010900",
                          ifelse(mlb$Home=="MIN","MN0271100",
                          ifelse(mlb$Home=="TB","FL0521400",
                          ifelse(mlb$Home=="ATL","GAAPD0000",
                          ifelse(mlb$Home=="CWS","ILCPD0000",
                          ifelse(mlb$Home=="STL","MOSPD0000",
                          ifelse(mlb$Home=="SEA","WASPD0000",
                          ifelse(mlb$Home=="MIL","WIMPD0000",
                          ifelse(mlb$Home=="SD","CA0371100",
                          ifelse(mlb$Home=="PIT","PAPPD0000",
                          ifelse(mlb$Home=="BOS","MA0130100",
                          ifelse(mlb$Home=="CIN","OHCIP0000",
                          ifelse(mlb$Home=="NYM","NY0303000",
                          ifelse(mlb$Home=="LAA","CA0300100",
                          ifelse(mlb$Home=="BAL","MDBPD0000",
                          ifelse(mlb$Home=="DET","MI8234900",
                          ifelse(mlb$Home=="MIA","FL0130600",
                          ifelse(mlb$Home=="HOU","TXHPD0000",
                          ifelse(mlb$Home=="PHI","PAPEP0000",
                          ifelse(mlb$Home=="COL","CODPD0000",
                          ifelse(mlb$Home=="CHC","ILCPD0000",0))))))))))))))))))))))))))))))

mlb$awaycityori <-        ifelse(mlb$Away=="KC","MOKPD0000",
                                 ifelse(mlb$Away=="WSH","DCMPD0000",
                                        ifelse(mlb$Away=="SF","	CA0380100",
                                               ifelse(mlb$Away=="TEX","TX2200100",
                                                      ifelse(mlb$Away=="NYY","NY0303000",
                                                             ifelse(mlb$Away=="TOR","Canada",
                                                                    ifelse(mlb$Away=="ARI","AZ0072300",
                                                                           ifelse(mlb$Away=="LAD","CA0194200",
                                                                                  ifelse(mlb$Away=="CLE","OHCLP0000",
                                                                                         ifelse(mlb$Away=="OAK","CA0010900",
                                                                                                ifelse(mlb$Away=="MIN","MN0271100",
                                                                                                       ifelse(mlb$Away=="TB","FL0521400",
                                                                                                              ifelse(mlb$Away=="ATL","GAAPD0000",
                                                                                                                     ifelse(mlb$Away=="CWS","ILCPD0000",
                                                                                                                            ifelse(mlb$Away=="STL","MOSPD0000",
                                                                                                                                   ifelse(mlb$Away=="SEA","WASPD0000",
                                                                                                                                          ifelse(mlb$Away=="MIL","WIMPD0000",
                                                                                                                                                 ifelse(mlb$Away=="SD","CA0371100",
                                                                                                                                                        ifelse(mlb$Away=="PIT","PAPPD0000",
                                                                                                                                                               ifelse(mlb$Away=="BOS","MA0130100",
                                                                                                                                                                      ifelse(mlb$Away=="CIN","OHCIP0000",
                                                                                                                                                                             ifelse(mlb$Away=="NYM","NY0303000",
                                                                                                                                                                                    ifelse(mlb$Away=="LAA","CA0300100",
                                                                                                                                                                                           ifelse(mlb$Away=="BAL","MDBPD0000",
                                                                                                                                                                                                  ifelse(mlb$Away=="DET","MI8234900",
                                                                                                                                                                                                         ifelse(mlb$Away=="MIA","FL0130600",
                                                                                                                                                                                                                ifelse(mlb$Away=="HOU","TXHPD0000",
                                                                                                                                                                                                                       ifelse(mlb$Away=="PHI","PAPEP0000",
                                                                                                                                                                                                                              ifelse(mlb$Away=="COL","CODPD0000",
                                                                                                                                                                                                                                     ifelse(mlb$Away=="CHC","ILCPD0000",0))))))))))))))))))))))))))))))




homedvcount2015 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2015.rds")
colnames(homedvcount2015)[1] = "incident_date"
colnames(homedvcount2015)[2] = "ori"
colnames(homedvcount2015)[3] = "homeoridvcount"
homedvcount2016 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2016.rds")
colnames(homedvcount2016)[1] = "incident_date"
colnames(homedvcount2016)[2] = "ori"
colnames(homedvcount2016)[3] = "homeoridvcount"
homedvcount2017 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2017.rds")
colnames(homedvcount2017)[1] = "incident_date"
colnames(homedvcount2017)[2] = "ori"
colnames(homedvcount2017)[3] = "homeoridvcount"
homedvcount2018 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2018.rds")
colnames(homedvcount2018)[1] = "incident_date"
colnames(homedvcount2018)[2] = "ori"
colnames(homedvcount2018)[3] = "homeoridvcount"
homedvcount2019 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2019.rds")
colnames(homedvcount2019)[1] = "incident_date"
colnames(homedvcount2019)[2] = "ori"
colnames(homedvcount2019)[3] = "homeoridvcount"
homedvcount2020 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2020.rds")
colnames(homedvcount2020)[1] = "incident_date"
colnames(homedvcount2020)[2] = "ori"
colnames(homedvcount2020)[3] = "homeoridvcount"
awaydvcount2015 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2015.rds")
colnames(awaydvcount2015)[1] = "incident_date"
colnames(awaydvcount2015)[2] = "ori"
colnames(awaydvcount2015)[3] = "awayoridvcount"
awaydvcount2016 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2016.rds")
colnames(awaydvcount2016)[1] = "incident_date"
colnames(awaydvcount2016)[2] = "ori"
colnames(awaydvcount2016)[3] = "awayoridvcount"
awaydvcount2017 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2017.rds")
colnames(awaydvcount2017)[1] = "incident_date"
colnames(awaydvcount2017)[2] = "ori"
colnames(awaydvcount2017)[3] = "awayoridvcount"
awaydvcount2018 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2018.rds")
colnames(awaydvcount2018)[1] = "incident_date"
colnames(awaydvcount2018)[2] = "ori"
colnames(awaydvcount2018)[3] = "awayoridvcount"
awaydvcount2019 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2019.rds")
colnames(awaydvcount2019)[1] = "incident_date"
colnames(awaydvcount2019)[2] = "ori"
colnames(awaydvcount2019)[3] = "awayoridvcount"
awaydvcount2020 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2020.rds")
colnames(awaydvcount2020)[1] = "incident_date"
colnames(awaydvcount2020)[2] = "ori"
colnames(awaydvcount2020)[3] = "awayoridvcount"

homedvcount<- rbind(homedvcount2015,homedvcount2016,homedvcount2017,homedvcount2018,homedvcount2019)
table2 <- homedvcount[homedvcount$ori=="OHCIP0000"|homedvcount$ori=="OHCLP0000"|homedvcount$ori=="CODPD0000"|homedvcount$ori=="MI8234900"|homedvcount$ori=="MOKPD0000"|homedvcount$ori=="WIMPD0000"|homedvcount$ori=="WASPD0000",]
table2 <- table2[!table2$incident_date==0,]
table2 <- table2[,-4]
colnames(table2)[1] = "incident_date"
colnames(table2)[2] = "ori"
colnames(table2)[3] = "Domestic_Violence"
table2$Team <- ifelse(table2$ori=="OHCIP0000","CIN",
                    ifelse(table2$ori=="MOKPD0000","KC",
                           ifelse(table2$ori=="DCMPD0000","WSH",
                                  ifelse(table2$ori=="	CA0380100","SF",
                                         ifelse(table2$ori=="TX2200100","TEX",
                                                ifelse(table2$ori=="NY0303000","NYY",
                                                       ifelse(table2$ori=="Canada","TOR",
                                                              ifelse(table2$ori=="AZ0072300","ARI",
                                                                     ifelse(table2$ori=="CA0194200","LAD",
                                                                            ifelse(table2$ori=="OHCLP0000","CLE",
                                                                                   ifelse(table2$ori=="CA0010900","OAK",
                                                                                          ifelse(table2$ori=="MN0271100","MIN",
                                                                                                 ifelse(table2$ori=="FL0521400","TB",
                                                                                                        ifelse(table2$ori=="GAAPD0000","ATL",
                                                                                                               ifelse(table2$ori=="ILCPD0000","CWS",
                                                                                                                      ifelse(table2$ori=="MOSPD0000","STL",
                                                                                                                             ifelse(table2$ori=="WASPD0000","SEA",
                                                                                                                                    ifelse(table2$ori=="WIMPD0000","MIL",
                                                                                                                                           ifelse(table2$ori=="CA0371100","SD",
                                                                                                                                                  ifelse(table2$ori=="PAPPD0000","PIT",
                                                                                                                                                         ifelse(table2$ori=="MA0130100","BOS",
                                                                                                                                                                ifelse(table2$ori=="OHCIP0000","CIN",
                                                                                                                                                                       ifelse(table2$ori=="NY0303000","NYM",
                                                                                                                                                                              ifelse(table2$ori=="CA0300100","LAA",
                                                                                                                                                                                     ifelse(table2$ori=="MDBPD0000","BAL",
                                                                                                                                                                                            ifelse(table2$ori=="MI8234900","DET",
                                                                                                                                                                                                   ifelse(table2$ori=="FL0130600","MIA",
                                                                                                                                                                                                          ifelse(table2$ori=="TXHPD0000","HOU",
                                                                                                                                                                                                                 ifelse(table2$ori=="PAPEP0000","PHI",
                                                                                                                                                                                                                        ifelse(table2$ori=="CODPD0000","COL",
                                                                                                                                                                                                                               ifelse(table2$ori=="ILCPD0000","CHC",0)))))))))))))))))))))))))))))))
datasummary_skim(table2)
datasummary(Domestic_Violence*Team~Mean+SD+Min+Max+Median,
            data=table2,
            title = "Table 2")

homedvcount2015$homecityori <- homedvcount2015$ori
awaydvcount2015$awaycityori <- awaydvcount2015$ori
homedvcount2016$homecityori <- homedvcount2016$ori
awaydvcount2016$awaycityori <- awaydvcount2016$ori
homedvcount2017$homecityori <- homedvcount2017$ori
awaydvcount2017$awaycityori <- awaydvcount2017$ori
homedvcount2018$homecityori <- homedvcount2018$ori
awaydvcount2018$awaycityori <- awaydvcount2018$ori
homedvcount2019$homecityori <- homedvcount2019$ori
awaydvcount2019$awaycityori <- awaydvcount2019$ori
homedvcount2020$homecityori <- homedvcount2020$ori
awaydvcount2020$awaycityori <- awaydvcount2020$ori

mlb2015<-mlb[mlb$incident_date >= "2015-01-01" & mlb$incident_date <= "2015-12-31",]
mlb2016<-mlb[mlb$incident_date >= "2016-01-01" & mlb$incident_date <= "2016-12-31",]
mlb2017<-mlb[mlb$incident_date >= "2017-01-01" & mlb$incident_date <= "2017-12-31",]
mlb2018<-mlb[mlb$incident_date >= "2018-01-01" & mlb$incident_date <= "2018-12-31",]
mlb2019<-mlb[mlb$incident_date >= "2019-01-01" & mlb$incident_date <= "2019-12-31",]
mlb2020<-mlb[mlb$incident_date >= "2020-01-01" & mlb$incident_date <= "2020-12-31",]

merge1 <- merge(mlb2015, homedvcount2015, by = c("incident_date","homecityori"), all.x = TRUE)
merge1 <- merge(merge1, awaydvcount2015, by = c("incident_date","awaycityori"), all.x = TRUE)
merge1home<- merge1[complete.cases(merge1[,c("homeoridvcount")]),]
merge1away<- merge1[complete.cases(merge1[,c("awayoridvcount")]),]
merge1complete<- merge1[complete.cases(merge1[,c("homeoridvcount", "awayoridvcount")]),]
merge1complete$dvcount<- merge1complete$homeoridvcount+merge1complete$awayoridvcount

merge2 <- merge(mlb2016, homedvcount2016, by = c("incident_date","homecityori"), all.x = TRUE)
merge2 <- merge(merge2, awaydvcount2016, by = c("incident_date","awaycityori"), all.x = TRUE)
merge2home<- merge2[complete.cases(merge2[,c("homeoridvcount")]),]
merge2away<- merge2[complete.cases(merge2[,c("awayoridvcount")]),]
merge2complete<- merge2[complete.cases(merge2[,c("homeoridvcount", "awayoridvcount")]),]
merge2complete$dvcount<- merge2complete$homeoridvcount+merge2complete$awayoridvcount

merge3 <- merge(mlb2017, homedvcount2017, by = c("incident_date","homecityori"), all.x = TRUE)
merge3 <- merge(merge3, awaydvcount2017, by = c("incident_date","awaycityori"), all.x = TRUE)
merge3home<- merge3[complete.cases(merge3[,c("homeoridvcount")]),]
merge3away<- merge3[complete.cases(merge3[,c("awayoridvcount")]),]
merge3complete<- merge3[complete.cases(merge3[,c("homeoridvcount", "awayoridvcount")]),]
merge3complete$dvcount<- merge3complete$homeoridvcount+merge3complete$awayoridvcount

merge4 <- merge(mlb2018, homedvcount2018, by = c("incident_date","homecityori"), all.x = TRUE)
merge4 <- merge(merge4, awaydvcount2018, by = c("incident_date","awaycityori"), all.x = TRUE)
merge4home<- merge4[complete.cases(merge4[,c("homeoridvcount")]),]
merge4away<- merge4[complete.cases(merge4[,c("awayoridvcount")]),]
merge4complete<- merge4[complete.cases(merge4[,c("homeoridvcount", "awayoridvcount")]),]
merge4complete$dvcount<- merge4complete$homeoridvcount+merge4complete$awayoridvcount

merge5 <- merge(mlb2019, homedvcount2019, by = c("incident_date","homecityori"), all.x = TRUE)
merge5 <- merge(merge5, awaydvcount2019, by = c("incident_date","awaycityori"), all.x = TRUE)
merge5home<- merge5[complete.cases(merge5[,c("homeoridvcount")]),]
merge5away<- merge5[complete.cases(merge5[,c("awayoridvcount")]),]
merge5complete<- merge5[complete.cases(merge5[,c("homeoridvcount", "awayoridvcount")]),]
merge5complete$dvcount<- merge5complete$homeoridvcount+merge5complete$awayoridvcount

merge6 <- merge(mlb2020, homedvcount2020, by = c("incident_date","homecityori"), all.x = TRUE)
merge6 <- merge(merge6, awaydvcount2020, by = c("incident_date","awaycityori"), all.x = TRUE)
merge6home<- merge6[complete.cases(merge6[,c("homeoridvcount")]),]
merge6away<- merge6[complete.cases(merge6[,c("awayoridvcount")]),]
merge6complete<- merge6[complete.cases(merge6[,c("homeoridvcount", "awayoridvcount")]),]
merge6complete$dvcount<- merge6complete$homeoridvcount+merge6complete$awayoridvcount

table(merge1home$Home)
table(merge1away$Away)
table(merge2home$Home)
table(merge2away$Away)
table(merge3home$Home)
table(merge3away$Away)
table(merge4home$Home)
table(merge4away$Away)
table(merge5home$Home)
table(merge5away$Away)

mlbtest <- rbind(merge1,merge2,merge3,merge4,merge5)
mlbtest <- mlbtest[,!(names(mlbtest) %in% c("ori.x","ori.y","X","starred"))]

datasummary_skim(mlbtest)


mlbhome<- rbind(merge1home,merge2home,merge3home,merge4home,merge5home,merge6home)
mlbaway<- rbind(merge1away,merge2away,merge3away,merge4away,merge5away,merge6away)
mlbcomplete<- rbind(merge1complete,merge2complete,merge3complete,merge4complete,merge5complete,merge6complete)

drops <- c("ori.x","ori.y","X")
mlbhome <- mlbhome[,!(names(mlbhome) %in% drops)]
mlbaway <- mlbaway[,!(names(mlbaway) %in% drops)]
mlbcomplete <- mlbcomplete[,!(names(mlbcomplete) %in% drops)]

##dropping atl because they have zeros for DV##
mlbhome<- mlbhome[!mlbhome$Home=="ATL",]
mlbaway<- mlbaway[!mlbaway$Away=="ATL",]
mlbcomplete<- mlbcomplete[!mlbcomplete$Home=="ATL",]
mlbcomplete<- mlbcomplete[!mlbcomplete$Away=="ATL",]

saveRDS(mlbhome,"C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/mlbhome.rds")
saveRDS(mlbaway,"C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/mlbaway.rds")
saveRDS(mlbcomplete,"C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/mlbcomplete.rds")



######################################### everything below this is previous work ####################################
table(merge4complete$dvcount)
hist(merge3complete$dvcount)



merge1 <- merge(merge1, dvcount2018, by = "incident_date", all.x = TRUE)
merge1 <- merge(merge1, dvcount2017, by = "incident_date", all.x = TRUE)
merge1$oridvcount2018[is.na(merge1$oridvcount2018)] <- 0
merge1$oridvcount2019[is.na(merge1$oridvcount2019)] <- 0
merge1$oridvcount2019[is.na(merge1$oridvcount2017)] <- 0
merge1$dvcount <- as.numeric(merge1$oridvcount2018 + merge1$oridvcount2019)
test1 <- merge1[merge1$Home == "COL" & merge1$incident_date >= "2018-01-01" & merge1$incident_date <= "2019-12-31"|merge1$Away == "COL" & merge1$incident_date >= "2018-01-01" & merge1$incident_date <= "2019-12-31",]
test1$month <- as.character(month(test1$incident_date))
test1$year <- as.character(year(test1$incident_date))
test1subset <- test1[test1$month==8,]
summary(lm(dvcount~Acc, data = test1subset))
coplot(dvcount~Acc|month, data=test1)


library(lubridate)
merge1$month <- month(merge1$incident_date)
test1 <- merge1[merge1$Home == "COL" & merge1$incident_date >= "2019-01-01" & merge1$incident_date <= "2019-12-31"| merge1$Away == "COL" & merge1$incident_date >= "2019-01-01" & merge1$incident_date <= "2019-12-31",]
test1 <- merge1[merge1$Home == "COL" & merge1$incident_date >= "2019-01-01" & merge1$incident_date <= "2019-12-31",]
coplot(oridvcount2019~Con|month,data=test1)
plot(oridvcount2019~Acc+Con+Fav..H.+totRI, data=test1)
table(test1$month)
table(test1$oridvcount2019,test1$month)

test1$month <- as.character(test1$month)
summary(lm(oridvcount2019~Acc+Con+Fav..H.+month+win, data=test1))


library(ggplot2)
ggplot(test1,aes(x=Acc, y=dvcount)) + geom_point()
ggplot(test1,aes(x=Con, y=dvcount)) + geom_point()
ggplot(test1,aes(x=Fav..H., y=dvcount)) + geom_point()
ggplot(test1, aes(win, dvcount)) + geom_point()
ggplot(test1, aes(Fav..H., win)) + geom_point()
table(test1$Fav..H.)


postseason2015_2020 <- read.csv("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/postseason2015_2020.csv")
## calling the date incident date to make merging easier##
postseason2015_2020$incident_date <- format(strptime(as.character(postseason2015_2020$Date),"%m/%d/%Y"), "%Y-%m-%d")
##changing starred rows to numerical but creating an indicator for starred##
postseason2015_2020$starred <- gsub("[^[:alnum:][:blank:]+?&/\\-]","",postseason2015_2020$PC)
postseason2015_2020$star <- ifelse(postseason2015_2020$PC == postseason2015_2020$starred,0,1)
postseason2015_2020$PC <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$PC))
postseason2015_2020$IC <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$IC))
postseason2015_2020$xIC <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$xIC))
postseason2015_2020$CC <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$CC))
postseason2015_2020$xCC <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$xCC))
postseason2015_2020$CCAx <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$CCAx))
postseason2015_2020$Acc <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$Acc))
postseason2015_2020$xAcc <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$xAcc))
postseason2015_2020$AAx <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$AAx))
postseason2015_2020$Con <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$Con))
postseason2015_2020$Fav..H. <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$Fav..H.))
postseason2015_2020$totRI <- as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-\\.]","",postseason2015_2020$totRI))
##removing ND rows##
postseason2015_2020 <- postseason2015_2020[!is.na(as.numeric(as.character(postseason2015_2020$PC))),]
summary(postseason2015_2020)
table(postseason2015_2020$Acc)
table(postseason2015_2020$Home)
table(postseason2015_2020$Away)
mlbhometeams <- read.csv("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/mlbhometeams.csv")
colnames(mlbhometeams)[1] = "Home"
mlbawayteams <- read.csv("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/mlbawayteams.csv")
colnames(mlbawayteams)[1] = "Away"
postseason2015_2020 <- merge(postseason2015_2020, mlbhometeams, by = "Home")
postseason2015_2020 <- merge(postseason2015_2020, mlbawayteams, by = "Away")
##postseason2015_2020$homestate <- ifelse(postseason2015_2020$Home == "ARI", "arizona", ifelse(postseason2015_2020$Home == "ATL", "georgia", ifelse(postseason2015_2020$Home == "BOS", "massachusetts", ifelse(postseason2015_2020$Home == "CHC", "illinois", ifelse(postseason2015_2020$Home == "CLE", "ohio", ifelse(postseason2015_2020$Home == "COL", "colorado", ifelse(postseason2015_2020$Home=="HOU", "texas",ifelse(postseason2015_2020$Home=="KC","missouri",ifelse(postseason2015_2020$Home=="LAD","california",ifelse(postseason2015_2020$Home=="MIL","wisconsin",ifelse(postseason2015_2020$Home=="MIN","minnesota",ifelse(postseason2015_2020$Home=="NYM","new york",ifelse(postseason2015_2020$Home=="NYY","new york",ifelse(postseason2015_2020$Home=="OAK","california",ifelse(postseason2015_2020$Home=="PIT","pennsylvania",ifelse(postseason2015_2020$Home=="SF","california",ifelse(postseason2015_2020$Home=="STL","missouri",ifelse(postseason2015_2020$Home=="TB","florida",ifelse(postseason2015_2020$Home=="TEX","texas",ifelse(postseason2015_2020$Home=="TOR","canada",ifelse(postseason2015_2020$Home=="WSH","district of columbia",0)))))))))))))))))))))
##postseason2015_2020$awaystate <- ifelse(postseason2015_2020$Away == "ARI", "arizona", ifelse(postseason2015_2020$Away == "ATL", "georgia", ifelse(postseason2015_2020$Away == "BOS", "massachusetts", ifelse(postseason2015_2020$Away == "CHC", "illinois", ifelse(postseason2015_2020$Away == "CLE", "ohio", ifelse(postseason2015_2020$Away == "COL", "colorado", ifelse(postseason2015_2020$Away=="HOU", "texas",ifelse(postseason2015_2020$Away=="KC","missouri",ifelse(postseason2015_2020$Away=="LAD","california",ifelse(postseason2015_2020$Away=="MIL","wisconsin",ifelse(postseason2015_2020$Away=="MIN","minnesota",ifelse(postseason2015_2020$Away=="NYM","new york",ifelse(postseason2015_2020$Away=="NYY","new york",ifelse(postseason2015_2020$Away=="OAK","california",ifelse(postseason2015_2020$Away=="BAL","maryland",ifelse(postseason2015_2020$Away=="SF","california",ifelse(postseason2015_2020$Away=="STL","missouri",ifelse(postseason2015_2020$Away=="TB","florida",ifelse(postseason2015_2020$Away=="TEX","texas",ifelse(postseason2015_2020$Away=="TOR","canada",ifelse(postseason2015_2020$Away=="WSH","district of columbia",0)))))))))))))))))))))


##putting the mlb data to the dv data ##
workingdata2 <- merge(postseason2015_2020, dvcounthome2015, by = c("incident_date","homestate"), all.x=TRUE)
workingdata3 <- merge(workingdata2, dvcountaway2015, by = c("incident_date","awaystate"), all.x=TRUE)

table(nibrs_batch_header_2015$state)
test <- nibrs_batch_header_2015[nibrs_batch_header_2015$ori == "AZ0072300",]
table(test$ori)


coplot(oridvcount2019~Acc|Month)