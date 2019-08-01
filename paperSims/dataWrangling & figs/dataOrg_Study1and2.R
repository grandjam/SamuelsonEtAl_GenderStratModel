library(plyr)
library(data.table)
library(reshape)
library(tidyr)
library(dplyr)

#Combine Study 2 OrgID 8001-9000 with others
load("aggCondsDatStudy2.RData")
load("aggCondsDatStudy2_9.RData")
study2<-aggCondsDat
study2_9<-aggCondsDat9
study2<-study2[order(OrgID),] 
study2lastYr <- aggregate(study2$Year, by = list(study2$OrgID), max)[,2]
study2$OrgID.temp<-c(rep(1:8000, 8*study2lastYr[1:8000]), rep(9001:10000, 8*study2lastYr[8001:9000]))
#study2[study2$OrgID %in% 8001:9000, c("OrgID", "OrgID.temp")]
study2$OrgID<-NULL
colnames(study2)[85]<-"OrgID"
study2<-rbind(study2, study2_9)

rm(study2_9)
rm(aggCondsDat)
rm(aggCondsDat9)
load("aggCondsDat.RData") #Study 1
study1<-aggCondsDat
rm(aggCondsDat)
study1$HJEffSize<-(study1$HJMean.M-study1$HJMean.F)/20
study1<-study1[,c("OrgID","Lvl","Year", "IGB", "ExGB", "HJEffSize", "Male", "LvlTO.M", "LvlTO.F", "TOTok.F", "TOLvl.M", "TOLvl.F", "TOAge.M", "TOAge.F", "TimeAtLvl.M", "TimeAtLvl.M.v", "TimeAtLvl.F", "TimeAtLvl.F.v", "Perf.M", "Perf.M.v", "Perf.F", "Perf.F.v", "Ability.M", "Ability.M.v", "Ability.F", "Ability.F.v", "Risk.M", "Risk.M.v", "Risk.F", "Risk.F.v", "Age.M", "Age.M.v", "Age.F", "Age.F.v", "YrlyHJBoost.M", "YrlyHJBoost.M.v", "YrlyHJBoost.F", "YrlyHJBoost.F.v",   "TotalPromotions.M", "TotalPromotions.M.v", "TotalPromotions.F", "TotalPromotions.F.v", "LowerExtHires.M", "LowerExtHires.F", "UpperExtHires.M", "UpperExtHires.F")]

#load("aggCondsDatStudy2.RData") #Study 2
#study2<-aggCondsDat
#rm(aggCondsDat)
study2$HJEffSize.init<-0
study2$HJEffSize<-0
study2$ExGB.init<-0
study2<-study2[,c("OrgID","Lvl","Year", "IGB", "ExGB", "ExGB.init", "HJEffSize", "HJEffSize.init", "Male", "LvlTO.M", "LvlTO.F", "TOTok.F", "TOLvl.M", "TOLvl.F", "TOAge.M", "TOAge.F", "TimeAtLvl.M", "TimeAtLvl.M.v", "TimeAtLvl.F", "TimeAtLvl.F.v", "Perf.M", "Perf.M.v", "Perf.F", "Perf.F.v", "Ability.M", "Ability.M.v", "Ability.F", "Ability.F.v", "Risk.M", "Risk.M.v", "Risk.F", "Risk.F.v", "Age.M", "Age.M.v", "Age.F", "Age.F.v", "YrlyHJBoost.M", "YrlyHJBoost.M.v", "YrlyHJBoost.F", "YrlyHJBoost.F.v",   "TotalPromotions.M", "TotalPromotions.M.v", "TotalPromotions.F", "TotalPromotions.F.v", "LowerExtHires.M", "LowerExtHires.F", "UpperExtHires.M", "UpperExtHires.F")]
#load("condsInput10k.RData")

######
#DATA#
######

#Add (initial) HJEffSize/ExGB and year 1 GB to Study 2 data set
#study1<-read.csv("Study 1 initial processed.csv")
study1<-study1[order(OrgID),]
study1HJES<-study1$HJEffSize[study1$Lvl==1 & study1$Year==1]
study1EXGB<-study1$ExGB[study1$Lvl==1 & study1$Year==1]
study2<-study2[order(OrgID),] 
study2lastYr <- aggregate(study2$Year, by = list(study2$OrgID), max)[,2]
study2$HJEffSize.init<-rep(study1HJES, 8*study2lastYr)
study2$ExGB.init<-rep(study1EXGB, 8*study2lastYr)
study2InitGB<-study2$Male[study2$Year==1]
study2$Yr1GB<-rep(study2InitGB, rep(study2lastYr, each=8))

saveRDS(study1, "Study 1 initial processed.Rds")
saveRDS(study2, "Study 2 initial processed.Rds")
#write.csv(study1, "Study 1 initial processed.csv")
#write.csv(study2, "Study 2 initial processed.csv")

##STUDY 1##
#WIDE
plotData1<-study1
plotData1$Female<-1-plotData1$Male
plotData1$TOTok.M<-0

plotData1[,c("IGB", "ExGB", "Male", "Female", "LvlTO.M", "LvlTO.F", "TOTok.M", "TOTok.F", "TOLvl.M", "TOLvl.F", "TOAge.M", "TOAge.F", "Risk.M", "Risk.F", "LowerExtHires.M", "LowerExtHires.F", "UpperExtHires.M", "UpperExtHires.F")]<-plotData1[,c("IGB", "ExGB", "Male", "Female", "LvlTO.M", "LvlTO.F", "TOTok.M", "TOTok.F", "TOLvl.M", "TOLvl.F", "TOAge.M", "TOAge.F", "Risk.M", "Risk.F", "LowerExtHires.M", "LowerExtHires.F", "UpperExtHires.M", "UpperExtHires.F")]*100
plotData1[,c("Risk.M.v", "Risk.F.v")]<-plotData1[,c("Risk.M.v", "Risk.F.v")]*10000

study1lastYr <- aggregate(study1$Year, by = list(study1$OrgID), max)[,2]
lastplotData1 <- plotData1[cumsum(rep(study1lastYr, each = length(unique(plotData1$Lvl)))),]

saveRDS(plotData1, "Study 1 Plot Data 2-26-19.Rds")
saveRDS(lastplotData1, "Study 1 Last Year Plot Data 2-26-19.Rds")

#LONG
plotData1<-readRDS("Study 1 Plot Data 2-26-19.Rds")
GB_long <- gather(plotData1[, c(1:7, 47)], Gender, Percentage, Male:Female, factor_key=TRUE)
LvlTO_long <- gather(plotData1[, c(1:6, 8:9)], Gender, LvlTO, LvlTO.M:LvlTO.F, factor_key=TRUE)
levels(LvlTO_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(GB_long, LvlTO_long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

TOTok_long <- gather(plotData1[, c(1:6, 10, 48)], Gender, TOTok, TOTok.F:TOTok.M, factor_key=TRUE)
levels(TOTok_long$Gender)<-c("Female", "Male")
plotData1.long<-merge(TOTok_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

TOLvl_long <- gather(plotData1[, c(1:6, 11:12)], Gender, TOLvl, TOLvl.M:TOLvl.F, factor_key=TRUE)
levels(TOLvl_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(TOLvl_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

TOAge_long <- gather(plotData1[, c(1:6, 13:14)], Gender, TOAge, TOAge.M:TOAge.F, factor_key=TRUE)
levels(TOAge_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(TOAge_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

TimeAtLvl_long <- gather(plotData1[, c(1:6, 15, 17)], Gender, TimeAtLvl, TimeAtLvl.M:TimeAtLvl.F, factor_key=TRUE)
levels(TimeAtLvl_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(TimeAtLvl_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))
TimeAtLvl.v_long <- gather(plotData1[, c(1:6, 16, 18)], Gender, TimeAtLvl.v, TimeAtLvl.M.v:TimeAtLvl.F.v, factor_key=TRUE)
levels(TimeAtLvl.v_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(TimeAtLvl.v_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

Perf_long <- gather(plotData1[, c(1:6, 19, 21)], Gender, Perf, Perf.M:Perf.F, factor_key=TRUE)
levels(Perf_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(Perf_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))
Perf.v_long <- gather(plotData1[, c(1:6, 20, 22)], Gender, Perf.v, Perf.M.v:Perf.F.v, factor_key=TRUE)
levels(Perf.v_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(Perf.v_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

Ability_long <- gather(plotData1[, c(1:6, 23, 25)], Gender, Ability, Ability.M:Ability.F, factor_key=TRUE)
levels(Ability_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(Ability_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))
Ability.v_long <- gather(plotData1[, c(1:6, 24, 26)], Gender, Ability.v, Ability.M.v:Ability.F.v, factor_key=TRUE)
levels(Ability.v_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(Ability.v_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

Risk_long <- gather(plotData1[, c(1:6, 27, 29)], Gender, Risk, Risk.M:Risk.F, factor_key=TRUE)
levels(Risk_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(Risk_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))
Risk.v_long <- gather(plotData1[, c(1:6, 28, 30)], Gender, Risk.v, Risk.M.v:Risk.F.v, factor_key=TRUE)
levels(Risk.v_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(Risk.v_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

Age_long <- gather(plotData1[, c(1:6, 31, 33)], Gender, Age, Age.M:Age.F, factor_key=TRUE)
levels(Age_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(Age_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))
Age.v_long <- gather(plotData1[, c(1:6, 32, 34)], Gender, Age.v, Age.M.v:Age.F.v, factor_key=TRUE)
levels(Age.v_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(Age.v_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

YrlyHJBoost_long <- gather(plotData1[, c(1:6, 35, 37)], Gender, YrlyHJBoost, YrlyHJBoost.M:YrlyHJBoost.F, factor_key=TRUE)
levels(YrlyHJBoost_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(YrlyHJBoost_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))
YrlyHJBoost.v_long <- gather(plotData1[, c(1:6, 36, 38)], Gender, YrlyHJBoost.v, YrlyHJBoost.M.v:YrlyHJBoost.F.v, factor_key=TRUE)
levels(YrlyHJBoost.v_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(YrlyHJBoost.v_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

TotalPromotions_long <- gather(plotData1[, c(1:6, 39, 41)], Gender, TotalPromotions, TotalPromotions.M:TotalPromotions.F, factor_key=TRUE)
levels(TotalPromotions_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(TotalPromotions_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))
TotalPromotions.v_long <- gather(plotData1[, c(1:6, 40, 42)], Gender, TotalPromotions.v, TotalPromotions.M.v:TotalPromotions.F.v, factor_key=TRUE)
levels(TotalPromotions.v_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(TotalPromotions.v_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

LowerExtHires_long <- gather(plotData1[, c(1:6, 43:44)], Gender, LowerExtHires, LowerExtHires.M:LowerExtHires.F, factor_key=TRUE)
levels(LowerExtHires_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(LowerExtHires_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

UpperExtHires_long <- gather(plotData1[, c(1:6, 45:46)], Gender, UpperExtHires, UpperExtHires.M:UpperExtHires.F, factor_key=TRUE)
levels(UpperExtHires_long$Gender)<-c("Male", "Female")
plotData1.long<-merge(UpperExtHires_long, plotData1.long, by=c(colnames(plotData1)[c(1:6)], "Gender"))

saveRDS(plotData1.long, "Study 1 Plot Data Long 3-5-19.Rds")

#LONG LAST
lastplotData1<-readRDS("Study 1 Last Year Plot Data 2-26-19.Rds")
GB_longLast <- gather(lastplotData1[, c(1:7, 47)], Gender, Percentage, Male:Female, factor_key=TRUE)
LvlTO_longLast <- gather(lastplotData1[, c(1:6, 8:9)], Gender, LvlTO, LvlTO.M:LvlTO.F, factor_key=TRUE)
levels(LvlTO_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(GB_longLast, LvlTO_longLast, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

TOTok_longLast <- gather(lastplotData1[, c(1:6, 10, 48)], Gender, TOTok, TOTok.F:TOTok.M, factor_key=TRUE)
levels(TOTok_longLast$Gender)<-c("Female", "Male")
lastplotData1.long<-merge(TOTok_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

TOLvl_longLast <- gather(lastplotData1[, c(1:6, 11:12)], Gender, TOLvl, TOLvl.M:TOLvl.F, factor_key=TRUE)
levels(TOLvl_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(TOLvl_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

TOAge_longLast <- gather(lastplotData1[, c(1:6, 13:14)], Gender, TOAge, TOAge.M:TOAge.F, factor_key=TRUE)
levels(TOAge_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(TOAge_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

TimeAtLvl_longLast <- gather(lastplotData1[, c(1:6, 15, 17)], Gender, TimeAtLvl, TimeAtLvl.M:TimeAtLvl.F, factor_key=TRUE)
levels(TimeAtLvl_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(TimeAtLvl_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))
TimeAtLvl.v_longLast <- gather(lastplotData1[, c(1:6, 16, 18)], Gender, TimeAtLvl.v, TimeAtLvl.M.v:TimeAtLvl.F.v, factor_key=TRUE)
levels(TimeAtLvl.v_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(TimeAtLvl.v_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

Perf_longLast <- gather(lastplotData1[, c(1:6, 19, 21)], Gender, Perf, Perf.M:Perf.F, factor_key=TRUE)
levels(Perf_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(Perf_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))
Perf.v_longLast <- gather(lastplotData1[, c(1:6, 20, 22)], Gender, Perf.v, Perf.M.v:Perf.F.v, factor_key=TRUE)
levels(Perf.v_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(Perf.v_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

Ability_longLast <- gather(lastplotData1[, c(1:6, 23, 25)], Gender, Ability, Ability.M:Ability.F, factor_key=TRUE)
levels(Ability_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(Ability_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))
Ability.v_longLast <- gather(lastplotData1[, c(1:6, 24, 26)], Gender, Ability.v, Ability.M.v:Ability.F.v, factor_key=TRUE)
levels(Ability.v_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(Ability.v_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

Risk_longLast <- gather(lastplotData1[, c(1:6, 27, 29)], Gender, Risk, Risk.M:Risk.F, factor_key=TRUE)
levels(Risk_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(Risk_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))
Risk.v_longLast <- gather(lastplotData1[, c(1:6, 28, 30)], Gender, Risk.v, Risk.M.v:Risk.F.v, factor_key=TRUE)
levels(Risk.v_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(Risk.v_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

Age_longLast <- gather(lastplotData1[, c(1:6, 31, 33)], Gender, Age, Age.M:Age.F, factor_key=TRUE)
levels(Age_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(Age_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))
Age.v_longLast <- gather(lastplotData1[, c(1:6, 32, 34)], Gender, Age.v, Age.M.v:Age.F.v, factor_key=TRUE)
levels(Age.v_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(Age.v_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

YrlyHJBoost_longLast <- gather(lastplotData1[, c(1:6, 35, 37)], Gender, YrlyHJBoost, YrlyHJBoost.M:YrlyHJBoost.F, factor_key=TRUE)
levels(YrlyHJBoost_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(YrlyHJBoost_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))
YrlyHJBoost.v_longLast <- gather(lastplotData1[, c(1:6, 36, 38)], Gender, YrlyHJBoost.v, YrlyHJBoost.M.v:YrlyHJBoost.F.v, factor_key=TRUE)
levels(YrlyHJBoost.v_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(YrlyHJBoost.v_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

TotalPromotions_longLast <- gather(lastplotData1[, c(1:6, 39, 41)], Gender, TotalPromotions, TotalPromotions.M:TotalPromotions.F, factor_key=TRUE)
levels(TotalPromotions_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(TotalPromotions_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))
TotalPromotions.v_longLast <- gather(lastplotData1[, c(1:6, 40, 42)], Gender, TotalPromotions.v, TotalPromotions.M.v:TotalPromotions.F.v, factor_key=TRUE)
levels(TotalPromotions.v_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(TotalPromotions.v_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

LowerExtHires_longLast <- gather(lastplotData1[, c(1:6, 43:44)], Gender, LowerExtHires, LowerExtHires.M:LowerExtHires.F, factor_key=TRUE)
levels(LowerExtHires_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(LowerExtHires_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

UpperExtHires_longLast <- gather(lastplotData1[, c(1:6, 45:46)], Gender, UpperExtHires, UpperExtHires.M:UpperExtHires.F, factor_key=TRUE)
levels(UpperExtHires_longLast$Gender)<-c("Male", "Female")
lastplotData1.long<-merge(UpperExtHires_longLast, lastplotData1.long, by=c(colnames(lastplotData1)[c(1:6)], "Gender"))

saveRDS(lastplotData1.long, "Study 1 Last Year Plot Data Long 3-5-19.Rds")

##STUDY 2##
#WIDE
plotData2<-study2
plotData2$Female<-1-plotData2$Male
plotData2$TOTok.M<-0
colnames(plotData2)[49]<-"Yr1Male"
plotData2$Yr1Female<-1-plotData2$Yr1Male

plotData2[,c("IGB", "ExGB", "ExGB.init", "Male", "Female", "Yr1Male", "Yr1Female", "LvlTO.M", "LvlTO.F", "TOTok.M", "TOTok.F", "TOLvl.M", "TOLvl.F", "TOAge.M", "TOAge.F", "Risk.M", "Risk.F", "LowerExtHires.M", "LowerExtHires.F", "UpperExtHires.M", "UpperExtHires.F")]<-plotData2[,c("IGB", "ExGB", "ExGB.init", "Male", "Female", "Yr1Male", "Yr1Female", "LvlTO.M", "LvlTO.F", "TOTok.M", "TOTok.F", "TOLvl.M", "TOLvl.F", "TOAge.M", "TOAge.F", "Risk.M", "Risk.F", "LowerExtHires.M", "LowerExtHires.F", "UpperExtHires.M", "UpperExtHires.F")]*100
plotData2[,c("Risk.M.v", "Risk.F.v")]<-plotData2[,c("Risk.M.v", "Risk.F.v")]*10000

lastplotData2 <- plotData2[cumsum(rep(study2lastYr, each = length(unique(plotData2$Lvl)))),]

saveRDS(plotData2, "Study 2 Plot Data 2-26-19.Rds")
saveRDS(lastplotData2, "Study 2 Last Year Plot Data 2-26-19.Rds")
#write.csv(plotData2, "Study 2 Plot Data 2-25-19.csv")
#write.csv(lastplotData2, "Study 2 Last Year Plot Data 2-25-19.csv")

#LONG
plotData2<-readRDS("Study 2 Plot Data 2-26-19.Rds")
GB_long <- gather(plotData2[, c(1:9, 50, 49, 52)], Gender, Percentage, Male:Female, factor_key=TRUE)
LvlTO_long <- gather(plotData2[, c(1:8, 49, 52, 10:11)], Gender, LvlTO, LvlTO.M:LvlTO.F, factor_key=TRUE)
levels(LvlTO_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(GB_long, LvlTO_long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

TOTok_long <- gather(plotData2[, c(1:8, 49, 52, 12, 51)], Gender, TOTok, TOTok.F:TOTok.M, factor_key=TRUE)
levels(TOTok_long$Gender)<-c("Female", "Male")
plotData2.long<-merge(TOTok_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

TOLvl_long <- gather(plotData2[, c(1:8, 49, 52, 13:14)], Gender, TOLvl, TOLvl.M:TOLvl.F, factor_key=TRUE)
levels(TOLvl_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(TOLvl_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

TOAge_long <- gather(plotData2[, c(1:8, 49, 52, 15:16)], Gender, TOAge, TOAge.M:TOAge.F, factor_key=TRUE)
levels(TOAge_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(TOAge_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

TimeAtLvl_long <- gather(plotData2[, c(1:8, 49, 52, 17, 19)], Gender, TimeAtLvl, TimeAtLvl.M:TimeAtLvl.F, factor_key=TRUE)
levels(TimeAtLvl_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(TimeAtLvl_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))
TimeAtLvl.v_long <- gather(plotData2[, c(1:8, 49, 52, 18, 20)], Gender, TimeAtLvl.v, TimeAtLvl.M.v:TimeAtLvl.F.v, factor_key=TRUE)
levels(TimeAtLvl.v_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(TimeAtLvl.v_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

Perf_long <- gather(plotData2[, c(1:8, 49, 52, 21, 23)], Gender, Perf, Perf.M:Perf.F, factor_key=TRUE)
levels(Perf_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(Perf_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))
Perf.v_long <- gather(plotData2[, c(1:8, 49, 52, 22, 24)], Gender, Perf.v, Perf.M.v:Perf.F.v, factor_key=TRUE)
levels(Perf.v_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(Perf.v_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

Ability_long <- gather(plotData2[, c(1:8, 49, 52, 25, 27)], Gender, Ability, Ability.M:Ability.F, factor_key=TRUE)
levels(Ability_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(Ability_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))
Ability.v_long <- gather(plotData2[, c(1:8, 49, 52, 26, 28)], Gender, Ability.v, Ability.M.v:Ability.F.v, factor_key=TRUE)
levels(Ability.v_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(Ability.v_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

Risk_long <- gather(plotData2[, c(1:8, 49, 52, 29, 31)], Gender, Risk, Risk.M:Risk.F, factor_key=TRUE)
levels(Risk_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(Risk_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))
Risk.v_long <- gather(plotData2[, c(1:8, 49, 52, 30, 32)], Gender, Risk.v, Risk.M.v:Risk.F.v, factor_key=TRUE)
levels(Risk.v_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(Risk.v_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

Age_long <- gather(plotData2[, c(1:8, 49, 52, 33, 35)], Gender, Age, Age.M:Age.F, factor_key=TRUE)
levels(Age_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(Age_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))
Age.v_long <- gather(plotData2[, c(1:8, 49, 52, 34, 36)], Gender, Age.v, Age.M.v:Age.F.v, factor_key=TRUE)
levels(Age.v_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(Age.v_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

YrlyHJBoost_long <- gather(plotData2[, c(1:8, 49, 52, 37, 39)], Gender, YrlyHJBoost, YrlyHJBoost.M:YrlyHJBoost.F, factor_key=TRUE)
levels(YrlyHJBoost_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(YrlyHJBoost_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))
YrlyHJBoost.v_long <- gather(plotData2[, c(1:8, 49, 52, 38, 40)], Gender, YrlyHJBoost.v, YrlyHJBoost.M.v:YrlyHJBoost.F.v, factor_key=TRUE)
levels(YrlyHJBoost.v_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(YrlyHJBoost.v_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

TotalPromotions_long <- gather(plotData2[, c(1:8, 49, 52, 41, 43)], Gender, TotalPromotions, TotalPromotions.M:TotalPromotions.F, factor_key=TRUE)
levels(TotalPromotions_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(TotalPromotions_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))
TotalPromotions.v_long <- gather(plotData2[, c(1:8, 49, 52, 42, 44)], Gender, TotalPromotions.v, TotalPromotions.M.v:TotalPromotions.F.v, factor_key=TRUE)
levels(TotalPromotions.v_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(TotalPromotions.v_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

LowerExtHires_long <- gather(plotData2[, c(1:8, 49, 52, 45:46)], Gender, LowerExtHires, LowerExtHires.M:LowerExtHires.F, factor_key=TRUE)
levels(LowerExtHires_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(LowerExtHires_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

UpperExtHires_long <- gather(plotData2[, c(1:8, 49, 52, 47:48)], Gender, UpperExtHires, UpperExtHires.M:UpperExtHires.F, factor_key=TRUE)
levels(UpperExtHires_long$Gender)<-c("Male", "Female")
plotData2.long<-merge(UpperExtHires_long, plotData2.long, by=c(colnames(plotData2)[c(1:8, 49, 52)], "Gender"))

saveRDS(plotData2.long, "Study 2 Plot Data Long 2-26-19.Rds")

#LONG LAST
lastplotData2<-readRDS("Study 2 Last Year Plot Data 2-26-19.Rds")
GB_longLast <- gather(lastplotData2[, c(1:9, 50, 49, 52)], Gender, Percentage, Male:Female, factor_key=TRUE)
LvlTO_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 10:11)], Gender, LvlTO, LvlTO.M:LvlTO.F, factor_key=TRUE)
levels(LvlTO_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(GB_longLast, LvlTO_longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

TOTok_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 12, 51)], Gender, TOTok, TOTok.F:TOTok.M, factor_key=TRUE)
levels(TOTok_longLast$Gender)<-c("Female", "Male")
lastplotData2._longLast<-merge(TOTok_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

TOLvl_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 13:14)], Gender, TOLvl, TOLvl.M:TOLvl.F, factor_key=TRUE)
levels(TOLvl_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(TOLvl_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

TOAge_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 15:16)], Gender, TOAge, TOAge.M:TOAge.F, factor_key=TRUE)
levels(TOAge_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(TOAge_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

TimeAtLvl_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 17, 19)], Gender, TimeAtLvl, TimeAtLvl.M:TimeAtLvl.F, factor_key=TRUE)
levels(TimeAtLvl_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(TimeAtLvl_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))
TimeAtLvl.v_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 18, 20)], Gender, TimeAtLvl.v, TimeAtLvl.M.v:TimeAtLvl.F.v, factor_key=TRUE)
levels(TimeAtLvl.v_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(TimeAtLvl.v_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

Perf_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 21, 23)], Gender, Perf, Perf.M:Perf.F, factor_key=TRUE)
levels(Perf_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(Perf_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))
Perf.v_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 22, 24)], Gender, Perf.v, Perf.M.v:Perf.F.v, factor_key=TRUE)
levels(Perf.v_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(Perf.v_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

Ability_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 25, 27)], Gender, Ability, Ability.M:Ability.F, factor_key=TRUE)
levels(Ability_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(Ability_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))
Ability.v_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 26, 28)], Gender, Ability.v, Ability.M.v:Ability.F.v, factor_key=TRUE)
levels(Ability.v_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(Ability.v_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

Risk_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 29, 31)], Gender, Risk, Risk.M:Risk.F, factor_key=TRUE)
levels(Risk_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(Risk_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))
Risk.v_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 30, 32)], Gender, Risk.v, Risk.M.v:Risk.F.v, factor_key=TRUE)
levels(Risk.v_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(Risk.v_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

Age_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 33, 35)], Gender, Age, Age.M:Age.F, factor_key=TRUE)
levels(Age_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(Age_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))
Age.v_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 34, 36)], Gender, Age.v, Age.M.v:Age.F.v, factor_key=TRUE)
levels(Age.v_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(Age.v_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

YrlyHJBoost_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 37, 39)], Gender, YrlyHJBoost, YrlyHJBoost.M:YrlyHJBoost.F, factor_key=TRUE)
levels(YrlyHJBoost_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(YrlyHJBoost_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))
YrlyHJBoost.v_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 38, 40)], Gender, YrlyHJBoost.v, YrlyHJBoost.M.v:YrlyHJBoost.F.v, factor_key=TRUE)
levels(YrlyHJBoost.v_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(YrlyHJBoost.v_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

TotalPromotions_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 41, 43)], Gender, TotalPromotions, TotalPromotions.M:TotalPromotions.F, factor_key=TRUE)
levels(TotalPromotions_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(TotalPromotions_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))
TotalPromotions.v_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 42, 44)], Gender, TotalPromotions.v, TotalPromotions.M.v:TotalPromotions.F.v, factor_key=TRUE)
levels(TotalPromotions.v_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(TotalPromotions.v_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

LowerExtHires_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 45:46)], Gender, LowerExtHires, LowerExtHires.M:LowerExtHires.F, factor_key=TRUE)
levels(LowerExtHires_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(LowerExtHires_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

UpperExtHires_longLast <- gather(lastplotData2[, c(1:8, 49, 52, 47:48)], Gender, UpperExtHires, UpperExtHires.M:UpperExtHires.F, factor_key=TRUE)
levels(UpperExtHires_longLast$Gender)<-c("Male", "Female")
lastplotData2._longLast<-merge(UpperExtHires_longLast, lastplotData2._longLast, by=c(colnames(lastplotData2)[c(1:8, 49, 52)], "Gender"))

saveRDS(lastplotData2._longLast, "Study 2 Last Year Plot Data Long 2-26-19.Rds")
