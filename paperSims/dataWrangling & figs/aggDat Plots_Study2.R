library(ggplot2)
library(viridis)
library(tidyr)

#######
#PLOTS#
#######

plotData2<-readRDS("Study 2 Plot Data 2-26-19.Rds")
lastplotData2<-readRDS("Study 2 Last Year Plot Data 2-26-19.Rds")
plotData2.long<-readRDS("Study 2 Plot Data Long 2-26-19.Rds")
lastplotData2.long<-readRDS("Study 2 Last Year Plot Data Long 2-26-19.Rds")
plotData2.long$OrgID.Gender<-paste(plotData2.long$OrgID, plotData2.long$Gender)
lastplotData2.long$OrgID.Gender<-paste(lastplotData2.long$OrgID, lastplotData2.long$Gender)

level_names <- c('1'="Level 1", '2'="Level 2", '3'="Level 3", '4'="Level 4", '5'="Level 5", '6'="Level 6", '7'="Level 7")
year_names <- c('1'="Year 1", '20'="Year 20", '40'="Year 40")
IGB_names <- c('1'="Gender Balanced", '2'="Slightly Male Dominated", '3'="Moderately Male Dominated", '4'="Extremely Male Dominated")

#PLOTS IN REVISION#
#1.	Time on x axis, color=IGB (org-level gender strat).
png(filename="MD Female percent x IGB.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2[plotData2$Lvl<8 & plotData2$IGB>=49,], aes(x = Year, y = Female, group=OrgID)) +
  geom_line(aes(color=IGB), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis", breaks=c(50, 85), limits=c(50, 85), labels=c("50% Male", "85% Male")) +
  scale_y_continuous(limits=c(0, 75), breaks=c(0, 25, 50, 75)) +
  scale_x_continuous(limits=c(0, 45), breaks=c(1, seq(15, 45, by=15))) +
  geom_hline(yintercept=50, color="red3") +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Percentage Female", color="Initial Organization \n Gender Breakdown") +
  theme(legend.title = element_text(size=7), legend.text=element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

mean(lastplotData2$Female[lastplotData2$IGB>=49 & lastplotData2$Lvl<8])
min(lastplotData2$Female[lastplotData2$IGB>=49 & lastplotData2$Lvl<8])
tail(sort(lastplotData2$Female[lastplotData2$IGB>=49 & lastplotData2$Lvl<8], TRUE), 25)
sd(lastplotData2$Female[lastplotData2$IGB>=49 & lastplotData2$Lvl<8])

#Time to reach parity
(length(unique(plotData2[plotData2$Female>=49 & plotData2$Female<=51 & plotData2$IGB>=50 & plotData2$Lvl<8, "OrgID"]))/max(plotData2$OrgID))*100
(nrow(unique(plotData2[plotData2$Female>=49 & plotData2$Female<=51 & plotData2$IGB>=50 & plotData2$Lvl<8, c("OrgID", "Lvl")]))/(max(plotData2$OrgID)*7))*100
(length(unique(plotData2[plotData2$Female>=49 & plotData2$Female<=51 & plotData2$IGB>=50 & plotData2$Lvl==7, "OrgID"]))/max(plotData2$OrgID))*100

plotData2$Parity<-ifelse(plotData2$Female>=49 & plotData2$Female<=51, 1, 0)
study2YrParity<-aggregate(Year~OrgID+Lvl, data=plotData2[plotData2$Parity==1,], min)
parityplotData2<-inner_join(plotData2, study2YrParity, by=c("OrgID", "Lvl", "Year"))

length(unique(parityplotData2$OrgID[parityplotData2$IGB>=50 & parityplotData2$Lvl<8]))/(10000)
length(unique(parityplotData2$OrgID[parityplotData2$IGB>=50 & parityplotData2$Lvl==7]))/(10000)
mean(parityplotData2$Year[parityplotData2$IGB>=50 & parityplotData2$Lvl==7])
sd(parityplotData2$Year[parityplotData2$IGB>=50 & parityplotData2$Lvl==7])
cor(parityplotData2$IGB[parityplotData2$IGB>=50 & parityplotData2$Lvl<8], parityplotData2$Year[parityplotData2$IGB>=50 & parityplotData2$Lvl<8])
cor(parityplotData2$IGB[parityplotData2$IGB>=50 & parityplotData2$Lvl %in% c(5:7)], parityplotData2$Year[parityplotData2$IGB>=50 & parityplotData2$Lvl%in% c(5:7)])

#Average % female in "stable years"
#mean(parityplotData2$Year[parityplotData2$IGB>=50 & parityplotData2$Lvl<8])
#aggregate(Year~Lvl, data=parityplotData2[parityplotData2$IGB>=50,], mean)
#Lvl     Year
#1 15.42674
#2 26.87908
#3 29.40072
#4 29.55004
#5 29.97379
#6 29.24222
#7 25.60734
#aggregate(Year~Lvl, data=parityplotData2[parityplotData2$IGB>=50,], sd)

#aggregate(Female~Lvl, data=plotData2[plotData2$Year>25 & plotData2$Lvl<8 & plotData2$IGB>=50,], mean)
#1 50.42851
#2 48.90167
#3 48.33586
#4 48.05455
#5 46.94429
#6 46.57940
#7 46.29819
#aggregate(Female~Lvl, data=plotData2[plotData2$Year>25 & plotData2$Lvl<8 & plotData2$IGB>=50,], sd)
#Lvl     Female
#1 0.2944795
#2 0.4332380
#3 0.6583577
#4 1.0923350
#5 1.5760322
#6 2.2733106
#7 5.0047306

#3. Promotions of men and women over time
png(filename="MD Promotions.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl %in% c(2:5) & plotData2.long$IGB>=49,], aes(x = Year, y = TotalPromotions, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), linetype=2) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  scale_x_continuous(limits=c(0, 45), breaks=c(1, seq(15, 45, by=15))) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Total Promotions Earned", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

mean((plotData2$TotalPromotions.M[plotData2$Year==1 & plotData2$IGB>=49 & plotData2$Lvl==5]-plotData2$TotalPromotions.F[plotData2$Year==1 & plotData2$IGB>=49 & plotData2$Lvl==5]), na.rm=T)
sd((plotData2$TotalPromotions.M[plotData2$Year==1 & plotData2$IGB>=49 & plotData2$Lvl==5]-plotData2$TotalPromotions.F[plotData2$Year==1 & plotData2$IGB>=49 & plotData2$Lvl==5]), na.rm=T)
mean((plotData2$TotalPromotions.M[plotData2$Year==40 & plotData2$IGB>=49 & plotData2$Lvl==5]-plotData2$TotalPromotions.F[plotData2$Year==40 & plotData2$IGB>=49 & plotData2$Lvl==5]), na.rm=T)
sd((plotData2$TotalPromotions.M[plotData2$Year==40 & plotData2$IGB>=49 & plotData2$Lvl==5]-plotData2$TotalPromotions.F[plotData2$Year==40 & plotData2$IGB>=49 & plotData2$Lvl==5]), na.rm=T)

sd(plotData2$TotalPromotions.F[plotData2$Year==1 & plotData2$IGB>=49 & plotData2$Lvl==5], na.rm=T)
sd(plotData2$TotalPromotions.F[plotData2$Year==40 & plotData2$IGB>=49 & plotData2$Lvl==5], na.rm=T)

#Overlapping density distributions
png(filename="MD Promotions Distributions.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl==5 & plotData2.long$IGB>=49 & plotData2.long$Year %in% c(1, 20, 40),], aes(x = TotalPromotions, fill=Gender, color=Gender)) +
  geom_density(aes(group=Gender), alpha=.7) +
  theme_minimal() +
  scale_fill_manual(values = c("#3288bd", "#d53e4f")) +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Year, nrow = 3, labeller = labeller(Year=year_names)) +
  labs(x = "Total Promotions Earned", y = "Density", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

#mean(plotData2.long$TotalPromotions[plotData2.long$Gender=="Female" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==1], na.rm=T)
#mean(plotData2.long$TotalPromotions[plotData2.long$Gender=="Female" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==20], na.rm=T)
#mean(plotData2.long$TotalPromotions[plotData2.long$Gender=="Female" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==40], na.rm=T)
#sd(plotData2.long$TotalPromotions[plotData2.long$Gender=="Female" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==1], na.rm=T)
#sd(plotData2.long$TotalPromotions[plotData2.long$Gender=="Female" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==20], na.rm=T)
#sd(plotData2.long$TotalPromotions[plotData2.long$Gender=="Female" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==40], na.rm=T)

#mean(plotData2.long$TotalPromotions[plotData2.long$Gender=="Male" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==1], na.rm=T)
#mean(plotData2.long$TotalPromotions[plotData2.long$Gender=="Male" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==20], na.rm=T)
#mean(plotData2.long$TotalPromotions[plotData2.long$Gender=="Male" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==40], na.rm=T)
#sd(plotData2.long$TotalPromotions[plotData2.long$Gender=="Male" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==1], na.rm=T)
#sd(plotData2.long$TotalPromotions[plotData2.long$Gender=="Male" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==20], na.rm=T)
#sd(plotData2.long$TotalPromotions[plotData2.long$Gender=="Male" & plotData2.long$IGB>=50 & plotData2.long$Lvl==5 & plotData2.long$Year==40], na.rm=T)

#2. Turnover of women over time, year 1 female as color
png(filename="MD Female Level TO x IGB.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female" & plotData2.long$IGB>=49,], aes(x = Year, y = LvlTO, group=OrgID)) +
  geom_line(aes(color=IGB), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis", breaks=c(50, 85), limits=c(50, 85), labels=c("50% Male", "85% Male")) + 
  scale_x_continuous(limits=c(0, 45), breaks=c(1, seq(15, 45, by=15))) +
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Turnover %", color="Initial Organization \n Gender Breakdown") +
  theme(legend.title = element_text(size=7), legend.text=element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

LvlTO.mean<-aggregate(LvlTO.inv~Year+Lvl+Gender, data=plotData2.long[plotData2.long$IGB>=49 & plotData2.long$Year %in% c(1, 20, 40),], mean)
colnames(LvlTO.mean)[4]<-"LvlTO.mean"
LvlTO.se<-aggregate(LvlTO.inv~Year+Lvl+Gender, data=plotData2.long[plotData2.long$IGB>=49 & plotData2.long$Year %in% c(1, 20, 40),], FUN = function(y){sd(y)/sqrt(length(y))})
colnames(LvlTO.se)[4]<-"LvlTO.se"
LvlTO.plotmeans<-merge(LvlTO.mean, LvlTO.se)

png(filename="MD Level TO Lvl 7.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(LvlTO.plotmeans[LvlTO.plotmeans$Lvl==7,], aes(x = Year, y = LvlTO.mean, fill=Gender)) +
  geom_bar(stat="identity", position="identity") + 
  geom_errorbar(aes(ymin=LvlTO.mean-LvlTO.se, ymax=LvlTO.mean+LvlTO.se), width=2, position=position_dodge(1), size=.25) +
  theme_minimal() +
  scale_fill_manual(values = c("#3288bd", "#d53e4f")) +
  coord_flip() +
  scale_x_reverse(limits=c(50, -10), breaks=c(40, 20, 1)) +
  scale_y_continuous(limits=c(-20, 20), breaks=c(-20, -10, 0, 10, 20), labels=c(20, 10, 0, 10, 20)) +
  facet_wrap(~Lvl, nrow = 3, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Turnover Rate (%)", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

mean(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Year==1 & plotData2.long$IGB>=50 & plotData2.long$MD>1 & plotData2.long$Lvl==7], na.rm=T)
sd(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Year==1 & plotData2.long$IGB>=50 & plotData2.long$MD>1 & plotData2.long$Lvl==7], na.rm=T)
mean(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Year==1 & plotData2.long$IGB>=50 & plotData2.long$MD==1 & plotData2.long$Lvl==7], na.rm=T)
sd(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Year==1 & plotData2.long$IGB>=50 & plotData2.long$MD==1 & plotData2.long$Lvl==7], na.rm=T)

mean(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Year==40 & plotData2.long$IGB>=50 & plotData2.long$MD>1 & plotData2.long$Lvl==7], na.rm=T)
sd(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Year==40 & plotData2.long$IGB>=50 & plotData2.long$MD>1 & plotData2.long$Lvl==7], na.rm=T)
mean(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Year==40 & plotData2.long$IGB>=50 & plotData2.long$MD==1 & plotData2.long$Lvl==7], na.rm=T)
sd(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Year==40 & plotData2.long$IGB>=50 & plotData2.long$MD==1 & plotData2.long$Lvl==7], na.rm=T)







##########################################3
plotData2.long$LvlTOProp[plotData2.long$Gender=="Female" & plotData2.long$Lvl==1]<-((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==1]/100)*13000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==1]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==1]/100)*13000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==1]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==1]/100)*13000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==1]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Female" & plotData2.long$Lvl==2]<-((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==2]/100)*10000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==2]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==2]/100)*10000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==2]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==2]/100)*10000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==2]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]<-((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]/100)*5000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]/100)*5000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==3]/100)*5000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==3]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Female" & plotData2.long$Lvl==4]<-((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==4]/100)*2000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==4]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==4]/100)*2000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==4]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==4]/100)*2000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==4]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]<-((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]/100)*1000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]/100)*1000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]/100)*1000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Female" & plotData2.long$Lvl==6]<-((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]/100)*500*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==6]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==6]/100)*500*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==6]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==6]/100)*500*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==6]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Female" & plotData2.long$Lvl==7]<-((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]/100)*100*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==7]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==7]/100)*100*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==7]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==7]/100)*100*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==7]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Male" & plotData2.long$Lvl==1]<-((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==1]/100)*13000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==1]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==1]/100)*13000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==1]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==1]/100)*13000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==1]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Male" & plotData2.long$Lvl==2]<-((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==2]/100)*10000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==2]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==2]/100)*10000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==2]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==2]/100)*10000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==2]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Male" & plotData2.long$Lvl==3]<-((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]/100)*5000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]/100)*5000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==3]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==3]/100)*5000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==3]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Male" & plotData2.long$Lvl==4]<-((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==4]/100)*2000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==4]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==4]/100)*2000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==4]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==4]/100)*2000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==4]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]<-((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]/100)*1000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]/100)*1000*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==5]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]/100)*1000*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Male" & plotData2.long$Lvl==6]<-((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]/100)*500*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==6]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==6]/100)*500*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==6]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==6]/100)*500*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==6]/100)))*100
plotData2.long$LvlTOProp[plotData2.long$Gender=="Male" & plotData2.long$Lvl==7]<-((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==5]/100)*100*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==7]/100)) / (((plotData2.long$Percentage[plotData2.long$Gender=="Female" & plotData2.long$Lvl==7]/100)*100*(plotData2.long$LvlTO[plotData2.long$Gender=="Female" & plotData2.long$Lvl==7]/100)) + ((plotData2.long$Percentage[plotData2.long$Gender=="Male" & plotData2.long$Lvl==7]/100)*100*(plotData2.long$LvlTO[plotData2.long$Gender=="Male" & plotData2.long$Lvl==7]/100)))*100

plotData2.long$LvlTOProp.inv<-ifelse(plotData2.long$Gender=="Female", plotData2.long$LvlTOProp*-1, plotData2.long$LvlTOProp)
ggplot(plotData2.long[plotData2.long$Lvl %in% c(5:7) & plotData2.long$IGB>50 & plotData2.long$Year %in% c(1, seq(10, 30, by=10)),], aes(x = Year, y = LvlTOProp.inv, fill=Gender)) +
  geom_bar(stat="identity", position="identity") + 
  theme_minimal() +
  scale_fill_manual(values = c("#3288bd", "#d53e4f")) +
  coord_flip() +
  scale_x_reverse(limits=c(40, -10), breaks=c(30, 20, 10, 1)) +
  scale_y_continuous(limits=c(-100, 100), breaks=c(-100, -50, 0, 50, 100), labels=c(100, 50, 0, 50, 100)) +
  facet_wrap(~Lvl, nrow = 1, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Turnover %", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
#3. Tokenism
png(filename="Tokenism TO x IGB.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2[plotData2$Lvl<8,], aes(x = Year, y = TOTok.F, group=OrgID)) +
  geom_line(aes(color=IGB), alpha=.5) + 
  scale_color_viridis(option="cividis", breaks=c(20, 85), limits=c(20, 85), labels=c("20% Male", "85% Male")) + 
  scale_x_continuous(limits=c(0, 45), breaks=c(1, seq(15, 45, by=15))) +
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Likelihood of TO due to Tokenism (%)", color="Initial Organization \n Gender Breakdown") +
  theme(legend.title = element_text(size=7), legend.text=element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

png(filename="MD Tokenism TO x IGB.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2[plotData2$Lvl<8 & plotData2$IGB>50,], aes(x = Year, y = TOTok.F, group=OrgID)) +
  geom_line(aes(color=IGB), alpha=.5) + 
  scale_color_viridis(option="cividis", breaks=c(50, 85), limits=c(50, 85), labels=c("50% Male", "85% Male")) + 
  scale_x_continuous(limits=c(0, 45), breaks=c(1, seq(15, 45, by=15))) +
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Likelihood of TO due to Tokenism (%)", color="Initial Organization \n Gender Breakdown") +
  theme(legend.title = element_text(size=7), legend.text=element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()



#plotData2.long$Gender.MD<-paste(plotData2.long$Gender, plotData2.long$MD, sep=".")
#ggplot(plotData2.long[plotData2.long$Lvl==5 & plotData2.long$IGB>=50 & plotData2.long$MD %in% c(1, 4) & plotData2.long$Year %in% c(1, 20, 40),], aes(x = TotalPromotions, fill=Gender.MD, color=Gender)) +
#  geom_density(aes(fill=Gender.MD, color=Gender), alpha=.7, size=1.3) + 
#  theme_minimal() +
#  scale_fill_manual(values = c("#d9d9d9", "#000000", "#d9d9d9", "#000000")) +
#  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
#  facet_wrap(~Year, nrow = 3, labeller = labeller(Year=year_names)) +
#  labs(x = "Total Promotions Earned", y = "Density", color="Gender") +
#  theme(strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
 


#3a. Promotions of women over time, year 1 female as color
png(filename="Female Promotions x IGB.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = TotalPromotions, group=OrgID)) +
  geom_line(aes(color=IGB), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis", breaks=c(20, 85), limits=c(20, 85), labels=c("20% Male", "85% Male")) + 
  scale_x_continuous(limits=c(0, 45), breaks=c(1, seq(15, 45, by=15))) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Total Promotions Earned by Females", color="Initial Organization \n Gender Breakdown") +
  theme(legend.title = element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()






#################################
##1.	Percentage of female agents in first year of the simulations on x axis.
png(filename="Final females.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData2[lastplotData2$Lvl<8,], aes(x = Yr1Female, y = Female)) +
  geom_point() + 
  theme_minimal() +
  geom_hline(yintercept=50, color="red3") +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year 1 Female %", y = "Final Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25))
dev.off()

##1a.	ExGB.init as color
png(filename="Final females x ExGB.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData2[lastplotData2$Lvl<8,], aes(x = Yr1Female, y = Female)) +
  geom_point(aes(color=ExGB.init), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  geom_hline(yintercept=50, color="red3") +
  #geom_smooth(method = "lm", color = "white") +
  #annotate("text", x = .4, y = .85, label = paste("r = ", round(datCors[1,], 2), sep="")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year 1 Female %", y = "Final Female %", color="Study 1 \n Male Hiring Rate") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off() 

##1b.	HJEffSize.init as color
png(filename="Final females x OppVal.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData2[lastplotData2$Lvl<8,], aes(x = Yr1Female, y = Female)) +
  geom_point(aes(color=HJEffSize.init), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  geom_hline(yintercept=50, color="red3") +
  #geom_smooth(method = "lm", color = "white") +
  #annotate("text", x = .4, y = .85, label = paste("r = ", round(datCors[1,], 2), sep="")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year 1 Female %", y = "Final Female %", color="Study 1 \n Dev. Opp. Effect Size") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

##1c.	ExGB on x axis, HJEffSize.init as color
png(filename="Final females x OppVal x ExGB.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData2[lastplotData2$Lvl<8,], aes(x = ExGB.init, y = Female)) +
  geom_point(aes(color=HJEffSize.init), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  geom_hline(yintercept=50, color="red3") +
  #geom_smooth(method = "lm", color = "white") +
  #annotate("text", x = .4, y = .85, label = paste("r = ", round(datCors[1,], 2), sep="")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Study 1 Male Hiring Rate", y = "Final Female %", color="Study 1 \n Dev. Opp. Effect Size") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#2.	Time on x axis, color=Yr1Female.
png(filename="Female percent x Year 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2[plotData2$Lvl<8,], aes(x = Year, y = Female, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  geom_hline(yintercept=50, color="red3") +
  #geom_smooth(method = "lm", color = "white") +
  #annotate("text", x = .4, y = .85, label = paste("r = ", round(datCors[1,], 2), sep="")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female %", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

ggplot(plotData2[plotData2$Lvl<8,], aes(x = Year, y = Female, group=OrgID)) +
  geom_line(aes(color=IGB), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  geom_hline(yintercept=50, color="red3") +
  #geom_smooth(method = "lm", color = "white") +
  #annotate("text", x = .4, y = .85, label = paste("r = ", round(datCors[1,], 2), sep="")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female %", color="Org. Level Male %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))

mean(aggregate(plotData2$Year[plotData2$Female>49.99 & plotData2$Female<51 & plotData2$Yr1Female<50 & plotData2$Lvl<8], by = list(plotData2$OrgID[plotData2$Female>49.99 & plotData2$Female<51 & plotData2$Yr1Female<50  & plotData2$Lvl<8]), min)[,2])
sd(aggregate(plotData2$Year[plotData2$Female>49.99 & plotData2$Female<51 & plotData2$Yr1Female<50  & plotData2$Lvl<8], by = list(plotData2$OrgID[plotData2$Female>49.99 & plotData2$Female<51 & plotData2$Yr1Female<50  & plotData2$Lvl<8]), min)[,2])

mean(lastplotData2$Female[lastplotData2$Yr1Female<50 & lastplotData2$Lvl<8])
min(lastplotData2$Female[lastplotData2$Yr1Female<50 & lastplotData2$Lvl<8])
tail(sort(lastplotData2$Female[lastplotData2$Yr1Female<50 & lastplotData2$Lvl<8], TRUE), 25)
sd(lastplotData2$Female[lastplotData2$Yr1Female<50 & lastplotData2$Lvl<8])

#3. Performance of men and women over time
png(filename="Performance.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = Perf, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Performance", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#3a. Performance of women over time, year 1 female as color
png(filename="Female Performance x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = Perf, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Performance", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#4. Ability of men and women over time
png(filename="Ability.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = Ability, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Ability", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#4a. Ability of women over time, year 1 female as color
png(filename="Female Ability x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = Ability, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Ability", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#5. Age of men and women over time
png(filename="Age.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = Age, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Age", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#5a. Age of women over time, year 1 female as color
png(filename="Female Age x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = Age, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Age", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#6. Level tenure of men and women over time
png(filename="Level tenure.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = TimeAtLvl, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Years in Level", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#6a. Level tenure of women over time, year 1 female as color
png(filename="Female Level tenure x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = TimeAtLvl, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Years in Level", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#6b. Level tenure of men over time, year 1 female as color
png(filename="Male Level tenure x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Male",], aes(x = Year, y = TimeAtLvl, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Male Years in Level", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#7. Tokenism
png(filename="Tokenism TO x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2[plotData2$Lvl<8,], aes(x = Year, y = TOTok.F, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Likelihood of TO due to Tokenism (%)", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#8. Turnover due to age
png(filename="Age TO.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = TOAge, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Likelihood TO due to Age (%)", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#8a. Turnover due to age of women over time, year 1 female as color
png(filename="Female Age TO x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = TOAge, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Likelihood TO Age (%)", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#9. Turnover due to level tenure
png(filename="Time in Level TO.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = TOLvl, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Likelihood TO due to Lvl Tenure", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#9a. Turnover due to level tenure of women over time, year 1 female as color
png(filename="Female Time in Level TO x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = TOLvl, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Likelihood TO Lvl (%)", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#9b. Turnover due to level tenure of men over time, year 1 female as color
png(filename="Male Time in Level TO x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Male",], aes(x = Year, y = TOLvl, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Male Likelihood TO Lvl (%)", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#10. Turnover of men and women over time
png(filename="Level TO.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = LvlTO, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Turnover %", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#10a. Turnover of women over time, year 1 female as color
png(filename="Female Level TO x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = LvlTO, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Turnover %", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#11. Upper ext hires, men and women over time
png(filename="Upper Ext Hires.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = UpperExtHires, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Upper Ext Hires %", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#11a. Upper ext hires women over time, year 1 female as color
png(filename="Female Upper Ext Hires x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = UpperExtHires, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Upper Ext Hires %", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#12. Lower ext hires, men and women over time
png(filename="Lower Ext Hires.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = LowerExtHires, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Lower Ext Hires %", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#12a. Lower ext hires women over time, year 1 female as color
png(filename="Female Lower Ext Hires x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = LowerExtHires, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Lower Ext Hires %", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#13. Total promotions, men and women over time
png(filename="Promotions.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8,], aes(x = Year, y = TotalPromotions, group=OrgID.Gender)) +
  geom_line(aes(color=Gender), alpha=.5) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Total Promotions", color="Gender") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#13a. Female promotions over time, year 1 female as color
png(filename="Female Promotions x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Female",], aes(x = Year, y = TotalPromotions, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Female Total Promotions", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#13b. Male promotions over time, year 1 female as color
png(filename="Male Promotions x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2.long[plotData2.long$Lvl<8 & plotData2.long$Gender=="Male",], aes(x = Year, y = TotalPromotions, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Male Total Promotions", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

#14. Orgs' total average performance over time
plotData2$Perf.F.temp<-ifelse(is.na(plotData2$Perf.F), 0, plotData2$Perf.F)
plotData2$Perf.M.temp<-ifelse(is.na(plotData2$Perf.M), 0, plotData2$Perf.M)
plotData2$Perf.average<-((plotData2$Perf.M.temp+plotData2$Perf.F.temp)/2)

png(filename="Overall Performance x Yr 1 Female.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(plotData2[plotData2$Lvl<8,], aes(x = Year, y = Perf.average, group=OrgID)) +
  geom_line(aes(color=Yr1Female), alpha=.5) + 
  theme_minimal() +
  scale_color_viridis(option="cividis") + 
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Year", y = "Average Performance", color="Year 1 Female %") +
  theme(strip.background = element_rect(color="black"), legend.position = c(.875,.25), legend.title = element_text(size=7))
dev.off()

##########################
#Lower and upper external hires
ExtHires.long<-melt(plotData.long[, c("OrgID", "Lvl", "Year", "extGenPercent", "hjEffSz", "Gender", "LowerExtHiresPercent", "UpperExtHiresPercent")], id=c("OrgID", "Lvl", "Year", "extGenPercent", "hjEffSz", "Gender"))
colnames(ExtHires.long)[c(7,8)]<-c("ExtHiresLevel", "ExtHiresPercent")
levels(ExtHires.long$ExtHiresLevel)[levels(ExtHires.long$ExtHiresLevel)=="LowerExtHiresPercent"]<-"Lower"
levels(ExtHires.long$ExtHiresLevel)[levels(ExtHires.long$ExtHiresLevel)=="UpperExtHiresPercent"]<-"Upper"
ExtHires.long$ExtHiresLevel.Gender<-paste(ExtHires.long$ExtHiresLevel,ExtHires.long$Gender, sep=".")
#ExtHires.long$HJDiff.Gender<-paste(ExtHires.long$hjEffSz,ExtHires.long$Gender, sep=".")