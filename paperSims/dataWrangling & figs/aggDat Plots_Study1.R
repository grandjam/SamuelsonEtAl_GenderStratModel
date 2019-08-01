library(ggplot2)
library(viridis)
library(tidyr)
library(reshape2)

#######
#PLOTS#
#######

plotData1<-readRDS("Study 1 Plot Data 2-26-19.Rds")
lastplotData1<-readRDS("Study 1 Last Year Plot Data 2-26-19.Rds")
plotData1.long<-readRDS("Study 1 Plot Data Long 3-5-19.Rds")
lastplotData1.long<-readRDS("Study 1 Last Year Plot Data Long 3-5-19.Rds")
plotData1.long$OrgID.Gender<-paste(plotData1.long$OrgID, plotData1.long$Gender)
lastplotData1.long$OrgID.Gender<-paste(lastplotData1.long$OrgID, lastplotData1.long$Gender)

ExtHires.long<-melt(plotData1.long[, c("OrgID", "Lvl", "Year", "ExGB", "HJEffSize", "Gender", "LowerExtHires", "UpperExtHires")], id=c("OrgID", "Lvl", "Year", "ExGB", "HJEffSize", "Gender"))
colnames(ExtHires.long)[c(7,8)]<-c("ExtHiresLevel", "ExtHiresPercent")
levels(ExtHires.long$ExtHiresLevel)[levels(ExtHires.long$ExtHiresLevel)=="LowerExtHiresPercent"]<-"Lower"
levels(ExtHires.long$ExtHiresLevel)[levels(ExtHires.long$ExtHiresLevel)=="UpperExtHiresPercent"]<-"Upper"
ExtHires.long$ExtHiresLevel.Gender<-paste(ExtHires.long$ExtHiresLevel,ExtHires.long$Gender, sep=".")
#ExtHires.long$HJDiff.Gender<-paste(ExtHires.long$hjEffSz,ExtHires.long$Gender, sep=".")

level_names <- c('1'="Level 1", '2'="Level 2", '3'="Level 3", '4'="Level 4", '5'="Level 5", '6'="Level 6", '7'="Level 7")

#PLOTS IN REVISION#
#Final Females
png(filename="Final females.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData1[lastplotData1$Lvl<8,], aes(x = ExGB, y = Female)) +
  geom_point(aes(color = HJEffSize), alpha=.5, size=.7) + 
  theme_minimal() +
  scale_color_viridis(option="cividis", breaks=c(0, .5, 1), limits=c(0, 1), labels=c("0 SD", ".5 SD", "1 SD")) + 
  geom_abline(size = 1.25, slope=-1, intercept=100, linetype=1, color="red3") +
  scale_x_continuous(limits=c(20, 80), breaks=c(seq(20, 80, by=20))) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0, 25, 50, 75, 100)) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Percentage of Male External Hires", y = "Final Percentage Female", color = "Developmental \n Opportunity \n Effect Size") +
  theme(legend.title = element_text(size=7), legend.text=element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

lastplotData1[lastplotData1$Female>50 & lastplotData1$Female<51 & lastplotData1$Lvl %in% c(5:7), c("HJEffSize", "ExGB")]

summary(lastplotData1[lastplotData1$ExGB>=49 & lastplotData1$ExGB<=51 & lastplotData1$Lvl==7, "Female"])

lastplotData1[lastplotData1$Lvl==7 & lastplotData1$ExGB>20 & lastplotData1$ExGB<21, c("Female", "HJEffSize")]
summary(lastplotData1[lastplotData1$Lvl==7 & lastplotData1$ExGB>20 & lastplotData1$ExGB<21, "Female"])


#Tokenism
png(filename="Final Tokenism TO.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData1[lastplotData1$Lvl<8,], aes(x = ExGB, y = TOTok.F)) +
  geom_point(color="#d53e4f", size=.7) + 
  scale_x_continuous(limits=c(20, 80), breaks=c(seq(20, 80, by=20))) +
  scale_y_continuous(limits=c(0, 11), breaks=seq(0, 10, by=2)) +
  theme_minimal() +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Percentage of Male External Hires", y = "Female Turnover Likelihood \n Due to Tokenism (%)") +
  theme(legend.title = element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

#png(filename="Tokenism TO with HJEffSize.png", 
#    type="cairo",
#    units="in", 
#    width=5, 
#    height=4, 
#    pointsize=12, 
#    res=96)
#ggplot(lastplotData1[lastplotData1$Lvl<8,], aes(x = ExGB, y = TOTok.F)) +
#  geom_point(aes(color = HJEffSize), alpha=.5, size=.7) + 
#  scale_color_viridis(option="cividis", breaks=c(0, .5, 1), limits=c(0, 1), labels=c("0 SD", ".5 SD", "1 SD")) + 
#  scale_x_continuous(limits=c(20, 80), breaks=c(seq(20, 80, by=20))) +
#  scale_y_continuous(limits=c(0, 11), breaks=c(seq(0,10, by=2))) +
#  theme_minimal() +
#  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
#  labs(x = "Percentage of Male External Hires", y = "Female Turnover Likelihood \n Due to Tokenism (%)", color = "Developmental \n Opportunity \n Effect Size") +
#  theme(legend.title = element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
#dev.off()

#ggplot(lastplotData1[lastplotData1$Lvl<8,], aes(x = ExGB, y = TOTok.F)) +
#  geom_point(aes(color = Age.F), alpha=.5) + 
#  scale_color_viridis(option="cividis") + 
#  scale_x_continuous(limits=c(20, 80), breaks=c(seq(20, 80, by=10))) +
#  scale_y_continuous(limits=c(0, 11), breaks=c(seq(0,10, by=2))) +
#  theme_minimal() +
#  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
#  labs(x = "Percentage of Male External Hires", y = "Female Turnover Likelihood \n Due to Tokenism (%)", color = "Female Age") +
#  theme(legend.title = element_text(size=7), strip.background = element_rect(color="black"), legend.position = c(.875,.25), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))

#ggplot(lastplotData1[lastplotData1$Lvl==7,], aes(x = ExGB, y = TOTok.F)) +
#  geom_point(aes(color = Female), alpha=.5) + 
#  scale_color_viridis(option="cividis") + 
#  scale_x_continuous(limits=c(20, 80), breaks=c(seq(20, 80, by=10))) +
#  scale_y_continuous(limits=c(0, 11), breaks=c(seq(0,10, by=2))) +
#  theme_minimal() +
#  labs(x = "Percentage of Male External Hires", y = "Female Turnover Likelihood \n Due to Tokenism (%)", color = "% Female") +
#  theme(legend.title = element_text(size=7), strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))

#Promotions of men and women in last year
png(filename="Final Promotions.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData1.long[lastplotData1.long$Lvl %in% c(2:5),], aes(x = HJEffSize, y = TotalPromotions)) +
  geom_point(aes(color=Gender), alpha=.5, size=.7) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, by=.25)) +
  scale_y_continuous(limits=c(0, 3), breaks=c(seq(0, 3, by=.5))) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Developmental Opportunity \n Effect Size", y = "Average Total Promotions", color = "Gender") +
  theme(legend.title = element_text(size=7), strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black")) +
  guides(color = guide_legend(override.aes = list(size=6)))
dev.off()

#Turnover due to level tenure
png(filename="Lvl Tenure TO.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData1.long[lastplotData1.long$Lvl %in% c(2:5),], aes(x = HJEffSize, y = TOLvl)) +
  geom_point(aes(color=Gender), alpha=.5, size=.7) + 
  theme_minimal() +
  scale_color_manual(values = c("#3288bd", "#d53e4f")) +
  scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, by=.25)) +
  scale_y_continuous(limits=c(0, 2.5), breaks=c(seq(0, 2.5, by=.5))) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Developmental Opportunity \n Effect Size", y = "Turnover Likelihood Due to Level Tenure (%)", color = "Gender") +
  theme(legend.title = element_text(size=7), strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black")) +
  guides(color = guide_legend(override.aes = list(size=6)))
dev.off()

#Lower and upper external hires
png(filename="External hires ExGB greater than 50.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(ExtHires.long[ExtHires.long$Lvl %in% c(5:7) & ExtHires.long$ExGB>=50,], aes(x=Gender, y=ExtHiresPercent)) +
  geom_bar(aes(fill=ExtHiresLevel), position="stack", stat = "summary", fun.y = "mean", color="black") + 
  theme_minimal() +
  scale_fill_viridis(option="cividis", discrete=TRUE, name=" ", labels=c("Promoted from \n Lower Levels", "Externally Hired in \n Upper Level")) +
  #scale_fill_manual(values = c("gray50", "gray20"), name=" ", labels=c("Promoted from \n Lower Level", "Externally Hired in \n Upper Level")) +
  facet_wrap(~Lvl, labeller = labeller(Lvl=level_names)) +
  labs(x = "Gender", y = "Percentage") +
  theme(strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
dev.off()

summary(ExtHires.long[ExtHires.long$Gender=="Female" & ExtHires.long$Lvl==7 & ExtHires.long$ExGB>=50 & ExtHires.long$ExtHiresLevel=="LowerExtHires", "ExtHiresPercent"])
summary(ExtHires.long[ExtHires.long$Gender=="Male" & ExtHires.long$Lvl==7 & ExtHires.long$ExGB>=50 & ExtHires.long$ExtHiresLevel=="LowerExtHires", "ExtHiresPercent"])

#Discussion graph - may not use any more given study 2
png(filename="Final females Discussion.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=96)
ggplot(lastplotData1[lastplotData1$Lvl==7 & lastplotData1$ExGB>50 & lastplotData1$HJEffSize<.5,], aes(x = ExGB, y = Female)) +
  geom_point(aes(color = HJEffSize)) + 
  theme_minimal() +
  scale_color_viridis(option="cividis", breaks=c(0, .5), limits=c(0, .5), labels=c("0 SD", ".5 SD")) + 
  scale_y_continuous(limits=c(0, 60), breaks=c(0, 25, 50)) +
  facet_wrap(~Lvl, nrow = 2, labeller = labeller(Lvl=level_names)) +
  labs(x = "Percentage of Male External Hires", y = "Final Percentage Female", color = "Developmental \n Opportunity \n Effect Size") +
  scale_x_reverse(limits=c(80, 50), breaks=c(80, 70, 60, 50)) +
  theme(legend.title = element_text(size=7), legend.text=element_text(size=7), strip.background = element_rect(color="black"), legend.position = "top", panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))
dev.off()