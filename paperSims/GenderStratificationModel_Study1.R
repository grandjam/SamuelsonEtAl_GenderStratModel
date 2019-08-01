sim <- function (externalGender, hotJobEffSize) {
  
library(truncnorm)

#1.  Initialize time clock T = 0
t<-0

#2.  Create organizational structure and populate with initial employees

#Time input
year.inp<-12 #Frequency of TO/promotions

#Organizational structure input
levels.inp<-8
grpSize.inp<-100
ceoSize.inp<-.01*grpSize.inp 
lowerLevels.inp<-1:4
upperLevels.inp<-5:8

#Number of groups per level
grps7.inp<-1
grps6.inp<-5
grps5.inp<-10
grps4.inp<-20
grps3.inp<-50
grps2.inp<-100
grps1.inp<-130
allGrps<-c(grps7.inp, grps6.inp, grps5.inp, grps4.inp, grps3.inp, grps2.inp, grps1.inp)

#Total agents
n.inp<-(ceoSize.inp+grpSize.inp*(grps7.inp+grps6.inp+grps5.inp+grps4.inp+grps3.inp+grps2.inp+grps1.inp))

#Initial gender distribution input
initPercent.male.inp<-.5 #Percentage of initial agents that are male

#Age input
minAge.inp<-22 #Minimum agent age
upperAgeCenter.inp<-55 #Center of age distribution for levels 5-7
lowerAgeCenter.inp<-35 #Center of age distribution for levels 1-4

#Line/staff input (percentage line)
line.female.inp<-.3 #Percentage of female agents that are line
line.male.inp<-1-line.female.inp #Percentage of male agents that are line

#Ability input
ability.mean.inp<-100 #Center of initial ability distribution
ability.sd.inp<-15 #SD of initial ability distribution

#Opportunity value input
hotJobs.inp<-.5 #Percentage of agents that are offered an opportunity each month
minHotJobsVal.inp<-15 #Minimum opportunity value
maxHotJobsVal.inp<-150 #Maximum opportunity value
hotJobsMean.inp<-(minHotJobsVal.inp+maxHotJobsVal.inp)/2 #Center of overall opportunity value distribution
hotJobsSD.inp<-20 #SD of opportunity value distributions

#Risk taking input
risktakers.male.inp<-.55 #Percentage of male agents with risk-taking propensity greater than 0.5
risktakers.female.inp<-.50 #Percentage of female agents with risk-taking propensity greater than 0.5

#External hiring input
upperExtHireRate.inp<-.40 #Percentage open positions filled by external hires at levels 5-8 each year
lowerExtHireRate.inp<-.60 #Percentage open positions filled by external hires at levels 2-4 (level 1=all external hires) each year
extHire.male.inp<-externalGender #Percentage of external hires who are male

#Career delay input
parents.female.inp<-.039 #Percentage of female career delay-takers who are on parental leave each year
delayTO.inp<-.101 #Percentage of delay takers who do not return each year

#Turnover (TO)
##Level tenure
timeWindow <- 7 
aSlopeTime <- 2
bMidTime <- 7 #Point at which TO probability is increasing most rapidly
cLowerTime <- 0
dUpperTime <- .25 #Max probability of leaving due to level tenure

##Tokenism
aSlopeTkn <- 50 #How quickly TO probability decreases as number of similar agents increases
bMidTkn <- .15 #Point at which TO probability is decreasing most rapidly
cLowerTkn <- 0
dUpperTkn <- .10 #Max probability of leaving due to tokenism

##Age (retirement)
retireAge <- 65
aSlopeAge <- 2 #How quickly TO probability due to age increases after retireAge has elapsed
bMidAge <- 0 #Point at which TO probability is increasing most rapidly
cLowerAge <- 0
dUpperAge <- 1 #Max probobability of leaving due to retirement

baselineTO<-0.08

#FUNCTIONS
###Level Tenure TO
toLvl <- function(timeAtLvl, timeWindow, aSlope, bMid, cLower, dUpper) {
  to.lvl <- cLower + ((dUpper-cLower)/(1+exp(-aSlope*(timeAtLvl-timeWindow-bMid))))
  return(to.lvl)
}

###Tokenism TO
toToken <- function(pctSim, aSlope, bMid, cLower, dUpper) {
  to.tkn <- cLower + ((dUpper-cLower)/(1+exp(aSlope*(pctSim-bMid))))
  return(to.tkn)
}

###Retirement TO
toAge <- function(age, retireAge, aSlope, bMid, cLower, dUpper) {
  to.age <- cLower + ((dUpper-cLower)/(1+exp(-aSlope*(age-retireAge-bMid))))
  return(to.age)
}

##Groups function. Returns sequence of 1-group size repeated the number of groups in the level
groups<-function(numGroups, groupSize) {
  groups<-rep(seq(1, numGroups), times=groupSize)
  return(groups)
}

##Line-staff function. Returns vector of 0s and 1s based on percentage line
line<-function(vectorLength, linePercent) {
  line<-sample(c(sample(0:1, vectorLength-(floor((1-linePercent)*vectorLength)+floor(linePercent*vectorLength)), replace=T, prob=c(1-linePercent, linePercent)), 
                 rep(0, floor((1-linePercent)*vectorLength)), 
                 rep(1, floor(linePercent*vectorLength))))
  return(line)
}

##Career delay function. Returns vector of 0s and 1s based on percentage who take career delay.
delays<-function(vectorLength, delayTakersPercent) {
  delays<-sample(c(sample(0:1, vectorLength-(floor(delayTakersPercent*vectorLength)+floor((1-delayTakersPercent)*vectorLength)), replace=T, prob=c(1-delayTakersPercent, delayTakersPercent)), 
                   rep(1, floor(delayTakersPercent*vectorLength)), 
                   rep(0, floor((1-delayTakersPercent)*vectorLength))))
  return(delays)
}

##Opportunity function. Returns vector of 0s and 1s based on percentage who receive a hot job.
hotJobs<-function(vectorLength, hotJobsPercent) {
  hotJobs<-sample(c(sample(0:1, vectorLength-(floor((1-hotJobsPercent)*vectorLength)+floor(hotJobsPercent*vectorLength)), replace=T, prob=c(1-hotJobsPercent, hotJobsPercent)), 
                    rep(0, floor((1-hotJobsPercent)*vectorLength)), 
                    rep(1, floor(hotJobsPercent*vectorLength))))
  return(hotJobs)
}

##Risk-taking function. Returns percentages that translate to likelihood of taking an opportunity if offered. Above 50%=more likely to take opportunity.
riskTaking<-function(vectorLength, riskTakersPercent) {
  riskTaking<-sample(c(runif(vectorLength-(floor((1-riskTakersPercent)*vectorLength)+floor(riskTakersPercent*vectorLength)), min=0, max=1), 
                       runif(riskTakersPercent*vectorLength, min=.51, max=1), 
                       runif((1-riskTakersPercent)*vectorLength, min=0, max=.5)))
  return(riskTaking)
}

#Create organization matrix.
org.mat<-as.data.frame(matrix(0, nrow = n.inp, ncol = 35))
colnames(org.mat)<-c("EmpID.s", "Level.s", "Group.s", "Line.s", "UpperEH.s", "LowerEH.s", "Male.s", "Original.s", "Ability.s", "RiskTaking.s", "Age.v", "TimeAtLvl.v", "TimeInOrg.v", "Similar.v", "TOAge.v", "TOLvl.v", "TOToken.v", "TOTotal.v", "HJBoost.v", "Perf.v", "PenultPerf.v", "YrlyPerf.v", "TotalHJBoost.v", "TotalHJ.v", "YrlyHJBoost.v", "YrlyHJ.v", "TotalPromotions.v", "TotalMonthsDelay.v", "TO.m", "Promote.m", "OnDelay.m", "Parent.m", "HJ.m", "TakesHJ.m", "ExtHire.m")

org.mat$EmpID.s<-(1:n.inp)

org.mat$Level.s<-c(8, rep(7, grpSize.inp*grps7.inp), rep(6, grpSize.inp*grps6.inp), rep(5, grpSize.inp*grps5.inp), rep(4, grpSize.inp*grps4.inp), rep(3, grpSize.inp*grps3.inp), rep(2, grpSize.inp*grps2.inp), rep(1, grpSize.inp*grps1.inp))

org.mat$Group.s<-c(1, unlist(sapply(allGrps, groups, groupSize=grpSize.inp))) #Extra 1 at beginning is for CEO level


#Create output object
output.arr<-array(0, dim=c(100, 83, 8), dimnames=list(c(1:100), c("OrgID", "IGB", "ExGB", "HJMean.M", "HJMean.F", "Level", "Male", "OrgTO", "OrgTO.M", "OrgTO.F", "Original", "Original.M", "Original.F", "OrgTOProportionMale", "OrgTOProportionFemale", "LvlTO", "LvlTO.M", "LvlTO.F", "PromotedOut", "PromotedOut.M", "PromotedOut.F", "YrlyExtHires", "YrlyExtHires.M", "YrlyExtHires.F", "LowerExtHires", "LowerExtHires.M", "LowerExtHires.F", "UpperExtHires", "UpperExtHires.M", "UpperExtHires.F", "TOTok.F", "TOLvl.M", "TOLvl.F", "TOAge.M", "TOAge.F", "TimeAtLvl.M", "TimeAtLvl.M.v", "TimeAtLvl.F", "TimeAtLvl.F.v", "TimeInOrg.M", "TimeInOrg.M.v", "TimeInOrg.F", "TimeInOrg.F.v", "Perf.M", "Perf.M.v", "Perf.F", "Perf.F.v", "Ability.M", "Ability.M.v", "Ability.F", "Ability.F.v", "Risk.M", "Risk.M.v", "Risk.F", "Risk.F.v", "Age.M", "Age.M.v", "Age.F", "Age.F.v", "YrlyHJ.M", "YrlyHJ.M.v", "YrlyHJ.F", "YrlyHJ.F.v", "YrlyHJBoost.M", "YrlyHJBoost.M.v", "YrlyHJBoost.F", "YrlyHJBoost.F.v", "TotalHJ.M", "TotalHJ.M.v", "TotalHJ.F", "TotalHJ.F.v", "TotalHJBoost.M", "TotalHJBoost.M.v", "TotalHJBoost.F", "TotalHJBoost.F.v", "TotalPromotions.M", "TotalPromotions.M.v", "TotalPromotions.F", "TotalPromotions.F.v", "TotalMonthsDelay.M", "TotalMonthsDelay.M.v", "TotalMonthsDelay.F", "TotalMonthsDelay.F.v"), c("Lvl1", "Lvl2", "Lvl3", "Lvl4", "Lvl5", "Lvl6", "Lvl7", "Lvl8")))
output.arr[,"IGB",]<-initPercent.male.inp
output.arr[,"ExGB",]<-externalGender
output.arr[,"HJMean.M",]<-hotJobsMean.inp
output.arr[,"HJMean.F",]<-hotJobsMean.inp-(hotJobEffSize*hotJobsSD.inp)

#Assign each employee the following attributes:

#Gender
##Assigns initial gender to entire organization at once (randomized)
org.mat$Male.s<-sample(c(sample(0:1, n.inp-(floor((1-initPercent.male.inp)*n.inp)+floor(initPercent.male.inp*n.inp)), replace=T, prob=c(1-initPercent.male.inp, initPercent.male.inp)), rep(0, floor((1-initPercent.male.inp)*n.inp)), rep(1, floor(initPercent.male.inp*n.inp))))

#Original employee status (=1) & hire status (upper or lower)
org.mat$Original.s<-1
org.mat$LowerEH.s[org.mat$Level.s %in% c(1:4)]<-1
org.mat$UpperEH.s[org.mat$Level.s %in% c(5:8)]<-1

#Age
org.mat$Age.v[org.mat$Level.s %in% upperLevels.inp]<-rpois(nrow(org.mat[org.mat$Level.s %in% upperLevels.inp,]), upperAgeCenter.inp)
org.mat$Age.v[org.mat$Level.s %in% lowerLevels.inp]<-rpois(nrow(org.mat[org.mat$Level.s %in% lowerLevels.inp,]), lowerAgeCenter.inp)
org.mat$Age.v<-ifelse(org.mat$Age.v<minAge.inp, minAge.inp, org.mat$Age.v)

#Line/staff designation based on gender (30% line=women, 70% staff=women).
##Female line and staff
org.mat$Line.s[org.mat$Male.s==0]<-line(length(org.mat$Line.s[org.mat$Male.s==0]), line.female.inp)
##Male line and staff
org.mat$Line.s[org.mat$Male.s==1]<-line(length(org.mat$Line.s[org.mat$Male.s==1]), line.male.inp)

#Ability
org.mat$Ability.s<-rnorm(n.inp, ability.mean.inp, ability.sd.inp) #Ability based on IQ, mean=100, sd=15 (see input variables)

#Risktaking.
##Male risktaking
org.mat$RiskTaking.s[org.mat$Male.s==1]<-riskTaking(length(org.mat$RiskTaking.s[org.mat$Male.s==1]), risktakers.male.inp)
##Female risktaking
org.mat$RiskTaking.s[org.mat$Male.s==0]<-riskTaking(length(org.mat$RiskTaking.s[org.mat$Male.s==0]), risktakers.female.inp)

#3. Increment time clock T = T + 1

while(sum(org.mat$Original.s)>0){
  t<-t+1
  # startItTime <- Sys.time()
  # Sys.sleep(.01)
  # print(paste("Start of iteration", t, sep = " "))
  
  #Performance
  
  ##Clear out monthly/yearly hot job variables and markers
  org.mat$HJ.m<-0
  org.mat$TakesHJ.m<-0
  
  ##4. Assign opportunities and determine which employees take opportunities assigned to them based on risk-taking propensity
  
  org.mat$HJ.m<-hotJobs(n.inp, hotJobs.inp)
  
  org.mat$TakesHJ.m[org.mat$HJ.m==1]<-sapply(org.mat$RiskTaking.s[org.mat$HJ.m==1], function(x) rbinom(1, 1, x))
  
  ##Reset HotJob.m to 0 if agent didn't take the opportunity they were given.
  org.mat$HJ.m[org.mat$TakesHJ.m==0]<-0
  
  ##Sum count of opportunities (Yrly gets cleared out every 12 months)
  org.mat$YrlyHJ.v<-org.mat$YrlyHJ.v+org.mat$HJ.m
  org.mat$TotalHJ.v<-org.mat$TotalHJ.v+org.mat$HJ.m
  
  ##Determine opportunity value
  org.mat$HJBoost.v[org.mat$Male.s==1 & org.mat$HJ.m==1]<-rtruncnorm(length(org.mat$HJBoost.v[org.mat$Male.s==1 & org.mat$HJ.m==1]), a=minHotJobsVal.inp, b=maxHotJobsVal.inp, mean=hotJobsMean.inp, sd=hotJobsSD.inp)
  org.mat$HJBoost.v[org.mat$Male.s==0 & org.mat$HJ.m==1]<-rtruncnorm(length(org.mat$HJBoost.v[org.mat$Male.s==0 & org.mat$HJ.m==1]), a=minHotJobsVal.inp, b=maxHotJobsVal.inp, mean=hotJobsMean.inp-(hotJobEffSize*hotJobsSD.inp), sd=hotJobsSD.inp)
  
  ##Sum total opportunity (yrly gets cleared out every 12 months)
  org.mat$YrlyHJBoost.v<-org.mat$YrlyHJBoost.v+org.mat$HJBoost.v
  org.mat$TotalHJBoost.v<-org.mat$TotalHJBoost.v+org.mat$HJBoost.v
  
  ##5. Assign performance scores, add opportunity values to employees' performance scores, accumulate performance scores
  
  org.mat$Perf.v<-sapply(org.mat$Ability.s, function(x) rnorm(1, x, ability.sd.inp))
  org.mat$Perf.v[org.mat$HJ.m==1]<-(org.mat$Perf.v[org.mat$HJ.m==1]+org.mat$HJBoost.v[org.mat$HJ.m==1])
  
  ##Cumulative performance:
  org.mat$YrlyPerf.v<-(org.mat$YrlyPerf.v+org.mat$Perf.v)
  
  ##Save second to last performance round:
  if (t%%(year.inp-1)==0) {
    org.mat$PenultPerf.v<-org.mat$Perf.v 
  }

    #EVERY 12 CYCLES:
  if(t%%year.inp==0) {
    # startAnnTime <- Sys.time()
    # Sys.sleep(.01)
    # print(paste("Starting annual computations for iteration",t, sep = " "))
    
    #Clear out TO, External Hire, and Promote markers
    org.mat$TO.m<-0
    org.mat$ExtHire.m<-0
    org.mat$Promote.m<-0
    
    #Calculate percentage of similar agents (1 for male agents, since tokenism doesn't apply)
    grpGenderProp<-aggregate(org.mat$Male.s, list(org.mat$Group.s, org.mat$Level.s), mean)
    org.mat$Similar.v<-sapply(1:nrow(org.mat), function (x) {1-(grpGenderProp[grpGenderProp$Group.1==org.mat$Group.s[x] & grpGenderProp$Group.2==org.mat$Level.s[x], 3])})
    org.mat$Similar.v[org.mat$Male.s==1]<-1
    
    #7. Assign career delays, deduct performance rounds from delay takers, and assign turnover to specified percentage of delay takers
      
    ##Assign career delays:
    ###Men 33 and below (.102)
    org.mat$OnDelay.m[org.mat$Age.v<34 & org.mat$Male.s==1]<-delays(length(org.mat$OnDelay.m[org.mat$Age.v<34 & org.mat$Male.s==1]), .102)
    ###Women 33 and below (.153)
    org.mat$OnDelay.m[org.mat$Age.v<34 & org.mat$Male.s==0]<-delays(length(org.mat$OnDelay.m[org.mat$Age.v<34 & org.mat$Male.s==0]), .153)
    ###Men 34-49 (.115)
    org.mat$OnDelay.m[org.mat$Age.v>33 & org.mat$Age.v<50 & org.mat$Male.s==1]<-delays(length(org.mat$OnDelay.m[org.mat$Age.v>33 & org.mat$Age.v<50 & org.mat$Male.s==1]), .115)
    ###Women 34-49 (.152)
    org.mat$OnDelay.m[org.mat$Age.v>33 & org.mat$Age.v<50 & org.mat$Male.s==0]<-delays(length(org.mat$OnDelay.m[org.mat$Age.v>33 & org.mat$Age.v<50 & org.mat$Male.s==0]), .152)
    ###Men 50 and above (.125)
    org.mat$OnDelay.m[org.mat$Age.v>49 & org.mat$Male.s==1]<-delays(length(org.mat$OnDelay.m[org.mat$Age.v>49 & org.mat$Male.s==1]), .125)
    ###Women 50 and above (.148)
    org.mat$OnDelay.m[org.mat$Age.v>49 & org.mat$Male.s==0]<-delays(length(org.mat$OnDelay.m[org.mat$Age.v>49 & org.mat$Male.s==0]), .148)
      
    ##Assign parental leave to females
    org.mat$Parent.m[org.mat$OnDelay.m==1 & org.mat$Male.s==0]<-delays(length(org.mat$Parent.m[org.mat$OnDelay.m==1 & org.mat$Male.s==0]), parents.female.inp)
    
    ##Subtract last performance round (or last two performance rounds, if parent) from delay takers.
    ###(Female) Parents 
    org.mat$YrlyPerf.v[org.mat$OnDelay.m==1 & org.mat$Parent.m==1]<-org.mat$YrlyPerf.v[org.mat$OnDelay.m==1 & org.mat$Parent.m==1]-(org.mat$Perf.v[org.mat$OnDelay.m==1 & org.mat$Parent.m==1] + org.mat$PenultPerf.v[org.mat$OnDelay.m==1 & org.mat$Parent.m==1])
    ###Non-Parents
    org.mat$YrlyPerf.v[org.mat$OnDelay.m==1 & org.mat$Parent.m==0]<-org.mat$YrlyPerf.v[org.mat$OnDelay.m==1 & org.mat$Parent.m==0]-org.mat$Perf.v[org.mat$OnDelay.m==1 & org.mat$Parent.m==0]
    
    ##Update months on delay
    org.mat$TotalMonthsDelay.v[org.mat$OnDelay.m==1]<-(org.mat$TotalMonthsDelay.v[org.mat$OnDelay.m==1]+1)
    org.mat$TotalMonthsDelay.v[org.mat$Parent.m==1]<-(org.mat$TotalMonthsDelay.v[org.mat$Parent.m==1]+1)
    
    ##Mark specified percentage (input variable) of those who took a career delay as turning over
    org.mat$TO.m[org.mat$OnDelay.m==1]<-delays(length(org.mat$TO.m[org.mat$OnDelay.m==1]), delayTO.inp)
        
    ##"Bring back" the other delay takers for TO and promotion
    org.mat$OnDelay.m[org.mat$TO.m==0]<-0
      
    #8. Update employee tenure at level and age, calculate likelihood of turning over due to level tenure, age, and tokenism
    org.mat$TimeAtLvl.v<-(org.mat$TimeAtLvl.v+1)
    org.mat$TimeInOrg.v<-(org.mat$TimeInOrg.v+1)
    org.mat$Age.v<-(org.mat$Age.v+1)
      
    #Calculate TO due to age:
    org.mat$TOAge.v <- toAge(org.mat$Age.v, retireAge, aSlopeAge, bMidAge, cLowerAge, dUpperAge)
      
    #Calculate TO due to time at same level:
    org.mat$TOLvl.v <- toLvl(org.mat$TimeAtLvl.v, timeWindow, aSlopeTime, bMidTime, cLowerTime, dUpperTime)
      
    #Calculate TO due to tokenism:
      org.mat$TOToken.v <- toToken(org.mat$Similar.v, aSlopeTkn, bMidTkn, cLowerTkn, dUpperTkn)
      
    #Sum TO probabilities + baseline; cap at 1
    org.mat$TOTotal.v <- org.mat$TOLvl.v + org.mat$TOToken.v + org.mat$TOAge.v + baselineTO
    org.mat$TOTotal.v<-ifelse(org.mat$TOTotal.v>1, 1, org.mat$TOTotal.v)
      
    #9. Invoke voluntary turnover based on total turnover likelihood
    org.mat$TO.m[org.mat$OnDelay.m==0] <- sapply(org.mat$TOTotal.v[org.mat$OnDelay.m==0], function(x) rbinom(1, 1, x))

###---OUTPUT (before TO cleared out)---###
    #TO and time in level/org (tenure) output
    output.arr[t/year.inp, "OrgTO",]<-(sum(org.mat$TO.m)/n.inp)
    output.arr[t/year.inp, "OrgTO.M",]<-(sum(org.mat$TO.m[org.mat$Male.s==1])/sum(org.mat$Male.s==1)) #Men who TO over total men
    output.arr[t/year.inp, "OrgTO.F",]<-(sum(org.mat$TO.m[org.mat$Male.s==0])/sum(org.mat$Male.s==0)) #Women who TO over total women
    
    output.arr[t/year.inp, "OrgTOProportionMale",]<-(sum(org.mat$TO.m[org.mat$Male.s==1])/sum(org.mat$TO.m)) #% of total TO who are men
    output.arr[t/year.inp, "OrgTOProportionFemale",]<-(sum(org.mat$TO.m[org.mat$Male.s==0])/sum(org.mat$TO.m)) #% of total TO who are women
    
    for(i in 1:levels.inp){
      output.arr[t/year.inp,"Level",i]<-i
      
      #Male - males/total org
      output.arr[t/year.inp,"Male",i]<-(sum(org.mat$Male.s[org.mat$Male.s == 1 & org.mat$Level.s == i])/length(org.mat$Male.s[org.mat$Level.s == i]))
      
      #Original employees by level
      output.arr[t/year.inp, "Original", i]<-(sum(org.mat$Original.s[org.mat$Level.s==i])/length(org.mat$Original.s[org.mat$Level.s==i]))  
      output.arr[t/year.inp, "Original.M", i]<-(sum(org.mat$Original.s[org.mat$Level.s==i & org.mat$Male.s==1])/length(org.mat$Original.s[org.mat$Level.s==i & org.mat$Male.s==1]))
      output.arr[t/year.inp, "Original.F", i]<-(sum(org.mat$Original.s[org.mat$Level.s==i & org.mat$Male.s==0])/length(org.mat$Original.s[org.mat$Level.s==i & org.mat$Male.s==0]))
      
      #Total TO by level
      output.arr[t/year.inp, "LvlTO",i]<-(sum(org.mat$TO.m[org.mat$Level.s==i])/sum(org.mat$Level.s==i))
      output.arr[t/year.inp, "LvlTO.M",i]<-(sum(org.mat$TO.m[org.mat$Male.s==1 & org.mat$Level.s==i])/sum(org.mat$Male.s==1 & org.mat$Level.s==i)) #Total men who TO in that level/total men in that level
      output.arr[t/year.inp, "LvlTO.F",i]<-(sum(org.mat$TO.m[org.mat$Male.s==0 & org.mat$Level.s==i])/sum(org.mat$Male.s==0 & org.mat$Level.s==i)) #Total women who TO in that level/total women in that level
      
      #Tokenism TO prob (females only)
      output.arr[t/year.inp,"TOTok.F",i]<-mean(org.mat$TOToken.v[org.mat$Male.s==0 & org.mat$Level.s==i])
      
      #Time-at-level TO prob
      output.arr[t/year.inp,"TOLvl.M",i]<-mean(org.mat$TOLvl.v[org.mat$Male.s==1 & org.mat$Level.s==i])
      output.arr[t/year.inp,"TOLvl.F",i]<-mean(org.mat$TOLvl.v[org.mat$Male.s==0 & org.mat$Level.s==i]) 
      
      #Age TO prob
      output.arr[t/year.inp, "TOAge.M", i]<-mean(org.mat$TOAge.v[org.mat$Male.s==1 & org.mat$Level.s==i])
      output.arr[t/year.inp, "TOAge.F", i]<-mean(org.mat$TOAge.v[org.mat$Male.s==0 & org.mat$Level.s==i])
      
      #Tenure
      output.arr[t/year.inp, "TimeAtLvl.M", i]<-mean(org.mat$TimeAtLvl.v[org.mat$Male.s==1 & org.mat$Level.s==i])
      output.arr[t/year.inp, "TimeAtLvl.M.v", i]<-var(org.mat$TimeAtLvl.v[org.mat$Male.s==1 & org.mat$Level.s==i])
      output.arr[t/year.inp, "TimeAtLvl.F", i]<-mean(org.mat$TimeAtLvl.v[org.mat$Male.s==0 & org.mat$Level.s==i])
      output.arr[t/year.inp, "TimeAtLvl.F.v", i]<-var(org.mat$TimeAtLvl.v[org.mat$Male.s==0 & org.mat$Level.s==i])
      
      output.arr[t/year.inp, "TimeInOrg.M", i]<-mean(org.mat$TimeInOrg.v[org.mat$Male.s==1 & org.mat$Level.s==i])
      output.arr[t/year.inp, "TimeInOrg.M.v", i]<-var(org.mat$TimeInOrg.v[org.mat$Male.s==1 & org.mat$Level.s==i])
      output.arr[t/year.inp, "TimeInOrg.F", i]<-mean(org.mat$TimeInOrg.v[org.mat$Male.s==0 & org.mat$Level.s==i])
      output.arr[t/year.inp, "TimeInOrg.F.v", i]<-var(org.mat$TimeInOrg.v[org.mat$Male.s==0 & org.mat$Level.s==i])
      
      #Age - average age of men/women, variance
      output.arr[t/year.inp,"Age.M",i]<-mean(org.mat$Age.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Age.M.v",i]<-var(org.mat$Age.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Age.F",i]<-mean(org.mat$Age.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Age.F.v",i]<-var(org.mat$Age.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      #Performance - average performance of men/women, variance
      output.arr[t/year.inp,"Perf.M",i]<-mean(org.mat$YrlyPerf.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Perf.M.v",i]<-var(org.mat$YrlyPerf.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Perf.F",i]<-mean(org.mat$YrlyPerf.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Perf.F.v",i]<-var(org.mat$YrlyPerf.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      #Ability - average ability of men/women, variance
      output.arr[t/year.inp,"Ability.M",i]<-mean(org.mat$Ability.s[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Ability.M.v",i]<-var(org.mat$Ability.s[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Ability.F",i]<-mean(org.mat$Ability.s[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp,"Ability.F.v",i]<-var(org.mat$Ability.s[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      #Risktaking - average risktaking propensity of men/women, variance
      output.arr[t/year.inp, "Risk.M",i]<-mean(org.mat$RiskTaking.s[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "Risk.M.v",i]<-var(org.mat$RiskTaking.s[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "Risk.F",i]<-mean(org.mat$RiskTaking.s[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp, "Risk.F.v",i]<-var(org.mat$RiskTaking.s[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      #Hot jobs (count) - total HJ over entire tenure & HJ that year
      output.arr[t/year.inp, "TotalHJ.M",i]<-mean(org.mat$TotalHJ.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalHJ.M.v",i]<-var(org.mat$TotalHJ.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalHJ.F",i]<-mean(org.mat$TotalHJ.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalHJ.F.v",i]<-var(org.mat$TotalHJ.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      output.arr[t/year.inp, "YrlyHJ.M",i]<-mean(org.mat$YrlyHJ.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "YrlyHJ.M.v",i]<-var(org.mat$YrlyHJ.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "YrlyHJ.F",i]<-mean(org.mat$YrlyHJ.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp, "YrlyHJ.F.v",i]<-var(org.mat$YrlyHJ.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      #Hot jobs boost - total HJ boost earned over entire tenure & HJ boost earned that year
      output.arr[t/year.inp, "TotalHJBoost.M",i]<-mean(org.mat$TotalHJBoost.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalHJBoost.M.v",i]<-var(org.mat$TotalHJBoost.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalHJBoost.F",i]<-mean(org.mat$TotalHJBoost.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalHJBoost.F.v",i]<-var(org.mat$TotalHJBoost.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      output.arr[t/year.inp, "YrlyHJBoost.M",i]<-mean(org.mat$YrlyHJBoost.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "YrlyHJBoost.M.v",i]<-var(org.mat$YrlyHJBoost.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "YrlyHJBoost.F",i]<-mean(org.mat$YrlyHJBoost.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp, "YrlyHJBoost.F.v",i]<-var(org.mat$YrlyHJBoost.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      #Months on delay
      output.arr[t/year.inp, "TotalMonthsDelay.M",i]<-mean(org.mat$TotalMonthsDelay.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalMonthsDelay.M.v",i]<-var(org.mat$TotalMonthsDelay.v[org.mat$Male.s == 1 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalMonthsDelay.F",i]<-mean(org.mat$TotalMonthsDelay.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      output.arr[t/year.inp, "TotalMonthsDelay.F.v",i]<-var(org.mat$TotalMonthsDelay.v[org.mat$Male.s == 0 & org.mat$Level.s == i])
      
      #Total promotions (over entire tenure)
      output.arr[t/year.inp, "TotalPromotions.M", i]<-mean(org.mat$TotalPromotions.v[org.mat$Level.s==i & org.mat$Male.s==1])
      output.arr[t/year.inp, "TotalPromotions.M.v", i]<-var(org.mat$TotalPromotions.v[org.mat$Level.s==i & org.mat$Male.s==1])
      output.arr[t/year.inp, "TotalPromotions.F", i]<-mean(org.mat$TotalPromotions.v[org.mat$Level.s==i & org.mat$Male.s==0])
      output.arr[t/year.inp, "TotalPromotions.F.v", i]<-var(org.mat$TotalPromotions.v[org.mat$Level.s==i & org.mat$Male.s==0])
      
    }
    
    #Calculate mean/var of ability to use for external hiring
    levelAbility.mean<-rep(0, levels.inp-1)
    levelAbility.sd<-rep(0, levels.inp-1)
    for(i in (seq(levels.inp, 2, -1))) {
      levelAbility.mean[i-1]<-mean(org.mat$Ability.s[org.mat$Level.s==i])
      levelAbility.sd[i-1]<-sd(org.mat$Ability.s[org.mat$Level.s==i])
    }
    
    #Clear out TO data, keep Emp.ID.s (1), Level.s (2), Group.s (3), Line.s (4), and TO.m (27)
    org.mat[org.mat$TO.m==1, c(5:28, 30:35)]<-0
    
    #10. Fill specified percentage of open positions with external hires
    
    #Mark external hires in levels 2-8
    org.mat$ExtHire.m[org.mat$TO.m==1 & org.mat$Level.s %in% upperLevels.inp]<-sample(c(rep(0, floor((1-upperExtHireRate.inp)*nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% upperLevels.inp,]))), rep(1, floor(upperExtHireRate.inp*nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% upperLevels.inp,]))), sample(0:1, nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% upperLevels.inp,])-(floor((1-upperExtHireRate.inp)*nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% upperLevels.inp,])) + floor(upperExtHireRate.inp*nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% upperLevels.inp,]))), replace = TRUE, prob=c(1-upperExtHireRate.inp, upperExtHireRate.inp))))

    org.mat$ExtHire.m[org.mat$TO.m==1 & org.mat$Level.s %in% lowerLevels.inp & org.mat$Level.s!= 1]<-sample(c(rep(0, floor((1-lowerExtHireRate.inp)*nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% lowerLevels.inp & org.mat$Level.s!= 1,]))), rep(1, floor(lowerExtHireRate.inp*nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% lowerLevels.inp & org.mat$Level.s!= 1,]))), sample(0:1, nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% lowerLevels.inp & org.mat$Level.s!= 1,])-(floor((1-lowerExtHireRate.inp)*nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% lowerLevels.inp & org.mat$Level.s!= 1,])) + floor(lowerExtHireRate.inp*nrow(org.mat[org.mat$TO.m==1 & org.mat$Level.s %in% lowerLevels.inp & org.mat$Level.s!= 1,]))), replace = TRUE, prob=c(1-lowerExtHireRate.inp, lowerExtHireRate.inp))))
        
    #Assign age to external hires
    org.mat$Age.v[org.mat$Level.s %in% upperLevels.inp & org.mat$ExtHire.m==1]<-rpois(length(org.mat$Age.v[org.mat$Level.s %in% upperLevels.inp & org.mat$ExtHire.m==1]), upperAgeCenter.inp)
    org.mat$Age.v[org.mat$Level.s %in% lowerLevels.inp & org.mat$ExtHire.m==1]<-rpois(length(org.mat$Age.v[org.mat$Level.s %in% lowerLevels.inp & org.mat$ExtHire.m==1]), lowerAgeCenter.inp)

    org.mat$Age.v[org.mat$ExtHire.m==1]<-ifelse(org.mat$Age.v[org.mat$ExtHire.m==1]<minAge.inp, minAge.inp, org.mat$Age.v[org.mat$ExtHire.m==1])

    #Assign gender to external hires
    org.mat$Male.s[org.mat$ExtHire.m==1]<-sample(c(rep(0, floor((1-extHire.male.inp)*nrow(org.mat[org.mat$ExtHire.m==1,]))), rep(1, floor(extHire.male.inp*nrow(org.mat[org.mat$ExtHire.m==1,]))), sample(0:1, nrow(org.mat[org.mat$ExtHire.m==1,])-(floor((1-extHire.male.inp)*nrow(org.mat[org.mat$ExtHire.m==1,])) + floor(extHire.male.inp*nrow(org.mat[org.mat$ExtHire.m==1,]))), replace = TRUE, prob=c(1-extHire.male.inp, extHire.male.inp))))

    #Assign ability to external hires
    for(i in (seq(levels.inp, 2, -1))) {
      if(i==levels.inp & sum(org.mat$ExtHire.m[org.mat$Level.s==i])>0){
        org.mat$Ability.s[org.mat$ExtHire.m==1 & org.mat$Level.s==i]<-rnorm(length(org.mat$Ability.s[org.mat$ExtHire.m==1 & org.mat$Level.s==i]), levelAbility.mean[i-1], ability.sd.inp)
      }
      if(i<levels.inp & sum(org.mat$ExtHire.m[org.mat$Level.s==i])>0){
        org.mat$Ability.s[org.mat$ExtHire.m==1 & org.mat$Level.s==i]<-rnorm(length(org.mat$Ability.s[org.mat$ExtHire.m==1 & org.mat$Level.s==i]), levelAbility.mean[i-1], levelAbility.sd[i-1]) 
      }
    }

    #Assign risktaking to external hires
    ##Male risktaking
    org.mat$RiskTaking.s[org.mat$ExtHire.m==1 & org.mat$Male.s==1]<-riskTaking(length(org.mat$RiskTaking.s[org.mat$ExtHire.m==1 & org.mat$Male.s==1]), risktakers.male.inp)
    ##Female risktaking
    org.mat$RiskTaking.s[org.mat$ExtHire.m==1 & org.mat$Male.s==0]<-riskTaking(length(org.mat$RiskTaking.s[org.mat$ExtHire.m==1 & org.mat$Male.s==0]), risktakers.female.inp)
    
    #Assign EH status to external hires
    org.mat$LowerEH.s[org.mat$ExtHire.m==1 & org.mat$Level.s %in% c(1:4)]<-1
    org.mat$UpperEH.s[org.mat$ExtHire.m==1 & org.mat$Level.s %in% c(5:8)]<-1
  
    #11. Promote employees into remaining open positions
    
      for(i in (seq(levels.inp, 2, -1))) {
        if (sum(org.mat$TO.m[org.mat$Level.s==i & org.mat$ExtHire.m==0])>0) {
              
          #Promotions with line/staff bias at top level
          if(i==levels.inp){
            topLine<-sample(0:1, 1, prob=c(.01, .99))
            org.mat$Promote.m[org.mat$Level.s==i-1 & org.mat$TO.m==0 & org.mat$Line.s==topLine & org.mat$YrlyPerf.v == max(org.mat$YrlyPerf.v[org.mat$Level.s==i-1 & org.mat$TO.m==0 & org.mat$Line.s==topLine])]<-1
           
            org.mat$TO.m[org.mat$Level.s==i-1 & org.mat$Promote.m==1]<-1
            
            org.mat$TotalPromotions.v[org.mat$Level.s==i-1 & org.mat$Promote.m==1]<-(org.mat$TotalPromotions.v[org.mat$Level.s==i-1 & org.mat$Promote.m==1]+1)
            
            org.mat[org.mat$TO.m==1 & org.mat$ExtHire.m==0 & org.mat$Level.s==i, c("Line.s", "Male.s", "Original.s", "LowerEH.s", "UpperEH.s", "Ability.s", "RiskTaking.s", "Age.v", "TimeInOrg.v", "TotalPromotions.v", "TotalMonthsDelay.v", "TotalHJ.v", "TotalHJBoost.v")]<-org.mat[org.mat$Promote.m==1 & org.mat$Level.s==i-1, c("Line.s", "Male.s", "Original.s", "LowerEH.s", "UpperEH.s", "Ability.s", "RiskTaking.s", "Age.v", "TimeInOrg.v", "TotalPromotions.v", "TotalMonthsDelay.v", "TotalHJ.v", "TotalHJBoost.v")] 
            
            ###---OUTPUT---###
            output.arr[t/year.inp, "PromotedOut", i-1]<-(sum(org.mat$Promote.m[org.mat$Level.s==i-1])/sum(org.mat$Level.s==i-1)) #Number of promotions out/level below size
            output.arr[t/year.inp, "PromotedOut.M", i-1]<-(sum(org.mat$Male.s[org.mat$Level.s==i-1 & org.mat$Promote.m==1])/sum(org.mat$Promote.m[org.mat$Level.s==i-1])) #% of promotions who were men
            output.arr[t/year.inp, "PromotedOut.F", i-1]<-(sum(org.mat$Male.s[org.mat$Level.s==i-1 & org.mat$Promote.m==1]==0)/sum(org.mat$Promote.m[org.mat$Level.s==i-1])) #% of promotions who were women
            
            output.arr[t/year.inp, "YrlyExtHires", i]<-(sum(org.mat$ExtHire.m[org.mat$Level.s==i]==1)/sum(org.mat$Level.s==i)) #% of level who are (new) external hires
            output.arr[t/year.inp, "YrlyExtHires.M", i]<-(sum(org.mat$ExtHire.m[org.mat$Male.s==1 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==1)) #% of men in level who are (new) external hires
            output.arr[t/year.inp, "YrlyExtHires.F", i]<-(sum(org.mat$ExtHire.m[org.mat$Male.s==0 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==0)) #% of women in level who are (new) external hires
            
            output.arr[t/year.inp, "UpperExtHires", i]<-(sum(org.mat$UpperEH.s[org.mat$Level.s==i]==1)/sum(org.mat$Level.s==i)) #% of level who are external hires from an upper level
            output.arr[t/year.inp, "UpperExtHires.M", i]<-(sum(org.mat$UpperEH.s[org.mat$Male.s==1 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==1)) #% of men in level who are external hires from an upper level
            output.arr[t/year.inp, "UpperExtHires.F", i]<-(sum(org.mat$UpperEH.s[org.mat$Male.s==0 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==0)) #% of women in level who are external hires from an upper level
            
            ##Clear out variables of those who were promoted (keep EmpID, Level, Group, and Line (1-4), TO and Promote (29-30))
            org.mat[org.mat$Level.s==i-1 & org.mat$Promote.m==1, c(5:28, 31:35)]<-0
            
          }
          
          #Promotions below top level
          if(i < levels.inp) {
            
            ##Assign promotions for line
            org.mat$Promote.m[org.mat$Level.s==i-1 & org.mat$TO.m==0 & org.mat$ExtHire.m==0 & org.mat$Line.s==1 & org.mat$YrlyPerf.v %in% head(sort(org.mat$YrlyPerf.v[org.mat$Level.s==i-1 & org.mat$TO.m==0 & org.mat$ExtHire.m==0 & org.mat$Line.s==1], decreasing=TRUE), sum(org.mat$TO.m[org.mat$Level.s==i & org.mat$TO.m==1 & org.mat$ExtHire.m==0 & org.mat$Line.s==1]))]<-1
            
            ##Assign promotions for staff
            org.mat$Promote.m[org.mat$Level.s==i-1 & org.mat$TO.m==0 & org.mat$ExtHire.m==0 & org.mat$Line.s==0 & org.mat$YrlyPerf.v %in% head(sort(org.mat$YrlyPerf.v[org.mat$Level.s==i-1 & org.mat$TO.m==0 & org.mat$ExtHire.m==0 & org.mat$Line.s==0], decreasing=TRUE), sum(org.mat$TO.m[org.mat$Level.s==i & org.mat$TO.m==1 & org.mat$ExtHire.m==0 & org.mat$Line.s==0]))]<-1
            
            ##Mark spots that were promoted as TO
            org.mat$TO.m[org.mat$Level.s==i-1 & org.mat$Promote.m==1]<-1
            
            ##Add to total promotions
            org.mat$TotalPromotions.v[org.mat$Level.s==i-1 & org.mat$Promote.m==1]<-(org.mat$TotalPromotions.v[org.mat$Level.s==i-1 & org.mat$Promote.m==1]+1)
            
            ##Move promoted lines up
            org.mat[org.mat$TO.m==1 & org.mat$ExtHire.m==0 & org.mat$Level.s==i & org.mat$Line.s==1, c("Male.s", "Original.s", "LowerEH.s", "UpperEH.s", "Ability.s", "RiskTaking.s", "Age.v", "TimeInOrg.v", "TotalPromotions.v", "TotalMonthsDelay.v", "TotalHJ.v", "TotalHJBoost.v")]<-org.mat[org.mat$Promote.m==1 & org.mat$Level.s==i-1 & org.mat$Line.s==1, c("Male.s", "Original.s", "LowerEH.s", "UpperEH.s", "Ability.s", "RiskTaking.s", "Age.v", "TimeInOrg.v", "TotalPromotions.v", "TotalMonthsDelay.v", "TotalHJ.v", "TotalHJBoost.v")]
            
            ##Move promoted staff up
            org.mat[org.mat$TO.m==1 & org.mat$ExtHire.m==0 & org.mat$Level.s==i & org.mat$Line.s==0, c("Male.s", "Original.s", "LowerEH.s", "UpperEH.s", "Ability.s", "RiskTaking.s", "Age.v", "TimeInOrg.v", "TotalPromotions.v", "TotalMonthsDelay.v", "TotalHJ.v", "TotalHJBoost.v")]<-org.mat[org.mat$Promote.m==1 & org.mat$Level.s==i-1 & org.mat$Line.s==0, c("Male.s", "Original.s", "LowerEH.s", "UpperEH.s", "Ability.s", "RiskTaking.s", "Age.v", "TimeInOrg.v", "TotalPromotions.v", "TotalMonthsDelay.v", "TotalHJ.v", "TotalHJBoost.v")]
            
            ###---OUTPUT---###
            output.arr[t/year.inp, "PromotedOut", i-1]<-(sum(org.mat$Promote.m[org.mat$Level.s==i-1])/sum(org.mat$Level.s==i-1))
            output.arr[t/year.inp, "PromotedOut.M", i-1]<-(sum(org.mat$Male.s[org.mat$Level.s==i-1 & org.mat$Promote.m==1])/sum(org.mat$Promote.m[org.mat$Level.s==i-1]))
            output.arr[t/year.inp, "PromotedOut.F", i-1]<-(sum(org.mat$Male.s[org.mat$Level.s==i-1 & org.mat$Promote.m==1]==0)/sum(org.mat$Promote.m[org.mat$Level.s==i-1]))
            
            output.arr[t/year.inp, "YrlyExtHires", i]<-(sum(org.mat$ExtHire.m[org.mat$Level.s==i]==1)/sum(org.mat$Level.s==i)) #% of level who are (new) external hires
            output.arr[t/year.inp, "YrlyExtHires.M", i]<-(sum(org.mat$ExtHire.m[org.mat$Male.s==1 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==1)) #% of men in level who are (new) external hires
            output.arr[t/year.inp, "YrlyExtHires.F", i]<-(sum(org.mat$ExtHire.m[org.mat$Male.s==0 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==0)) #% of women in level who are (new) external hires
            
            output.arr[t/year.inp, "LowerExtHires", i]<-(sum(org.mat$LowerEH.s[org.mat$Level.s==i]==1)/sum(org.mat$Level.s==i)) #% of level who are external hires from a lower level
            output.arr[t/year.inp, "LowerExtHires.M", i]<-(sum(org.mat$LowerEH.s[org.mat$Male.s==1 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==1)) #% of men in level who are external hires from a lower level
            output.arr[t/year.inp, "LowerExtHires.F", i]<-(sum(org.mat$LowerEH.s[org.mat$Male.s==0 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==0)) #% of women in level who are external hires from a lower level
            
            output.arr[t/year.inp, "UpperExtHires", i]<-(sum(org.mat$UpperEH.s[org.mat$Level.s==i]==1)/sum(org.mat$Level.s==i)) #% of level who are external hires from an upper level
            output.arr[t/year.inp, "UpperExtHires.M", i]<-(sum(org.mat$UpperEH.s[org.mat$Male.s==1 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==1)) #% of men in level who are external hires from an upper level
            output.arr[t/year.inp, "UpperExtHires.F", i]<-(sum(org.mat$UpperEH.s[org.mat$Male.s==0 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==i]==0)) #% of women in level who are external hires from an upper level
            
            ##Clear out variables of those who were promoted (keep EmpID, Level, Group, and Line (1-4), TO and Promote (25-26))
            org.mat[org.mat$Level.s==i-1 & org.mat$Promote.m==1, c(5:28, 31:35)]<-0
          }
        }
      }

            
      #12. Fill open positions in lowest level of organization with external hires
    
      org.mat$ExtHire.m[org.mat$Level.s==1 & org.mat$TO.m==1]<-1
      org.mat$LowerEH.s[org.mat$Level.s==1 & org.mat$TO.m==1]<-1
      ##Assign gender, line/staff, ability, risktaking, and age
      ###Gender
      org.mat$Male.s[org.mat$Level.s==1 & org.mat$TO.m==1]<-sample(c(sample(0:1, length(org.mat$Male.s[org.mat$Level.s==1 & org.mat$TO.m==1])-(floor((1-extHire.male.inp)*length(org.mat$Male.s[org.mat$Level.s==1 & org.mat$TO.m==1]))+floor(extHire.male.inp*length(org.mat$Male.s[org.mat$Level.s==1 & org.mat$TO.m==1]))), replace=T, prob=c(1-extHire.male.inp, extHire.male.inp)), rep(0, floor((1-extHire.male.inp)*length(org.mat$Male.s[org.mat$Level.s==1 & org.mat$TO.m==1]))), rep(1, floor(extHire.male.inp*length(org.mat$Male.s[org.mat$Level.s==1 & org.mat$TO.m==1])))))
                  
      ###Ability
      org.mat$Ability.s[org.mat$Level.s==1 & org.mat$TO.m==1]<-rnorm(length(org.mat$Ability.s[org.mat$Level.s==1 & org.mat$TO.m==1]), ability.mean.inp, ability.sd.inp)
                  
      ###Risktaking
      ####Male risktaking
      org.mat$RiskTaking.s[org.mat$Level.s==1 & org.mat$TO.m==1 & org.mat$Male.s==1]<-riskTaking(length(org.mat$RiskTaking.s[org.mat$Level.s==1 & org.mat$TO.m==1 & org.mat$Male.s==1]), risktakers.male.inp)
      ####Female risktaking
      org.mat$RiskTaking.s[org.mat$Level.s==1 & org.mat$TO.m==1 & org.mat$Male.s==0]<-riskTaking(length(org.mat$RiskTaking.s[org.mat$Level.s==1 & org.mat$TO.m==1 & org.mat$Male.s==0]), risktakers.female.inp)
                  
      ###Age
      org.mat$Age.v[org.mat$Level.s==1 & org.mat$TO.m==1]<-rpois(length(org.mat$Age.v[org.mat$Level.s==1 & org.mat$TO.m==1]), lowerAgeCenter.inp)
      org.mat$Age.v[org.mat$Level.s==1 & org.mat$TO.m==1]<-ifelse(org.mat$Age.v[org.mat$Level.s==1 & org.mat$TO.m==1]<minAge.inp, minAge.inp, org.mat$Age.v[org.mat$Level.s==1 & org.mat$TO.m==1])
      
      ###---OUTPUT---###
      output.arr[t/year.inp, "YrlyExtHires", 1]<-(sum(org.mat$ExtHire.m[org.mat$Level.s==1]==1)/sum(org.mat$Level.s==1)) #% of level who are (new) external hires
      output.arr[t/year.inp, "YrlyExtHires.M", 1]<-(sum(org.mat$ExtHire.m[org.mat$Male.s==1 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==1]==1)) #% of men in level who are (new) external hires
      output.arr[t/year.inp, "YrlyExtHires.F", 1]<-(sum(org.mat$ExtHire.m[org.mat$Male.s==0 & org.mat$Level.s==i]==1)/sum(org.mat$Male.s[org.mat$Level.s==1]==0)) #% of women in level who are (new) external hires
      
      output.arr[t/year.inp, "LowerExtHires", 1]<-(sum(org.mat$LowerEH.s[org.mat$Level.s==1]==1)/sum(org.mat$Level.s==1)) #% of level who are external hires from a lower level
      output.arr[t/year.inp, "LowerExtHires.M", 1]<-(sum(org.mat$LowerEH.s[org.mat$Male.s==1 & org.mat$Level.s==1]==1)/sum(org.mat$Male.s[org.mat$Level.s==1]==1)) #% of men in level who are external hires from a lower level
      output.arr[t/year.inp, "LowerExtHires.F", 1]<-(sum(org.mat$LowerEH.s[org.mat$Male.s==0 & org.mat$Level.s==1]==1)/sum(org.mat$Male.s[org.mat$Level.s==1]==0)) #% of women in level who are external hires from a lower level
      
#Clear out yearly perf, HJ count, and HJ boost for everyone
org.mat$YrlyPerf.v<-0
org.mat$YrlyHJ.v<-0
org.mat$YrlyHJBoost.v<-0

# annTime <- Sys.time() - startAnnTime  
# Sys.sleep(.01)
# print(paste("Finished annual computations for iteration ",t,". Time elapsed: ", annTime, " ", units(annTime), sep = ""))

  } # Closes if statement for annual calculations

# itTime <- Sys.time() - startItTime  
# Sys.sleep(.01)
# print(paste("Finished iteration ",t,". Time elapsed: ", itTime, " ", units(itTime), sep = ""))
# print(paste("Proportion original employees remaining: ", sum(org.mat$Original.s)/nrow(org.mat), sep=""))
} # Closes while loop
  output.arr<-output.arr[1:(t/year.inp),,]
  output <- list("Output.arr" = output.arr, "Org.mat" = org.mat)
  return(output)
  
  print("Finished an organization")
} # Closes function