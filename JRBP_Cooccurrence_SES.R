##### JRBP ants co-occurrence analysis set-up ######

ReadNew = read.csv("~/Documents/Current_Projects/Ants/JRBP_Ants_SitesPerYear_NoClimateData.csv", header = T)

# Separate into Fall and Spring surveys
ReadNewF = ReadNew[ReadNew$season == "Fall",]
ReadNewS = ReadNew[ReadNew$season == "Spring",]


#### C-scores between L. humile and P. imparis: Fall surveys ####

# Calculate observed C-score for each year
year = unique(ReadNewF$year)
CscoreObs = vector()
for (i in 1:length(year))
 {year = unique(ReadNewF$year)[i]
  Data = ReadNewF[ReadNewF$year == year,]
CscoreObs[i] = C.score(Data[,c(9,14)])
}

# Mean permuted C-score for each year
year = unique(ReadNewF$year)
CscoreSim = vector()
CscoreSimSd = vector()
for (i in 1:length(year))
 {year = unique(ReadNewF$year)[i]
  Data = ReadNewF[ReadNewF$year == year,]

holder = vector()
  for (j in 1:5000){
holder[j] = C.score(as.matrix(cbind(sample(Data$Linepithema.humile), sample(Data$Prenolepis.imparis))))}
CscoreSim[i] = mean(holder)
CscoreSimSd[i] = sd(holder)
}

year = unique(ReadNewF$year)
DfF = cbind.data.frame(year, "Fall", CscoreObs, CscoreSim, CscoreSimSd)
DfF$SES = (DfF$CscoreObs - DfF$CscoreSim)/ DfF$CscoreSimSd
colnames(DfF)[2] = "season"


#### C-scores between L. humile and P. imparis: Spring surveys ####

# Calculate observed C-score for each year
year = unique(ReadNewS$year)
CscoreObsS = vector()
for (i in 1:length(year))
{year = unique(ReadNewS$year)[i]
Data = ReadNewS[ReadNewS$year == year,]
CscoreObsS[i] = C.score(Data[,c(9,14)])
}


# Mean permuted C-score for each year
year = unique(ReadNewS$year)
CscoreSimS = vector()
CscoreSimSdS = vector()
for (i in 1:length(year))
{year = unique(ReadNewS$year)[i]
Data = ReadNewS[ReadNewS$year == year,]

holder = vector()
for (j in 1:5000){
  holder[j] = C.score(as.matrix(cbind(sample(Data$Linepithema.humile), sample(Data$Prenolepis.imparis))))}
CscoreSimS[i] = mean(holder)
CscoreSimSdS[i] = sd(holder)
}

year = unique(ReadNewS$year)
DfS = cbind.data.frame(year, "Spring", CscoreObsS, CscoreSimS, CscoreSimSdS)
DfS$SES = (DfS$CscoreObsS - DfS$CscoreSimS)/ DfS$CscoreSimSdS
colnames(DfS) = colnames(DfF)
DfAll = rbind.data.frame(DfF, DfS)

#write.csv(DfAll, "~/Downloads/DfAll.csv")
