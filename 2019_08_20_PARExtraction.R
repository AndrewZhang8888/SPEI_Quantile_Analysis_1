library(tidyverse)
rad <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/updated_ridgetop_1995_2019_new(Refined).csv", sep = ",", header = TRUE)
rad1 <- select(rad, Month, Day, Year, Rad)
rad2 <- filter(rad1, Year >= 1996)
rad3 <- group_by(rad2, Day, Month, Year)
rad4 <- summarize(rad3, MaxRad = max(Rad))
rad5 <- filter(rad4, Month %in% c(6,7,8))
rad5 <- na.omit(rad5)
rad5 <- filter(rad5, MaxRad != 7999)
radq <- qnorm(seq(0.25, 0.75, 0.25), mean(rad5$MaxRad), sd(rad5$MaxRad))
rad6 <- group_by(rad5, Year)
rad7 <- summarize(rad6, GSMaxRad = mean(MaxRad))
rad8 <- select(rad7, GSMaxRad)

Rerad <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/bai_official.csv", sep = ",", header = TRUE)
Rerad1 <- Rerad[c(2:24),]
Rerad2 <- cbind(Rerad1, rad8)

Arad <- Rerad2
Arad1 <- filter(Arad, GSMaxRad < 3409.771)
Arad2 <- filter(Arad, 3409.771 <= GSMaxRad & GSMaxRad <= 4441.493)
Arad3 <- filter(Arad, 4441.493 <= GSMaxRad & GSMaxRad <= 5473.214)
Arad4 <- filter(Arad, 5473.214 < GSMaxRad)
Arad5 <- rbind(Arad1, Arad2, Arad3, Arad4)
Arad5[,27] = 0
Arad5[1:4,27] = "0-25%"
Arad5[5:7,27] = "25-50%"
Arad5[8:21,27] = "50-75%"
Arad5[22:23,27] = "75-100%"
Arad5 <- group_by(Arad5, V27)
Arad6 <- summarize(Arad5, N_Anom1 = mean(N1_2anom), N_Anom2 = mean(N1_4anom), 
                   N_Anom3 = mean(N1_6anom), sd_N1 = sd(N1_2anom), sd_N2 = sd(N1_4anom), 
                   sd_N3 = sd(N1_6anom))
Arad7 <- gather(Arad6, Location, Anomaly, c(N_Anom1, N_Anom2, N_Anom3))
Arad7 <- select(Arad7, Location, Anomaly, V27)
Arad8 <- gather(Arad6, Location, SD, c(sd_N1, sd_N2, sd_N3))
Arad8 <- select(Arad8, SD )
Arad9 <- cbind(Arad7, Arad8)
Arad9[c(1:4),1] = "North1"
Arad9[c(5:8),1] = "North2"
Arad9[c(9:12),1] = "North3"
Arad9$Anomaly <- Arad9$Anomaly - 1
X <- 0.7
ggplot(Arad9, aes(x = V27, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1,1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

Brad <- Rerad2
Brad1 <- filter(Brad, GSMaxRad < 3409.771)
Brad2 <- filter(Brad, 3409.771 <= GSMaxRad & GSMaxRad <= 4441.493)
Brad3 <- filter(Brad, 4441.493 <= GSMaxRad & GSMaxRad <= 5473.214)
Brad4 <- filter(Brad, 5473.214 < GSMaxRad)
Brad5 <- rbind(Brad1, Brad2, Brad3, Brad4)
Brad5[,27] = 0
Brad5[1:4,27] = "0-25%"
Brad5[5:7,27] = "25-50%"
Brad5[8:21,27] = "50-75%"
Brad5[22:23,27] = "75-100%"
Brad5 <- group_by(Brad5, V27)
Brad6 <- summarize(Brad5, S_Anom1 = mean(S1_2anom), S_Anom2 = mean(S1_4anom), 
                   S_Anom3 = mean(S1_6anom), sd_S1 = sd(S1_2anom), sd_S2 = sd(S1_4anom), 
                   sd_S3 = sd(S1_6anom))
Brad7 <- gather(Brad6, Location, Anomaly, c(S_Anom1, S_Anom2, S_Anom3))
Brad7 <- select(Brad7, Location, Anomaly, V27)
Brad8 <- gather(Brad6, Location, SD, c(sd_S1, sd_S2, sd_S3))
Brad8 <- select(Brad8, SD )
Brad9 <- cbind(Brad7, Brad8)
Brad9[c(1:4),1] = "South1"
Brad9[c(5:8),1] = "South2"
Brad9[c(9:12),1] = "South3"
Brad9$Anomaly <- Brad9$Anomaly - 1
X <- 0.7
ggplot(Brad9, aes(x = V27, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1,1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

Crad <- Rerad2
Crad1 <- filter(Crad, GSMaxRad < 3409.771)
Crad2 <- filter(Crad, 3409.771 <= GSMaxRad & GSMaxRad <= 4441.493)
Crad3 <- filter(Crad, 4441.493 <= GSMaxRad & GSMaxRad <= 5473.214)
Crad4 <- filter(Crad, 5473.214 < GSMaxRad)
Crad5 <- rbind(Crad1, Crad2, Crad3, Crad4)
Crad5[,27] = 0
Crad5[1:4,27] = "0-25%"
Crad5[5:7,27] = "25-50%"
Crad5[8:21,27] = "50-75%"
Crad5[22:23,27] = "75-100%"
Crad5 <- group_by(Crad5, V27)
Crad6 <- summarize(Crad5, V_Anom1 = mean(V1_2anom), V_Anom2 = mean(V1_4anom), 
                   V_Anom3 = mean(V1_6anom), sd_V1 = sd(V1_2anom), sd_V2 = sd(V1_4anom), 
                   sd_V3 = sd(V1_6anom))
Crad7 <- gather(Crad6, Location, Anomaly, c(V_Anom1, V_Anom2, V_Anom3))
Crad7 <- select(Crad7, Location, Anomaly, V27)
Crad8 <- gather(Crad6, Location, SD, c(sd_V1, sd_V2, sd_V3))
Crad8 <- select(Crad8, SD)
Crad9 <- cbind(Crad7, Crad8)
Crad9[c(1:4),1] = "Valley1"
Crad9[c(5:8),1] = "Valley2"
Crad9[c(9:12),1] = "Valley3"
Crad9$Anomaly <- Crad9$Anomaly - 1
X <- 0.7
ggplot(Crad9, aes(x = V27, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1,1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

Rerad3 <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/MapleDataRefined.csv", sep = ",", header = TRUE)
Rerad4 <- Rerad3[c(2:24),]
Rerad5 <- cbind(Rerad4, rad8)

Drad <- Rerad5
Drad1 <- filter(Drad, GSMaxRad < 3409.771)
Drad2 <- filter(Drad, 3409.771 <= GSMaxRad & GSMaxRad <= 4441.493)
Drad3 <- filter(Drad, 4441.493 <= GSMaxRad & GSMaxRad <= 5473.214)
Drad4 <- filter(Drad, 5473.214 < GSMaxRad)
Drad5 <- rbind(Drad1, Drad2, Drad3, Drad4)
Drad5[,23] = 0
Drad5[1:4,23] = "0-25%"
Drad5[5:7,23] = "25-50%"
Drad5[8:21,23] = "50-75%"
Drad5[22:23,23] = "75-100%"
Drad5 <- group_by(Drad5, V23)
Drad6 <- summarize(Drad5, N_Anom1 = mean(N_1Anomaly), N_Anom2 = mean(N_3Anomaly), 
                   N_Anom3 = mean(N_5Anomaly), sd_N1 = sd(N_1Anomaly), sd_N2 = sd(N_3Anomaly), 
                   sd_N3 = sd(N_5Anomaly))
Drad7 <- gather(Drad6, Location, Anomaly, c(N_Anom1, N_Anom2, N_Anom3))
Drad7 <- select(Drad7, Location, Anomaly, V23)
Drad8 <- gather(Drad6, Location, SD, c(sd_N1, sd_N2, sd_N3))
Drad8 <- select(Drad8, SD)
Drad9 <- cbind(Drad7, Drad8)
Drad9[c(1:4),1] = "NorthMap1"
Drad9[c(5:8),1] = "NorthMap2"
Drad9[c(9:12),1] = "NorthMap3"
Drad9$Anomaly <- Drad9$Anomaly - 1
Drad9 <- na.omit(Drad9)
X <- 0.7
ggplot(Drad9, aes(x = V23, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1.2,1.2)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

Erad <- Rerad5
Erad1 <- filter(Erad, GSMaxRad < 3409.771)
Erad2 <- filter(Erad, 3409.771 <= GSMaxRad & GSMaxRad <= 4441.493)
Erad3 <- filter(Erad, 4441.493 <= GSMaxRad & GSMaxRad <= 5473.214)
Erad4 <- filter(Erad, 5473.214 < GSMaxRad)
Erad5 <- rbind(Erad1, Erad2, Erad3, Erad4)
Erad5[,23] = 0
Erad5[1:4,23] = "0-25%"
Erad5[5:7,23] = "25-50%"
Erad5[8:21,23] = "50-75%"
Erad5[22:23,23] = "75-100%"
Erad5 <- group_by(Erad5, V23)
Erad6 <- summarize(Erad5, N_Anom1 = mean(S_1Anomaly), N_Anom2 = mean(S_3Anomaly), 
                   sd_N1 = sd(S_1Anomaly), sd_N2 = sd(S_3Anomaly))
Erad7 <- gather(Erad6, Location, Anomaly, c(N_Anom1, N_Anom2))
Erad7 <- select(Erad7, Location, Anomaly, V23)
Erad8 <- gather(Erad6, Location, SD, c(sd_N1, sd_N2))
Erad8 <- select(Erad8, SD)
Erad9 <- cbind(Erad7, Erad8)
Erad9[c(1:4),1] = "SouthMap1"
Erad9[c(5:8),1] = "SouthMap2"
Erad9$Anomaly <- Erad9$Anomaly - 1
X <- 0.7
ggplot(Erad9, aes(x = V23, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1.1,1.1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

Frad <- Rerad5
Frad1 <- filter(Frad, GSMaxRad < 3409.771)
Frad2 <- filter(Frad, 3409.771 <= GSMaxRad & GSMaxRad <= 4441.493)
Frad3 <- filter(Frad, 4441.493 <= GSMaxRad & GSMaxRad <= 5473.214)
Frad4 <- filter(Frad, 5473.214 < GSMaxRad)
Frad5 <- rbind(Frad1, Frad2, Frad3, Frad4)
Frad5[,23] = 0
Frad5[1:4,23] = "0-25%"
Frad5[5:7,23] = "25-50%"
Frad5[8:21,23] = "50-75%"
Frad5[22:23,23] = "75-100%"
Frad5 <- group_by(Frad5, V23)
Frad6 <- summarize(Frad5, V_Anom1 = mean(V_1Anomaly), V_Anom2 = mean(V_3Anomaly), 
                   V_Anom3 = mean(V_5Anomaly), sd_V1 = sd(V_1Anomaly), sd_V2 = sd(V_3Anomaly), 
                   sd_V3 = sd(V_5Anomaly))
Frad7 <- gather(Frad6, Location, Anomaly, c(V_Anom1, V_Anom2, V_Anom3))
Frad7 <- select(Frad7, Location, Anomaly, V23)
Frad8 <- gather(Frad6, Location, SD, c(sd_V1, sd_V2, sd_V3))
Frad8 <- select(Frad8, SD)
Frad9 <- cbind(Frad7, Frad8)
Frad9[c(1:4),1] = "ValleyMap1"
Frad9[c(5:8),1] = "ValleyMap2"
Frad9[c(9:12),1] = "ValleyMap3"
Frad9$Anomaly <- Frad9$Anomaly - 1
Frad9 <- na.omit(Frad9)
X <- 0.7
ggplot(Frad9, aes(x = V23, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1.1,1.1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")