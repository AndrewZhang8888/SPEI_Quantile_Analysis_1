library(dplyr)
library(ggplot2)
library(tidyverse)
CBAI <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/bai_official.csv", sep = ",", header = TRUE)
#Growing Season is June-July-August
CBAI1 <- filter(CBAI, AvgGSHighTemp < 29.08186)
CBAI2 <- filter(CBAI, 29.08186 <= AvgGSHighTemp & AvgGSHighTemp <= 30.00359)
CBAI3 <- filter(CBAI, 30.00359 <= AvgGSHighTemp & AvgGSHighTemp <= 30.92533)
CBAI4 <- filter(CBAI, 30.92533 < AvgGSHighTemp)
CBAI5 <- rbind(CBAI1, CBAI2, CBAI3, CBAI4)
CBAI5[,26] = 0
CBAI5[1:7,26] = "0-25%"
CBAI5[8:9,26] = "25-50%"
CBAI5[10:19,26] = "50-75%"
CBAI5[20:24,26] = "75-100%"
CBAI5 <- group_by(CBAI5, V26)
CBAI6 <- summarize(CBAI5, N_Anom1 = mean(N1_2anom), N_Anom2 = mean(N1_4anom), 
                   N_Anom3 = mean(N1_6anom), sd_N1 = sd(N1_2anom), sd_N2 = sd(N1_4anom), 
                   sd_N3 = sd(N1_6anom))
CBAI7 <- gather(CBAI6, Location, Anomaly, c(N_Anom1, N_Anom2, N_Anom3))
CBAI7 <- select(CBAI7, Location, Anomaly, V26)
CBAI8 <- gather(CBAI6, Location, SD, c(sd_N1, sd_N2, sd_N3))
CBAI8 <- select(CBAI8, SD )
CBAI9 <- cbind(CBAI7, CBAI8)
CBAI9[c(1:4),1] = "North1"
CBAI9[c(5:8),1] = "North2"
CBAI9[c(9:12),1] = "North3"
CBAI9$Anomaly <- CBAI9$Anomaly - 1
X <- 0.7
ggplot(CBAI9, aes(x = V26, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1,1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

DBAI <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/bai_official.csv", sep = ",", header = TRUE)
#Growing Season is June-July-August
DBAI1 <- filter(DBAI, AvgGSHighTemp < 29.08186)
DBAI2 <- filter(DBAI, 29.08186 <= AvgGSHighTemp & AvgGSHighTemp <= 30.00359)
DBAI3 <- filter(DBAI, 30.00359 <= AvgGSHighTemp & AvgGSHighTemp <= 30.92533)
DBAI4 <- filter(DBAI, 30.92533 < AvgGSHighTemp)
DBAI5 <- rbind(DBAI1, DBAI2, DBAI3, DBAI4)
DBAI5[,26] = 0
DBAI5[1:7,26] = "0-25%"
DBAI5[8:9,26] = "25-50%"
DBAI5[10:19,26] = "50-75%"
DBAI5[20:24,26] = "75-100%"
DBAI5 <- group_by(DBAI5, V26)
DBAI6 <- summarize(DBAI5, S_Anom1 = mean(S1_2anom), S_Anom2 = mean(S1_4anom), 
                   S_Anom3 = mean(S1_6anom), sd_S1 = sd(S1_2anom), sd_S2 = sd(S1_4anom), 
                   sd_S3 = sd(S1_6anom))
DBAI7 <- gather(DBAI6, Location, Anomaly, c(S_Anom1, S_Anom2, S_Anom3))
DBAI7 <- select(DBAI7, Location, Anomaly, V26)
DBAI8 <- gather(DBAI6, Location, SD, c(sd_S1, sd_S2, sd_S3))
DBAI8 <- select(DBAI8, SD )
DBAI9 <- cbind(DBAI7, DBAI8)
DBAI9[c(1:4),1] = "South1"
DBAI9[c(5:8),1] = "South2"
DBAI9[c(9:12),1] = "South3"
DBAI9$Anomaly <- DBAI9$Anomaly - 1
X <- 0.7
ggplot(DBAI9, aes(x = V26, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1,1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

EBAI <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/bai_official.csv", sep = ",", header = TRUE)
#Growing Season is June-July-August
EBAI1 <- filter(EBAI, AvgGSHighTemp < 29.08186)
EBAI2 <- filter(EBAI, 29.08186 <= AvgGSHighTemp & AvgGSHighTemp <= 30.00359)
EBAI3 <- filter(EBAI, 30.00359 <= AvgGSHighTemp & AvgGSHighTemp <= 30.92533)
EBAI4 <- filter(EBAI, 30.92533 < AvgGSHighTemp)
EBAI5 <- rbind(EBAI1, EBAI2, EBAI3, EBAI4)
EBAI5[,26] = 0
EBAI5[1:7,26] = "0-25%"
EBAI5[8:9,26] = "25-50%"
EBAI5[10:19,26] = "50-75%"
EBAI5[20:24,26] = "75-100%"
EBAI5 <- group_by(EBAI5, V26)
EBAI6 <- summarize(EBAI5, V_Anom1 = mean(V1_2anom), V_Anom2 = mean(V1_4anom), 
                   V_Anom3 = mean(V1_6anom), sd_V1 = sd(V1_2anom), sd_V2 = sd(V1_4anom), 
                   sd_V3 = sd(V1_6anom))
EBAI7 <- gather(EBAI6, Location, Anomaly, c(V_Anom1, V_Anom2, V_Anom3))
EBAI7 <- select(EBAI7, Location, Anomaly, V26)
EBAI8 <- gather(EBAI6, Location, SD, c(sd_V1, sd_V2, sd_V3))
EBAI8 <- select(EBAI8, SD)
EBAI9 <- cbind(EBAI7, EBAI8)
EBAI9[c(1:4),1] = "Valley1"
EBAI9[c(5:8),1] = "Valley2"
EBAI9[c(9:12),1] = "Valley3"
EBAI9$Anomaly <- EBAI9$Anomaly - 1
X <- 0.7
ggplot(EBAI9, aes(x = V26, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1,1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

FBAI <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/MapleDataRefined.csv", sep = ",", header = TRUE)
#Growing Season is June-July-August
FBAI1 <- filter(FBAI, AvgGSHighTemp < 29.08186)
FBAI2 <- filter(FBAI, 29.08186 <= AvgGSHighTemp & AvgGSHighTemp <= 30.00359)
FBAI3 <- filter(FBAI, 30.00359 <= AvgGSHighTemp & AvgGSHighTemp <= 30.92533)
FBAI4 <- filter(FBAI, 30.92533 < AvgGSHighTemp)
FBAI5 <- rbind(FBAI1, FBAI2, FBAI3, FBAI4)
FBAI5[,22] = 0
FBAI5[1:7,22] = "0-25%"
FBAI5[8:9,22] = "25-50%"
FBAI5[10:19,22] = "50-75%"
FBAI5[20:24,22] = "75-100%"
FBAI5 <- group_by(FBAI5, V22)
FBAI6 <- summarize(FBAI5, N_Anom1 = mean(N_1Anomaly), N_Anom2 = mean(N_3Anomaly), 
                   N_Anom3 = mean(N_5Anomaly), sd_N1 = sd(N_1Anomaly), sd_N2 = sd(N_3Anomaly), 
                   sd_N3 = sd(N_5Anomaly))
FBAI7 <- gather(FBAI6, Location, Anomaly, c(N_Anom1, N_Anom2, N_Anom3))
FBAI7 <- select(FBAI7, Location, Anomaly, V22)
FBAI8 <- gather(FBAI6, Location, SD, c(sd_N1, sd_N2, sd_N3))
FBAI8 <- select(FBAI8, SD)
FBAI9 <- cbind(FBAI7, FBAI8)
FBAI9[c(1:4),1] = "NorthMap1"
FBAI9[c(5:8),1] = "NorthMap2"
FBAI9[c(9:12),1] = "NorthMap3"
FBAI9$Anomaly <- FBAI9$Anomaly - 1
FBAI9 <- na.omit(FBAI9)
X <- 0.7
ggplot(FBAI9, aes(x = V22, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1.1,1.1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

GBAI <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/MapleDataRefined.csv", sep = ",", header = TRUE)
#Growing Season is June-July-August
GBAI1 <- filter(GBAI, AvgGSHighTemp < 29.08186)
GBAI2 <- filter(GBAI, 29.08186 <= AvgGSHighTemp & AvgGSHighTemp <= 30.00359)
GBAI3 <- filter(GBAI, 30.00359 <= AvgGSHighTemp & AvgGSHighTemp <= 30.92533)
GBAI4 <- filter(GBAI, 30.92533 < AvgGSHighTemp)
GBAI5 <- rbind(GBAI1, GBAI2, GBAI3, GBAI4)
GBAI5[,22] = 0
GBAI5[1:7,22] = "0-25%"
GBAI5[8:9,22] = "25-50%"
GBAI5[10:19,22] = "50-75%"
GBAI5[20:24,22] = "75-100%"
GBAI5 <- group_by(GBAI5, V22)
GBAI6 <- summarize(GBAI5, N_Anom1 = mean(S_1Anomaly), N_Anom2 = mean(S_3Anomaly), 
                   sd_N1 = sd(S_1Anomaly), sd_N2 = sd(S_3Anomaly))
GBAI7 <- gather(GBAI6, Location, Anomaly, c(N_Anom1, N_Anom2))
GBAI7 <- select(GBAI7, Location, Anomaly, V22)
GBAI8 <- gather(GBAI6, Location, SD, c(sd_N1, sd_N2))
GBAI8 <- select(GBAI8, SD)
GBAI9 <- cbind(GBAI7, GBAI8)
GBAI9[c(1:4),1] = "SouthMap1"
GBAI9[c(5:8),1] = "SouthMap2"
GBAI9$Anomaly <- GBAI9$Anomaly - 1
X <- 0.7
ggplot(GBAI9, aes(x = V22, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1.1,1.1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")

HBAI <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/MapleDataRefined.csv", sep = ",", header = TRUE)
#Growing Season is June-July-August
HBAI1 <- filter(HBAI, AvgGSHighTemp < 29.08186)
HBAI2 <- filter(HBAI, 29.08186 <= AvgGSHighTemp & AvgGSHighTemp <= 30.00359)
HBAI3 <- filter(HBAI, 30.00359 <= AvgGSHighTemp & AvgGSHighTemp <= 30.92533)
HBAI4 <- filter(HBAI, 30.92533 < AvgGSHighTemp)
HBAI5 <- rbind(HBAI1, HBAI2, HBAI3, HBAI4)
HBAI5[,22] = 0
HBAI5[1:7,22] = "0-25%"
HBAI5[8:9,22] = "25-50%"
HBAI5[10:19,22] = "50-75%"
HBAI5[20:24,22] = "75-100%"
HBAI5 <- group_by(HBAI5, V22)
HBAI6 <- summarize(HBAI5, V_Anom1 = mean(V_1Anomaly), V_Anom2 = mean(V_3Anomaly), 
                   V_Anom3 = mean(V_5Anomaly), sd_V1 = sd(V_1Anomaly), sd_V2 = sd(V_3Anomaly), 
                   sd_V3 = sd(V_5Anomaly))
HBAI7 <- gather(HBAI6, Location, Anomaly, c(V_Anom1, V_Anom2, V_Anom3))
HBAI7 <- select(HBAI7, Location, Anomaly, V22)
HBAI8 <- gather(HBAI6, Location, SD, c(sd_V1, sd_V2, sd_V3))
HBAI8 <- select(HBAI8, SD)
HBAI9 <- cbind(HBAI7, HBAI8)
HBAI9[c(1:4),1] = "ValleyMap1"
HBAI9[c(5:8),1] = "ValleyMap2"
HBAI9[c(9:12),1] = "ValleyMap3"
HBAI9$Anomaly <- HBAI9$Anomaly - 1
HBAI9 <- na.omit(HBAI9)
X <- 0.7
ggplot(HBAI9, aes(x = V22, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-SD, ymax=Anomaly+SD), position = position_dodge(X))+
  ylim(-1.1,1.1)+
  theme(axis.text.x = element_text(vjust = c(0.4,0.8)))+
  xlab("Percentile")+
  ylab("Anomaly")