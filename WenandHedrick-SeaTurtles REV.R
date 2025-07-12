#Wen and Hedrick, in review
#High Field Pivotal Temperature of Olive Ridley Sea Turtles (Lepidochelys olivacea) 
#Last modified July 10 2025 BPH

library(car) #v 3.1-2
library(embryogrowth) #v 9.1
library(scales) #v 1.3.0
library(ggplot2) #v 3.5.1
library(ggpubr) #v 0.6.0
library(cowplot) #v 1.1.1
library(reshape) #v 0.8.9
library(nlme) #v 3.1-159
library(multcomp) #v 1.4-25
library(dplyr) #v 1.1.4
library(tidyverse) #v 1.3.2

#Set palettes 
#Red to blue color gradient for plot, used this to color the boxes in the nest diagram. 
#Rounded % to nearest 5 where 0% is blue and 100% is red
colfunc <- colorRampPalette(c("blue", "red"))
cols <- colfunc(21)

#Colors for treatments
colorBlindBlack5  <- c("#D55E00", 
                       "#56B4E9", 
                       "#0072B2", 
                       "#009E73", 
                       "#CC79A7")
pie(rep(1, 5), col = colorBlindBlack5)


#Set working directory
setwd("~/Desktop/Current Projects/Florence-MS/FlorenceRCode/FinalRCode") #Brandon

#Look at nest variability across the day

#Present all temperature data during the study for the different nests.
nest <- read.csv("HourTempDataNest.csv")

tempMinMax <- nest %>%
  group_by(Day) %>%
  summarise_at(vars(Control, 
                    A1, A3, A5, A7, A9,
                    B2, B4, B6, B8, B10,
                    C1, C3, C5, C7, C9,
                    D2, D4, D6, D8, D10,
                    E1, E3, E5, E7, E9,
                    Control1_30cm, Control2_30cm),  list(min, max))

diffs <- data.frame(Day = tempMinMax$Day,
                    Control = tempMinMax$Control_fn2 - tempMinMax$Control_fn1,
                    A1 = tempMinMax$A1_fn2 - tempMinMax$A1_fn1,
                    A3 = tempMinMax$A3_fn2 - tempMinMax$A3_fn1,
                    A5 = tempMinMax$A5_fn2 - tempMinMax$A5_fn1,
                    A7 = tempMinMax$A7_fn2 - tempMinMax$A7_fn1,
                    A9 = tempMinMax$A9_fn2 - tempMinMax$A9_fn1,
                    
                    B2 = tempMinMax$B2_fn2 - tempMinMax$B2_fn1,
                    B4 = tempMinMax$B4_fn2 - tempMinMax$B4_fn1,
                    B6 = tempMinMax$B6_fn2 - tempMinMax$B6_fn1,
                    B8 = tempMinMax$B8_fn2 - tempMinMax$B8_fn1,
                    B10 = tempMinMax$B10_fn2 - tempMinMax$B10_fn1,
                    
                    C1 = tempMinMax$C1_fn2 - tempMinMax$C1_fn1,
                    C3 = tempMinMax$C3_fn2 - tempMinMax$C3_fn1,
                    C5 = tempMinMax$C5_fn2 - tempMinMax$C5_fn1,
                    C7 = tempMinMax$C7_fn2 - tempMinMax$C7_fn1,
                    C9 = tempMinMax$C9_fn2 - tempMinMax$C9_fn1,
                    
                    D2 = tempMinMax$D2_fn2 - tempMinMax$D2_fn1,
                    D4 = tempMinMax$D4_fn2 - tempMinMax$D4_fn1,
                    D6 = tempMinMax$D6_fn2 - tempMinMax$D6_fn1,
                    D8 = tempMinMax$D8_fn2 - tempMinMax$D8_fn1,
                    D10 = tempMinMax$D10_fn2 - tempMinMax$D10_fn1,
                    
                    E1 = tempMinMax$E1_fn2 - tempMinMax$E1_fn1,
                    E3 = tempMinMax$E3_fn2 - tempMinMax$E3_fn1,
                    E5 = tempMinMax$E5_fn2 - tempMinMax$E5_fn1,
                    E7 = tempMinMax$E7_fn2 - tempMinMax$E7_fn1,
                    E9 = tempMinMax$E9_fn2 - tempMinMax$E9_fn1,
                    
                    Control1_30cm = tempMinMax$Control1_30cm_fn2 - tempMinMax$Control1_30cm_fn1,
                    Control2_30cm = tempMinMax$Control2_30cm_fn2 - tempMinMax$Control2_30cm_fn1
                    )

#Get mins and maxes for each group
diffsMinMax <- diffs %>%
  summarise_at(vars(Control, 
                    A1, A3, A5, A7, A9,
                    B2, B4, B6, B8, B10,
                    C1, C3, C5, C7, C9,
                    D2, D4, D6, D8, D10,
                    E1, E3, E5, E7, E9,
                    Control1_30cm, Control2_30cm),
               list(min, max, mean), na.rm = TRUE)

#Visualize the clutch day variability
diffsMelt <- melt(diffs, id = c("Day"), na.rm = TRUE) 

tempDiffs <- ggplot(diffsMelt, aes(x = variable, y = value)) +
  geom_violin(trim = FALSE) +
  xlab("") +
  ylab("") +
  geom_boxplot(width = 0.2, fill = "white") +
  theme_classic() +
  theme(legend.position = "none") 

tempDiffs


####Figure S1####
#showing all plots#

dev.off()
par(mfrow = c(3, 2))

#Nest A
plot(1, type = "n", xlab = "Hours",
     ylab = "Temperature (Cº)", xlim = c(0, 560), 
     ylim = c(30, 35.5))

lines(nest$A1 ~ nest$Hour, lty=2, lwd=2, col="#D55E00")
lines(nest$A3 ~ nest$Hour, lty=2, lwd=2, col="#D55E00")
lines(nest$A5 ~ nest$Hour, lty=2, lwd=2, col="#D55E00")
lines(nest$A7 ~ nest$Hour, lty=2, lwd=2, col="#D55E00")
lines(nest$A9 ~ nest$Hour, lty=2, lwd=2, col="#D55E00")

plot(1, type = "n", xlab = "Hours",
     ylab = "Temperature (Cº)", xlim = c(0, 560), 
     ylim = c(30, 35.5))

lines(nest$B2 ~ nest$Hour, lty=2, lwd=2, col="#56B4E9")
lines(nest$B4 ~ nest$Hour, lty=2, lwd=2, col="#56B4E9")
lines(nest$B6 ~ nest$Hour, lty=2, lwd=2, col="#56B4E9")
lines(nest$B8 ~ nest$Hour, lty=2, lwd=2, col="#56B4E9")
lines(nest$B10 ~ nest$Hour, lty=2, lwd=2, col="#56B4E9")

plot(1, type = "n", xlab = "Hours",
     ylab = "Temperature (Cº)", xlim = c(0, 560), 
     ylim = c(30, 35.5))

lines(nest$C1 ~ nest$Hour, lty=2, lwd=2, col="#0072B2")
lines(nest$C3 ~ nest$Hour, lty=2, lwd=2, col="#0072B2")
lines(nest$C5 ~ nest$Hour, lty=2, lwd=2, col="#0072B2")
lines(nest$C7 ~ nest$Hour, lty=2, lwd=2, col="#0072B2")
lines(nest$C9 ~ nest$Hour, lty=2, lwd=2, col="#0072B2")

plot(1, type = "n", xlab = "Hours",
     ylab = "Temperature (Cº)", xlim = c(0, 560), 
     ylim = c(30, 35.5))
lines(nest$D2 ~ nest$Hour, lty=2, lwd=2, col="#009E73")
lines(nest$D4 ~ nest$Hour, lty=2, lwd=2, col="#009E73")
lines(nest$D6 ~ nest$Hour, lty=2, lwd=2, col="#009E73")
lines(nest$D8 ~ nest$Hour, lty=2, lwd=2, col="#009E73")
lines(nest$D10 ~ nest$Hour, lty=2, lwd=2, col="#009E73")

plot(1, type = "n", xlab = "Hours",
     ylab = "Temperature (Cº)", xlim = c(0, 560), 
     ylim = c(30, 35.5))
lines(nest$E1 ~ nest$Hour, lty=2, lwd=2, col="#CC79A7")
lines(nest$E3 ~ nest$Hour, lty=2, lwd=2, col="#CC79A7")
lines(nest$E5 ~ nest$Hour, lty=2, lwd=2, col="#CC79A7")
lines(nest$E7 ~ nest$Hour, lty=2, lwd=2, col="#CC79A7")
lines(nest$E9 ~ nest$Hour, lty=2, lwd=2, col="#CC79A7")

plot(1, type = "n", xlab = "Hours",
     ylab = "Temperature (Cº)", xlim = c(0, 560), 
     ylim = c(30, 35.5))
lines(nest$Control ~ nest$Hour, lty=1, lwd=2, col="black")
lines(nest$Control1_30cm ~ nest$Hour, lty=1, lwd=2, col="gray")
lines(nest$Control2_30cm ~ nest$Hour, lty=1, lwd=2, col="gray")

###Figure 2####
#Using only middle of array treatments for single plot (A5, B6, C5, D6, E5)
dev.off()

plot(1, type = "n", xlab = "Hours",
     ylab = "Temperature (Cº)", 
     xlim = c(0, 560), 
     ylim = c(30, 37), 
     cex.axis = 1.1,
     cex.lab = 1.2)
lines(nest$Control ~ nest$Hour, lty=1, lwd=3, col="darkgray")
lines(nest$A5 ~ nest$Hour, lty=1, lwd=3, col="#D55E00")
lines(nest$B6 ~ nest$Hour, lty=1, lwd=3, col="#56B4E9")
lines(nest$C5 ~ nest$Hour, lty=1, lwd=3, col="#0072B2")
lines(nest$D6 ~ nest$Hour, lty=1, lwd=3, col="#009E73")
lines(nest$E5 ~ nest$Hour, lty=1, lwd=3, col="#CC79A7")

text(190, 36, "Treatment", cex = 2, pos = 2)
text(200, 36.9, "A: (40cm)", col = "#D55E00", cex = 1.5, pos = 4)
text(200, 36.4, expression("B: (40cm, H"[2]), col = "#56B4E9", cex = 1.5, pos = 4) 
     text(310, 36.46, "0, 40% Shade)", col = "#56B4E9", cex = 1.5, pos = 4)
text(200, 35.9, expression("C: (60cm, H"[2]), col = "#0072B2", cex = 1.5, pos = 4) 
     text(310, 35.96, "0, 80% Shade)", col = "#0072B2", cex = 1.5, pos = 4)
text(200, 35.4, "D: (40cm, 40% Shade)", col = "#009E73", cex = 1.5, pos = 4)
text(200, 34.9, "E: (60cm, 80% Shade)", col = "#CC79A7", cex = 1.5, pos = 4)


####Figure 3####
#Load data for figure 3 and for analyses
tempData <- read.csv("MeanTempTSP.csv")
      tempData$Treatment <- as.factor(tempData$Treatment)
turtData <- read.csv("TurtData.csv")
      turtData$Treatment <- as.factor(turtData$Treatment)
massData <- read.csv("Mass.csv")
      massData$Treatment <- as.factor(massData$Treatment)
      
#Look at differences between temp during TSP across treatments (Fig 3A)
test3 <- data.frame(matrix(ncol=3, nrow=5))
colnames(test3) <- c("treatment", "min", "max")
test3$treatment <- c("A", "B", "C", "D", "E")
## get the max/min values for each treatment
for(letter in as.vector(test3$treatment)){
  test3[test3$treatment == letter, "min"] <- min(tempData[tempData$Treatment == letter, "MinTempTSP"])
  test3[test3$treatment == letter, "max"] <- max(tempData[tempData$Treatment == letter, "MaxTempTSP"])
}
## melt the dataframe into a "long" format
test3 <- melt.data.frame(test3, "treatment", c("min", "max"))

## plot that sucker 
tempBox <- ggplot(tempData) + 
  ylab("Mean TSP Temperature (Cº)") +
  xlab("") +
  geom_boxplot(width = 0.3, aes(x = Treatment, y = MeanTempTSP, fill = Treatment)) +
  geom_point(data=test3, aes(x=treatment, y=value, col=variable, size = 2)) +
  scale_color_manual(values=c("blue", "red")) +
  scale_fill_manual(values = c("#D55E00", "#56B4E9", "#0072B2", "#009E73", "#CC79A7")) +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(c(29.5, 37.2)) +
  geom_bracket(
    xmin = c("A", "A", "A", "A", "B"),
    xmax = c("B", "C", "D", "E", "E"),
    label = c("***", "***", "***", "***", "*"),
    tip.length = 0.01,
    size = 0.5,
    label.size = 5,
    y.position = c(35.4, 35.7, 36, 36.3, 36.6))

tempBox

#Including clutch as a random factor
meanTSPANOVA2 <- lme(MeanTempTSP ~ Treatment, 
                      random= ~1|Nest, 
                      method="ML", 
                      data = tempData) 

summary(meanTSPANOVA2)

summary(glht(meanTSPANOVA2, linfct = mcp(Treatment = "Tukey")))

#Plot incubation time by mean temp (Fig 3b)
incubTemp <- ggplot(turtData, aes(x = DailyMeanTempTSP, y = IncubationDur)) +            
  geom_point(aes(size = 2, col = Treatment)) +
  xlab("Mean Daily Temperature During TSP (Cº)") +
  ylab("Incubation Duration") +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#D55E00", "#56B4E9", "#0072B2", "#009E73", "#CC79A7"))

incubTemp 

#Is daily mean temp during TSP correlated with incubation duration?
summary(lm(IncubationDur ~ DailyMeanTempTSP, data = turtData))

#Including clutch as a random factor
IncubationDurANOVA2 <- lme(IncubationDur ~ DailyMeanTempTSP, 
                           random= ~1|Nest, 
                           method="ML", 
                           data = turtData) 

summary(IncubationDurANOVA2)

#Look at hatch success rate (Fig 3C)
hatchrate <- ggbarplot(turtData, x = "Treatment", y = "HatchingRate", 
                       fill = "Treatment",
                       color = "black",
                       add = c("mean_se"),
                       xlab = FALSE,
                       ylab = "Hatching Success",
                       palette =  c("#D55E00", 
                                    "#56B4E9", 
                                    "#0072B2", 
                                    "#009E73", 
                                    "#CC79A7"))

hatchrate <- hatchrate + theme(legend.position = "none") +
  ylim(c(0, 105)) +
  geom_bracket(
    xmin = c("A", "D"),
    xmax = c("E", "E"),
    label = c("*", "*"),
    tip.length = 0.01,
    size = 0.5,
    label.size = 5,
    y.position = c(98, 100))

hatchrate

#Does hatch rate differ by treatment?
#Including clutch as a random factor
hatchingANOVA2 <- lme(HatchingRate ~ Treatment, 
                      random= ~1|Nest, 
                      method="ML", 
                      data = turtData) 

summary(hatchingANOVA2)

summary(glht(hatchingANOVA2, linfct = mcp(Treatment = "Tukey")))

#Compare carapace area across treatments (Fig 3d)
violinMass <- ggplot(massData, aes(x = Treatment, y = Carapace.Area)) +
  geom_violin(trim = FALSE) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_boxplot(width = 0.2, fill = "white") +
  ylab(expression("Carapace Area (mm" ^ 2 *")")) +
  xlab("") +
  ylim(c(750, 2050))

violinMass <- violinMass +  
  geom_bracket(
    xmin = c("A", "A"),
    xmax = c("C", "D"),
    label = c("***", "*"),
    tip.length = 0.01,
    size = 0.5,
    label.size = 5,
    y.position = c(1900, 1950))

violinMass


#Does hatch rate differ by treatment? 
#Including clutch as a random factor
caraAreaANOVA2 <- lme(Carapace.Area ~ Treatment, 
                          random= ~1|Nest, 
                          method="ML", 
                          data = massData) 

summary(caraAreaANOVA2)

summary(glht(caraAreaANOVA2, linfct = mcp(Treatment = "Tukey")))


#Check mean and sd for different depths among experimental nests
deepNests <- c(turtData[which(turtData$Treatment == "C"), ]$IncubationDur,
              turtData[which(turtData$Treatment == "E"), ]$IncubationDur)
mean(deepNests)
sd(deepNests)

shallowNests <- c(turtData[which(turtData$Treatment == "B"), ]$IncubationDur,
               turtData[which(turtData$Treatment == "D"), ]$IncubationDur)
mean(shallowNests)
sd(shallowNests)


#Plot Figure 3, put all plots together
plot_grid(tempBox, hatchrate, incubTemp, violinMass, labels=c("a", "c", "b", "d"), ncol = 2, nrow = 2)



##Additional assessment of mass to include yolk size
#Compare mass across treatments (Fig 3d)
violinMass2 <- ggplot(massData, aes(x = Mass, y = Treatment, fill = Treatment)) +
  geom_violin(trim = FALSE) +
  coord_flip() +
  xlab(expression("Mass (g)")) +
  ylab("") +
  geom_boxplot(width = 0.2, fill = "white") +
  theme_classic() +
  theme(legend.position = "none") 

violinMass2 <- violinMass2 + scale_fill_manual(values = c("#D55E00", "#56B4E9", "#0072B2", "#009E73", "#CC79A7"))

violinMass2

#Does hatch rate differ by treatment? 
#Including clutch as a random factor
massANOVA2 <- lme(Mass ~ Treatment, 
                      random= ~1|Nest, 
                      method="ML", 
                      data = massData) 

summary(massANOVA2)

summary(glht(massANOVA2, linfct = mcp(Treatment = "Tukey")))





####Figure 4####

#Load in data
data2 <- read.csv("NewFloData.csv")

#Run TSD models and select for best model fit

#Models (Hill, logistic)
data_Hill <- with(data2, tsd(males=Males, females=Females, 
                        temperatures=TempCorr,
                        equation = "Hill"))

data_Logistic <- with(data2, tsd(males=Males, females=Females, 
                             temperatures=TempCorr,
                             equation = "logistic"))

#Compare AICs. 
compare_AIC(Logistic = data_Logistic, 
            Hill = data_Hill)

compare_AICc(Logistic = data_Logistic, 
             Hill = data_Hill, 
             factor.value = -1)

#Both are equally supported so presenting the logistic curve

dev.off()

#plot these data, x axis is temp (C), y axis is ratio of males from all males (1) to no males (0)
#inflection point is the pivotal temp

#Figure 4 to print
plot(data_Logistic,
     males.freq = FALSE,
     show.PTRT = FALSE,
     show.observations = FALSE,
     cex.axis= 1.3,
     cex.lab = 1.5,
     lwd = 2)
    points(data2$FemaleFreq ~ data2$TempCorr, pch = 21, cex = 2,
           col = "black",
           bg = data2$Cols
           )
    abline(v = data_Logistic$P_TRT[2, 4], lwd = 2, col = "gray")
    abline(v = data_Logistic$P_TRT[2, 1], lwd = 2, lty = 2, col = "gray")
    abline(v = data_Logistic$P_TRT[2, 2], lwd = 2, lty = 2, col = "gray")

#Other curves to look at variability (it is small)
plot(data_Hill,
         males.freq = FALSE,
         show.PTRT = FALSE,
         show.observations = FALSE)
    points(data2$FemaleFreq ~ data2$TempCorr, pch = 19, cex = 1.5)
    abline(v = data_Hill$P_TRT[2, 4], lwd = 2, col = "red")
    abline(v = data_Hill$P_TRT[2, 1], lwd = 2, lty = 2, col = "gray")
    abline(v = data_Hill$P_TRT[2, 2], lwd = 2, lty = 2, col = "gray")

    
#Predict male ratio at a given temperature using the logistic model
    #50% is the mean estimate, 2.5% is the lower confidence interval, 97.5 is the upper confidence interval
predict(data_Logistic, temperatures = c(31.5))













