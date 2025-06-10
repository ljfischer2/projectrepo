#################################################################################################
#Trial 1
#################################################################################################

rm(list = ls())
library(respR)
library(tidyverse)

rm(list = ls())
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data1<-read.csv("Trial_1_Comp.csv",header=T)
#data1 <- data1[-c(42751:42937), ]
data1 <- data1[-c(1:10800),]
chamber1.1<-inspect(data1, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber1.2<-inspect(data1, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber1.3<-inspect(data1, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber1.4<-inspect(data1, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

#################################################################################################
#Fish O2 Consumption
#################################################################################################
#?calc_rate
Masu1 <- calc_rate.int(chamber1.1,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Ito1 <- calc_rate.int(chamber1.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito2 <- calc_rate.int(chamber1.3,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Ito3 <-calc_rate.int(chamber1.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")


#############################################################################################################


convert_Masu1 <- convert_rate(Masu1,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0033)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito1 <- convert_rate(Ito1,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0055)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito2 <- convert_rate(Ito2,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0118)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito3 <- convert_rate(Ito3,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0056)            # mass of the specimen in kg (fish wet mass: kg)



#ggplot(data = data2) +
 # geom_histogram(binwidth = .5, mapping = aes(x = TempA)) +
  #ggtitle("Trials Per Half Degree Change in Temperature") +
  #xlab("Temperature(Celsius)")


summary(convert_Masu1)
summary(convert_Ito1)
summary(convert_Ito2)
summary(convert_Ito3)

#########################################################################################################################
#Attempts to bin based on temp.
#####################################################

#What if we select the temp at the start of every trial?
total_rows <- nrow(data1)
num_sets <- total_rows %/% 450
Rep_TempA <- data1$Temp[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA, breaks = 11)

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)


#plot(convert_Ito4, type = "rate")
#plot(convert_Ito4, type = "overlap")



#######################################################
#Masu 2
######################################################
#### llx start ###
dfMasu1 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu1_T <- convert_Masu1 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu1 <- rbind(dfMasu1, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu1_T$rate.output[o]
    ))
  }
}

View(dfMasu1)
### llx end ###

posrate <- abs(dfMasu1$rate_output)
plot(posrate)
ggplot(data = dfMasu1, aes(x = temp, y = rate_output)) + 
  geom_point() +
  labs(x = "Temp",
       y = 'Oxygen Consumption(g O2·g−1·day−1)',
       title  = "Oxygen Consumption Rate of Masu 1") + 
  scale_color_manual(values = c('#1b9e77'))

mean(dfMasu1$rate_output)



##################################################################################
#Ito1
##################################################################################
dfIto1 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto1_T <- convert_Ito1 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto1 <- rbind(dfIto1, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRIto1_T$rate.output[o]
    ))
  }
}

View(dfIto1)
#posrateIto4 <- abs(dfIto4$rate_output)
#plot(posrateIto4)
Ito2
################################################################################
#Masu3
################################################################################

dfIto2 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())



for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto2_T <- convert_Ito2 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto2 <- rbind(dfIto2, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRIto2_T$rate.output[o]
    ))
  }
}

View(dfIto2)
#posrateMasu3 <- abs(dfMasu3$rate_output)
#plot(posrateMasu3)
################################################################################
#Masu4
################################################################################
dfIto3 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())


for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto3_T <- convert_Ito3 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto3 <- rbind(dfIto3, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRIto3_T$rate.output[o]
    ))
  }
}

View(dfIto3)
#posrateMasu4 <- abs(dfMasu4$rate_output)
#plot(posrateMasu4)
