library(dplyr)
library(ggplot2)


setwd("C:/Users/heref/Documents/Project stuff/LucasProject")
ctm_data <- read.csv("CTM_data_analysis.csv")
?t.test
Masu_ctm <- ctm_data$Temp_LOE[ctm_data$Species == "Masu"]
Ito_ctm <- ctm_data$Temp_LOE[ctm_data$Species == "Ito"]
t.test(Masu_ctm, Ito_ctm)



ctmnochar <- ctm_data[-c(ctm_data$Species == "Char"),]
ctmnochar <- subset(ctm_data, ctm_data$Species == "Masu" | ctm_data$Species == "Ito" )


?boxplot
boxplot(ctmnochar$Temp_LOE ~ ctmnochar$Species)
ggplot(data = ctm_data) + 
  geom_point(mapping = aes(x = Trial, y = Temp_LOE,
                           color = Species, size = 2))
  
# A really basic boxplot.
ggplot(ctmnochar, aes(x=Species, y=Temp_LOE, fill = Species)) + 
  geom_boxplot() + 
  labs(title = 'Critical Thermal Maxima',
       x = 'Species',
       y = 'Temperature(Celsius)') +
  scale_fill_brewer(palette = 'Dark2')
  
  xlab("Species") + ylab("Temperature(Celsius)") +
    title('Critical Thermal Maxima') +
              
?plot
?geom_point
