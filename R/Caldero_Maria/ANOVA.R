#install packages: only the first time
install.packages('ggplot2') #nice plots
install.packages('tidyverse') #manipulate datasets

#load packages: every time we want to run the script
library(ggplot2)
library (tidyverse)

#set working directory to input files: it may change depending on your local folder!
setwd("C:/Rprojects/BETAseminars")

#load input file
MBRdata <- read.csv("Input/MBR_data.csv", stringsAsFactors = T) #make sure our PCs are configured to same csv files separated by ',' instead of ';'
?read.csv #when you want to know more about the function used --> the HELP window opens below.
names(MBRdata) #check if it has been imported correctly
str(MBRdata) #check if it has been imported correctly
view(MBRdata) #open imported dataset
summary(MBRdata) #basic statistical data such as mean

#convert stage and HRT int to factor
MBRdata$Stage <- as.factor(MBRdata$Stage)
MBRdata$HRT <- as.factor(MBRdata$HRT)

#ANOVAS for boxplots presented: consider to group data by weeks!




#create a separate dataset to plot Fouling Rate: remove NAs from Fouling Rate column
MBRdata_FR <- MBRdata  %>% 
  select(Stage, HRT, FoulingRate)
MBRdata_FR.na <- MBRdata_FR[complete.cases(MBRdata_FR),] #remove NA values


#plots
MBRdata_FR.na$Stage <- ordered(MBRdata_FR.na$Stage, levels=c("1", "2", "3", "4", "5", "6"))

Fig_FRstage <- ggplot(MBRdata_FR.n, aes(x = Stage, y = FoulingRate, fill=Stage)) +
  geom_hline(yintercept=0, linetype = "dashed", linewidth = 0.5, colour="grey60")+
  scale_fill_viridis_d() +
  geom_boxplot(width = 0.8) +
  ylab("Fouling Rate (mbar/h)")+
  ylim(-20,20)+ #like this I cut outliers!
  xlab("Stage") +
  ggtitle("Fouling Rate") +
  theme(text = element_text(size = 10),
        plot.title = element_text(color="black",size = 10,
                                  face="bold.italic"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")

Fig_FRstage
ggsave("Output/Figures/Fig_FRstage.jpeg", width = 20, height = 15, units = "cm")

MBRdata_FR.na$HRT <- ordered(MBRdata_FR.na$HRT, levels=c("1", "2.25", "1.5", "2", "3"))

Fig_FR_HRT <- ggplot(MBRdata_FR.na, aes(x = HRT, y = FoulingRate, fill=HRT)) +
  geom_hline(yintercept=0, linetype = "dashed", linewidth = 0.5, colour="grey60")+
  scale_fill_manual(values =
                      c("olivedrab3","yellow3","orange3","orangered3","red"))+
  geom_boxplot(width = 0.8) +
  ylab("Fouling Rate (mbar/h)")+
  ylim(-20,20)+ #like this I cut outliers!
  xlab("HRT (days)") +
  ggtitle("Fouling Rate") +
  theme(text = element_text(size = 10),
        plot.title = element_text(color="black",size = 10,
                                  face="bold.italic"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")

Fig_FR_HRT
ggsave("Output/Figures/Fig_FR_HRT.jpeg", width = 20, height = 15, units = "cm")
