#install packages: only the first time
install.packages('ggplot2') #nice plots
install.packages('tidyverse') #reshaping data i.e., pivol longer, remove NAs..
install.packages('dplyr') #manipulate datasets in general: if you get a phyton error go to Tools > global options > Phython > untick 'automatically...' > ok
install.packages('rstatix') #function add_significance() function
install.packages('lubridate') #timeseries
install.packages('zoo') #useful for irregular timeseries
#it may be useful to save output of statistical tests = package 'tidystats' --> for my PhD papers I did it manually
#it may be useful to add p-values significance to ggplot graphs = package 'ggsignif' --> for my PhD papers I did it manually

#load packages: every time we want to run the script
library(ggplot2)
library (tidyverse)
library(dplyr)
library(rstatix)
library(lubridate)
library(zoo)

#set working directory to input files: it may change depending on your local folder!
setwd("C:/Rscripts/BETAseminars/R/Iborra_Roser")

#load input file
MBRdata <- read.csv("Input/MBR_data.csv", stringsAsFactors = T) #make sure our PCs are configured to same csv files separated by ',' instead of ';'
?read.csv #when you want to know more about the function used --> the HELP window opens below.
names(MBRdata) #check if it has been imported correctly
str(MBRdata) #check if it has been imported correctly
View(MBRdata) #open imported dataset
summary(MBRdata) #basic statistical data such as mean
#important to know how to work with timeseries = convert Date default 'factor' variable to a real 'date' variable
MBRdata$Date <- as.Date(MBRdata$Date,"%d/%m/%Y")
#create a separate dataset to plot Fouling Rate: remove NAs from Fouling Rate column
MBRdata_FR <- MBRdata  %>% 
  select(Date, Stage, HRT, FoulingRate)
MBRdata_FR.na <- MBRdata_FR[complete.cases(MBRdata_FR),] #remove NA values

#consider to work with weekly means
#1st step: group current data by operational day = date
MBRdata_FR.na_daily <- aggregate(MBRdata_FR.na[, 2:4], list(MBRdata_FR.na$Date), mean)
colnames(MBRdata_FR.na_daily)[1] <- "Date"
#2nd step: add missing dates
ts <- seq(ymd("2023-05-29"), ymd("2023-09-28"), by="day")
ts_df <- data.frame(Date=ts)
MBRdata_FRdaily <- full_join(ts_df,MBRdata_FR.na_daily) #by date
#3rd step: calculate weekly averages --> problem = during some weeks you have different treatments....
#how did you solve this?
weekly_means <- MBRdata_FRdaily %>%
  mutate(Week = floor_date(Date, unit = "week")) %>%  #to create a new column representing the week of each date
  group_by(Week) %>%
  summarize(WeeklyMean_Stage = mean(Stage, na.rm = TRUE),
            WeeklyMean_HRT = mean(HRT, na.rm = TRUE),
            WeeklyMean_FR = mean(FoulingRate, na.rm = TRUE),
            .groups = 'drop'
  )

#t-tests for sub-daily dataset
MBRdata_FR.na_Stage <- MBRdata_FR.na %>% 
  select(Stage, FoulingRate)
MBRdata_FR.na_HRT <- MBRdata_FR.na %>% 
  select(HRT, FoulingRate)
#step useful when we have more than 1 variable
MBRdata_Stage.long <- MBRdata_FR.na_Stage %>%
  pivot_longer(-Stage, names_to = "variables", values_to = "value")
str(MBRdata_Stage.long)

MBRdata_HRT.long <- MBRdata_FR.na_HRT %>%
  pivot_longer(-HRT, names_to = "variables", values_to = "value")
str(MBRdata_HRT.long)

#Pairwise t test: sub-daily FOULING RATE by Stage
stat.test <- MBRdata_Stage.long %>%
  group_by(variables) %>%
  t_test(value ~ Stage) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test #not significantly among sub-daily Stages

#Pairwise t test: sub-daily FOULING RATE by HRT
stat.test <- MBRdata_HRT.long %>%
  group_by(variables) %>%
  t_test(value ~ HRT) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test #not significantly among sub-daily HRT

#t-tests for weekly dataset
#weekly_means good for plotting but not good for t-test as it has some NAs
#and treatments are not the same...




#BOXPLOTS
#SUB-DAILY
#convert stage and HRT int to factor
MBRdata$Stage <- as.factor(MBRdata$Stage)
MBRdata$HRT <- as.factor(MBRdata$HRT)

MBRdata_FR.na$Stage <- ordered(MBRdata_FR.na$Stage, levels=c("1", "2", "3", "4", "5", "6"))

Fig_FRstage <- ggplot(MBRdata_FR.na, aes(x = Stage, y = FoulingRate, fill=Stage)) +
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

MBRdata_FR.na$HRT <- ordered(MBRdata_FR.na$HRT, levels=c("1", "1.25", "1.5", "2", "3"))

Fig_FR_HRT <- ggplot(MBRdata_FR.na, aes(x = HRT, y = FoulingRate, fill=HRT)) +
  geom_hline(yintercept=0, linetype = "dashed", linewidth = 0.5, colour="grey60")+
  scale_fill_manual(values =
                      c("olivedrab2","yellow","yellow3","orange","red"))+
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

#WEEKLY
#solve first the fact that "treatments" change if we calculate weekly means....





