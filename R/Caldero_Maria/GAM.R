#install packages: only the first time
install.packages('ggplot2') #nice plots
install.packages('cowplot') #align multiple ggplots
install.packages('tidyverse') #reshaping data i.e., pivol longer, remove NAs..
install.packages('dplyr') #manipulate datasets in general: if you get a phyton error go to Tools > global options > Phython > untick 'automatically...' > ok

#load packages: every time we want to run the script
library(ggplot2)
library (tidyverse)
library(dplyr)
library(cowplot)

#set working directory to input files: it may change depending on your local folder!
setwd("C:/Rprojects/BETAseminars")

#load input file
MBRdata <- read.csv("Input/MBR_data.csv", stringsAsFactors = T) #make sure our PCs are configured to same csv files separated by ',' instead of ';'



#Timeseries plot: ggplot does not allow to plot 2 timeseries variables with different y axis as the creator thinks it is not a good practice. 
#Instead, we can plot them well aligned (see below for TMPmax and Operational Flux variables)

Fig_TMPmax <- ggplot(MBRdata, aes(x = OD, y = TMPmax, fill=HRT, shape =
                             HRT)) +
  scale_colour_manual(name = "HRT",values =
                        c("olivedrab2","yellow","yellow3","orange","red"))+
  scale_fill_manual(name = "HRT",values =
                      c("olivedrab2","yellow","yellow3","orange","red"))+
  scale_shape_manual(name = "HRT", values=c(21,22,23,24,25))+
  geom_point(size = 2, position = position_jitterdodge( jitter.width = 0,
                                                        dodge.width = 0)) +
  ylab("TMPmax (mbar/h)")+
  xlab("Operational Day (days)")+
  scale_x_continuous(limits = c(0, 196), breaks = seq(0, 196, by = 7))+
  theme(text = element_text(size = 8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(color="black",size = 10,
                                  face="bold.italic"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "top")+
    geom_vline(xintercept=57, linetype="dotted", size=1, colour="grey50")+
    geom_vline(xintercept=94, linetype="dotted", size=1, colour="grey50")+
    geom_vline(xintercept=118, linetype="dotted", size=1, colour="grey50")+
    geom_vline(xintercept=172, linetype="dotted", size=1, colour="grey50")+
    geom_vline(xintercept=186, linetype="dotted", size=1, colour="grey50")

Fig_TMPmax

Fig_Flux <- ggplot(MBRdata, aes(x = OD, y = Flux, fill=HRT, shape =
                                    HRT)) +
  scale_colour_manual(name = "HRT",values =
                        c("olivedrab2","yellow","yellow3","orange","red"))+
  scale_fill_manual(name = "HRT",values =
                      c("olivedrab2","yellow","yellow3","orange","red"))+
  scale_shape_manual(name = "HRT", values=c(21,22,23,24,25))+
  geom_point(size = 2, position = position_jitterdodge( jitter.width = 0,
                                                        dodge.width = 0)) +
  ylab("Operational Flux (LMH)")+
  xlab("Operational Day (days)")+
  scale_x_continuous(limits = c(0, 196), breaks = seq(0, 196, by = 7))+
  theme(text = element_text(size = 8),
        plot.title = element_text(color="black",size = 10,
                                  face="bold.italic"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  geom_vline(xintercept=57, linetype="dotted", size=1, colour="grey50")+
  geom_vline(xintercept=94, linetype="dotted", size=1, colour="grey50")+
  geom_vline(xintercept=118, linetype="dotted", size=1, colour="grey50")+
  geom_vline(xintercept=172, linetype="dotted", size=1, colour="grey50")+
  geom_vline(xintercept=186, linetype="dotted", size=1, colour="grey50")

Fig_Flux

Figs_Timeseries <- plot_grid(Fig_TMPmax, Fig_Flux, align = "v", labels = c("a", "b"), ncol = 1)
Figs_Timeseries 
ggsave("Output/Figures/Figs_Timeseries.jpeg", width = 20, height = 15, units = "cm")





