#install packages: only the first time
install.packages('ggplot2') #nice plots
install.packages('tidyverse') #manipulate datasets
install.packages('cowplot') #plot_grid function

#load packages: every time we want to run the script
library(ggplot2)
library (tidyverse)
library (cowplot)

#set working directory to input files: it may change depending on your local folder!
setwd("C:/Rprojects/BETAseminars")

#load input file
ordination <- read.csv("Input/Fig_PCA.csv", stringsAsFactors = T, header=T, row.names = NULL)
str(ordination)
#change 'int' to 'num' for COD_in
ordination$COD_in <- as.numeric(ordination$COD_in)
#remove NA values and convert all variables to numeric = ready for PCAs
ordination <- ordination[complete.cases(ordination),] 
#remove "treatment factors" from the PCA sub-dataset
dat_pca <- ordination  %>% 
  select(SampleID, Temp, COD_in, COD_eff, COD_removal, TAN_eff, TAN_in, TAN_removal) 
#check dataset
str(dat_pca)
summary(dat_pca) 

#PCA ANALYSIS
pca <- prcomp(dat_pca[,-1], scale=TRUE, center = TRUE)
summary(pca) #proportion of PCs

#PLOT WITH GGPLOT
# extract PC scores for first two component and add to "ordination" dataframe
ordination$pc1 <- pca$x[,1] # indexing the first column
ordination$pc2 <- pca$x[,2] # indexing the second column
pca.vars <- pca$rotation %>% data.frame
pca.vars$vars <- rownames(pca.vars)

#plot by HRT (we will not use stage in here)
ordination$HRT <- ordered(ordination$HRT, levels=c("1", "1.25", "1.5", "2", "3"))

Fig_PCAstageA <- ggplot(data = ordination, aes(x = pc1, y = pc2, fill=HRT, shape = HRT)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_shape_manual(values =c(21,22,23,24,25))+ #sometimes different shapes per each colour facilitates the reader if they cannot differentiate colours correctly
  scale_fill_manual(values = c("1"="olivedrab2",
                               "1.25"="yellow",
                               "1.5"="yellow3",
                               "2"="orange",
                               "3"="red")) +
  geom_point(size=5, alpha = 1)+
  ylab("PC 2 (29.00%)")+
  xlab("PC 1 (37.02%)")+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major =element_blank(), #remove major-grid labels
        panel.grid.minor = element_blank(), #remove minor-grid labels
        plot.background = element_blank())

Fig_PCAstageA

Fig_PCAstageB <- ggplot() +
  geom_text(data = pca.vars, aes(x = PC1*1.15, y = PC2*1.15),
            label = c("Temp", 
            "COD_in", "COD_eff", "COD_removal", 
            "TAN_eff", "TAN_in", "TAN_removal"),
            nudge_y = 0, nudge_x = 0,
            check_overlap = F, size = 4) +
  geom_segment(data = pca.vars, aes(x = 0, xend = PC1, y = 0, yend = PC2),
               arrow = arrow(length = unit(0.025,"npc"), type = "open"), lwd
               = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlim(-0.65, 0.6)+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major =element_blank(), #remove major-grid labels
        panel.grid.minor = element_blank(), #remove minor-grid labels
        plot.background = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

Fig_PCAstageB 

Figs_PCA <- plot_grid(Fig_PCAstageA, Fig_PCAstageB, align = "h", labels = c("a", "b"),
                  ncol = 2, rel_widths=c(1.2,1))

Figs_PCA
ggsave("Output/Figures/Figs_PCA.jpeg", width = 18, height = 12, units = "cm")

