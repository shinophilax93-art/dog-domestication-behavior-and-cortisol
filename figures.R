install.packages("ggplot2")
install.packages("tidyverse")
install.packages("vangogh")
install.packages("flexdashboard")
install.packages("DT")
library(ggplot2)
library(tidyverse)
library(vangogh)
library(flexdashboard)
library(DT)

data <- read.csv("data.csv", header=TRUE)

#Exclusion of Dog 1, Dog 4, and Dog 18
data[cbind(c(1,3,11),
           c(7,8,9))] <- NA

data2 <- data[, -which (colnames(data) #Remove irrelevant data columns
                        %in% c("ID", "BreedGroup", "NumberOfChoices_probe_familiar"))]
na.omit(data2)
print(data2)
data2 <- as.data.frame(sapply(data2, as.numeric))

#Obtain color codes
as.character(vangogh_palette("StarryNight"))
as.character(vangogh_palette("SelfPortrait"))

#Set the font
windowsFonts(
  Times = windowsFont("Times New Roman")
)

#NumberOfChoices_probe_novel and CortiConc_SD
plot1 <- ggplot(data = data2, aes(x = CortiConc_SD, 
                                  y = NumberOfChoices_probe_novel,
                                  colour = data$BreedGroup)) +
  theme_classic() +
  geom_point(size = 6, alpha = 0.7) +
  scale_colour_manual(
    values = c(
      "European" = "#F8766D",
      "Japanese" = "#619CFF",
      "Mongrel" = "#00BA38"
    )
  ) +
  theme(text = element_text(family = "Times", size = 18)) +
  geom_smooth(method = "lm", 
              colour = "#B4D9CE", lwd = 1.5, fill = "#27708C") +
  coord_cartesian(xlim = c(0, NA),
                  ylim = c(0, NA))+
  theme(axis.line = element_line(colour = "#0b1e38",
                                 size = 0.8, 
                                 linetype = "solid")) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "SD of Cortisol Conc. (ng / mg creatinine)") +
  scale_y_continuous(name = "Number of Novel Stimuli selected in Probe trials") +
  annotate("text", x=21.5, y=3.75, size = 6.5, family = "Times",
           fontface = "italic", colour ="#A6511F",
           label="rs = -0.48, p = .021")
plot1

#Nei_GrayWolf and CortiConc_SD
plot2 <- ggplot(data = data2, aes(x = Nei_GrayWolf, 
                                  y = CortiConc_SD,
                                  colour = data$BreedGroup)) +
  theme_classic() +
  geom_point(size = 6, alpha = 0.7) +
  scale_colour_manual(
    values = c(
      "European" = "#F8766D",
      "Japanese" = "#619CFF",
      "Mongrel" = "#00BA38"
    )
  ) +
  theme(text = element_text(family = "Times", size = 18)) +
  geom_smooth(method = "lm", colour = "#B4D9CE", lwd = 1.5, fill = "#27708C") +
  coord_cartesian(ylim = c(0, NA))+
  theme(axis.line = element_line(colour = "#0b1e38", 
                                 size = 0.8, 
                                 linetype = "solid")) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "Genetic Distance from Grey Wolf") +
  scale_y_continuous(name = "SD of Cortisol Conc. (ng / mg creatinine)") +
  annotate("text", x=0.21, y=31.5, size = 6.5, family = "Times", 
           fontface = "italic", colour ="#A6511F",
           label="rs = 0.37, p = .037")
plot2

#NumberOfTrialSessions and CortiConc_Ave
plot3 <- ggplot(data = data2, aes(x = CortiConc_Ave, 
                                  y = NumberOfTrialSessions,
                                  colour = data$BreedGroup)) +
  theme_classic() +
  geom_point(size = 6, alpha = 0.7) +
  scale_colour_manual(
    values = c(
      "European" = "#F8766D",
      "Japanese" = "#619CFF",
      "Mongrel" = "#00BA38"
    )
  ) +
  theme(text = element_text(family = "Times", size = 18)) +
  geom_smooth(method = "lm", colour = "#B4D9CE", lwd = 1.5, fill = "#27708C") +
  theme(axis.line = element_line(colour = "#0b1e38", 
                                 size = 0.8, 
                                 linetype = "solid")) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "Cortisol Conc. (ng / mg creatinine)") +
  scale_y_continuous(name = "Number of Trial sesstions") +
  annotate("text", x=38, y=7.5, size = 6.5, family = "Times", 
           fontface = "italic", colour ="#A6511F",
           label="rs = 0.57, p < .01")
plot3

#Neophobia and Owner_directed_aggression
plot4 <- ggplot(data = data2, aes(x = Neophobia, 
                                  y = Owner_directed_aggression,
                                  colour = data$BreedGroup)) +
  theme_classic() +
  geom_point(size = 6, alpha = 0.7) +
  scale_colour_manual(
    values = c(
      "European" = "#F8766D",
      "Japanese" = "#619CFF",
      "Mongrel" = "#00BA38"
    )
  ) +
  theme(text = element_text(family = "Times", size = 18)) +
  geom_smooth(method = "lm", colour = "#B4D9CE", lwd = 1.5, fill = "#27708C") +
  theme(axis.line = element_line(colour = "#0b1e38", 
                                 size = 0.8, 
                                 linetype = "solid")) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "Latency difference (Neophobia)") +
  scale_y_continuous(name = "Owner-directed aggression") +
  annotate("text", x=1.75, y=2.0, size = 6.5, family = "Times", 
           fontface = "italic", colour ="#A6511F",
           label="rs = 0.65, p = .013")
plot4

#CortiConc_SD and Familiar_dog_aggression
plot5 <- ggplot(data = data2, aes(x = CortiConc_SD, 
                                  y = Familiar_dog_aggression,
                                  colour = data$BreedGroup)) +
  theme_classic() +
  geom_point(size = 6, alpha = 0.7) +
  scale_colour_manual(
    values = c(
      "European" = "#F8766D",
      "Japanese" = "#619CFF",
      "Mongrel" = "#00BA38"
    )
  ) +
  theme(text = element_text(family = "Times", size = 18)) +
  geom_smooth(method = "lm", colour = "#B4D9CE", lwd = 1.5, fill = "#27708C") +
  coord_cartesian(xlim = c(0, NA),
                  ylim = c(0, NA))+
  theme(axis.line = element_line(colour = "#0b1e38", 
                                 size = 0.8, 
                                 linetype = "solid")) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "SD of Cortisol Conc. (ng / mg creatinine)") +
  scale_y_continuous(name = "Familiar -dog aggression") +
  annotate("text", x=21, y=0.85, size = 6.5, family = "Times", 
           fontface = "italic", colour ="#A6511F",
           label="rs = -0.41, p = .039")
plot5

#Between-group comparison of cortisol SD

data3 <- subset(
  data,
  BreedGroup %in% c("Japanese", "European")
)

CortisolSDgg <- ggplot(data3) +
  theme_classic() +
  geom_boxplot(
    aes(x=BreedGroup,y= CortiConc_SD), width = 0.5)+
  theme(text = element_text(size = 18, family = "Times")) +
  scale_y_continuous(name = "SD of Cortisol Conc. (ng / mg creatinine)", limits=c(0,35)) +
  scale_x_discrete(name = "Breed Group") +
  theme(legend.position = "none") +
  geom_jitter(aes(x=BreedGroup,y= CortiConc_SD, colour = BreedGroup), 
              size = 6.5, width = 0.1, alpha = 0.7) +
  scale_colour_manual(
    values = c(
      "European" = "#F8766D",
      "Japanese" = "#619CFF"
    )
  ) +
  geom_text(x = 1.5, y = 32, size = 11, Family ="Times", label = "*") + #有意差マーク
  geom_segment(x = 1, xend = 1, y = 31, yend = 30) + #有意差左線
  geom_segment(x = 1, xend = 2, y = 31, yend = 31) + #有意差直線
  geom_segment(x = 2, xend = 2, y = 31, yend = 30) + #有意差右線
  annotate("text", x=1.5, y=35, size = 5.5, family = "Times", 
           fontface = "italic", colour ="#A6511F",
           label="Wilcoxon rank-sum test: W = 54, P = 0.016")

plot(CortisolSDgg)
