#This file contains all codes for article titled: Spatial isolation slows biodiversity recovery more than frequent recultivation in post-agricultural tropical savannahs

#Contents

#1. Importing old-growth savannah data
#2. Importing fallow data
#3. Community Dissimilarity import
#4. Figure 2: Biodiversity metrics vs Fallow age regression
#5. Figure 3: Diversity metrics vs Distance to nearest remnant savanna (OG) patch
#6. Figure 4: PCA Habitat and Soil
#7. Figure 5: Future prediction framework

#8. Table 1: Interaction terms table

library(ggplot2)
library(dplyr)
library(vegan)
library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(patchwork)
library(tidyr)
library(ggrepel)
library(ggord)
library(stargazer)
options(scipen=9999)
set.seed(1)

#set your working directory where all data files are stored

#setwd()

#1. Importing old-growth savannah data

data_OG <- read.csv("richness_cover_OG.csv", header = TRUE) #import old-growth (OG) data set
str(data_OG) #check data
head(data_OG)
total_richness_og <- data_OG$Total.Richness #specify all herbaceous richness y variables for OG box plot
native_richness_og <- data_OG$Native.Richness
non.native_richness_og <- data_OG$Non.native.Richness
invasive_richness_og <- data_OG$Invasive.Richness

total_cover_og <- data_OG$Total.Cover #specify all herbaceous cover y variables for OG box plot
native_cover_og <- data_OG$Native.Cover
non.native_cover_og <- data_OG$Non.native.Cover

#Get means of OG values for adding in plots later

meanTotalRich <- mean(total_richness_og)
meanNativeRich <- mean(native_richness_og)
meanNonNativeRich <- mean(non.native_richness_og)

meanTotalCover <- mean(total_cover_og)
meanNativeCover <- mean(native_cover_og)
meanNonNativeCover <- mean(non.native_cover_og)

####################################################################################################################################################################################

#2. Importing fallow data

data_fallow <- read.csv("richness_cover_fallow.csv", header = TRUE) #import fallow data set: only contains fallow sites data
data_fallow$age <- as.numeric(data_fallow$age)
data_fallow$distance <- as.numeric(data_fallow$distance)
str(data_fallow) #check data

age_fallow <- data_fallow$age #x variable
dist_fallow <- data_fallow$distance #x variable

total_richness_fallow <- data_fallow$Total.Richness #specify all richness y variables for fallow
native_richness_fallow <- data_fallow$Native.Richness
non.native_richness_fallow <- data_fallow$Non.native.Richness

total_cover_fallow <- data_fallow$Total.Cover #specify all cover y variables for fallow
native_cover_fallow <- data_fallow$Native.Cover
non.native_cover_fallow <- data_fallow$Non.native.Cover

####################################################################################################################################################################################

#3. Community Dissimilarity import

dissimilarity_data <- read.csv("DissimiData_no_UI.csv")

#name variables 
dissim_fallow <- dissimilarity_data$dissimilarity_fallow

# To see the effect of fallow age on compositional dissimilarity of fallows
Dissimilarity_Age_model <- lm(data= dissimilarity_data, dissim_fallow ~ age_fallow)
summary(Dissimilarity_Age_model)
coef(Dissimilarity_Age_model)

# To see the effect of distance to nearest remnant savannah on compositional dissimilarity of fallows
Dissimilarity_Dist_model <- lm(data= dissimilarity_data, dissim_fallow ~ log10(dist_fallow))
summary(Dissimilarity_Dist_model)
coef(Dissimilarity_Dist_model)

####################################################################################################################################################################################

#4. Figure 2: Biodiversity metrics vs Fallow age regression

#4.1 Regression of herbaceous richness of fallows with fallow age  

# Model to see the effect of fallow age on total richness of fallows
lmTotalRichAge <- lm(data = data_fallow, total_richness_fallow ~ age_fallow) #Linear model (lm) of Total Richness & Age
summary(lmTotalRichAge)
coef(lmTotalRichAge) #c= 9.97 slope(m)= 0.148
#After solving equation, it takes fallows x = 13.1 years to reach mean of OGs

# Model to see the effect of fallow age on native richness of fallows
lmNativeRichAge <- lm(data = data_fallow, native_richness_fallow ~ age_fallow) #lm of Native Richness & Age
summary(lmNativeRichAge)
coef(lmNativeRichAge)
#After solving equation, it takes fallows x = 26.7 years to reach mean of OGs

# Model to see the effect of fallow age on non-native richness of fallows
lmNonNativeRichAge <- lm(data = data_fallow, non.native_richness_fallow ~ age_fallow) #lm of Non-native Richness & Age
summary(lmNonNativeRichAge) 
coef(lmNonNativeRichAge)
#After solving equation, it takes fallows x = 58.7 years to reach mean(Total Richness of OGs)

#First, plot boxplot of just total, native and non-native richness of the oldgrowth savannahs as a reference 

box_rich <- ggplot(data_OG) +
  geom_boxplot(aes(x = 1, y = Total.Richness, colour = "Total"), 
               width = 0.9, outlier.shape = NA)+ 
  geom_boxplot(aes(x = 2, y = Native.Richness, colour = "Native"), 
               width = 0.9, outlier.shape = NA)+ 
  geom_boxplot(aes(x = 3, y = Non.native.Richness, colour = "Non-native"), 
               width = 0.9, outlier.shape = NA)+
  scale_colour_manual(values = c("Total" = "black", "Native" = "#BA7333", "Non-native" = "#F8B55C")) +
  geom_jitter(aes(x=1, y = Total.Richness, colour = "Total"), width = 0.1, alpha = 0.3, size = 2.5)+
  geom_jitter(aes(x=2, y = Native.Richness, colour = "Native"),width = 0.1,  alpha = 0.6, size = 2.5)+
  geom_jitter(aes(x=3, y = Non.native.Richness, colour = "Non-native"),width = 0.1, alpha = 0.3, size = 2.5)+
  stat_summary(aes(x = 1, y = Total.Richness), fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(aes(x = 2, y = Native.Richness), fun = mean, geom = "point", shape = 18, size = 4, color = "#BA7333") +
  stat_summary(aes(x = 3, y = Non.native.Richness), fun = mean, geom = "point", shape = 18, size = 4, color = "#F8B55C") +
  coord_cartesian(ylim = c(0, 35)) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),   
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))

# Now, plot regression of total, native and non-native richness of fallows vs fallow age

regRichnessAge <- ggplot(data_fallow, aes(x=age)) +
  annotate("segment", x = 12.9, xend = 12.9, y = 0, yend = -1.5, #to draw an arrow where Total Richness of fallows reaches that of mean(OGs)
           color = "black", linetype = "solid", linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 26.7, xend = 26.7, y = 0, yend = -1.5, #to draw an arrow where Native Richness of fallows reaches that of mean(OGs)
           color = "#BA7333", linetype = "solid", linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  stat_poly_line(aes(y = total_richness_fallow, colour="Total", fill="Total"), formula = y ~ x, se = TRUE, alpha = 0.12) +
  geom_point(size = 2.5, alpha = 0.3, aes(y = total_richness_fallow, colour="Total")) +
  stat_poly_line(aes(y = native_richness_fallow, colour="Native", fill="Native"), formula = y ~ x, se = TRUE, alpha = 0.12) +
  geom_point(size = 2.5, alpha = 0.6, aes(y = native_richness_fallow, colour="Native")) +
  
  stat_poly_line(aes(y = non.native_richness_fallow, colour="Non-native", fill="Non-native"), formula = y ~ x, se = TRUE, alpha = 0.12) +
  geom_point(size = 2.5, alpha = 0.3, aes(y = non.native_richness_fallow, colour="Non-native")) +
  
  scale_colour_manual(name = "", 
                      values = c("Total" = "black", "Native" = "#BA7333", "Non-native" = "#F8B55C"),
                      breaks = c("Total", "Native", "Non-native"),
                      labels = c(
                        "Total" = expression("Total" ~ italic("(R² = 0.09, p = 0.001)")),
                        "Native" = expression("Native" ~ italic("(R² = 0.16, p < 0.001)")),
                        "Non-native" = expression("Non-native" ~ italic("(R² = 0.11, p < 0.001)"))))+
  
  scale_fill_manual(name = "", 
                    values = c("Total" = "black", "Native" = "#BA7333", "Non-native" = "#F8B55C"),
                    breaks = c("Total", "Native", "Non-native"),
                    labels = c(
                      "Total" = expression("Total" ~ italic("(R² = 0.09, p = 0.001)")),
                      "Native" = expression("Native" ~ italic("(R² = 0.16, p < 0.001)")),
                      "Non-native" = expression("Non-native" ~ italic("(R² = 0.11, p < 0.001)"))))+

  theme_classic() +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 18, face = "bold", color = "black"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11), 
        legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"), 
        legend.background = element_blank(), legend.box.background = element_blank(), 
        legend.key.size = unit(0.6, "cm"), legend.spacing.x = unit(0.2, "cm"), 
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1),
        axis.title.x = element_blank())+
  coord_cartesian(ylim = c(0, 35), xlim = c(0,50)) + #IMP
  labs(y = expression(bold("Species richness/m²")), x = "")+
  scale_y_continuous(breaks = seq(0, 30, 10))

RichnessAge <- (regRichnessAge | box_rich) + #Stich all plots together
  plot_layout(widths = c(9,2)) & theme(plot.margin = margin(0,0,0,0))

#4.2 Regression of herbaceous cover of fallows with fallow age 

# Model to see the effect of fallow age on total cover of fallows
lmTotalCoverAge <- lm(data = data_fallow, total_cover_fallow ~ age_fallow) #lm of Total Cover & Age
summary(lmTotalCoverAge) 
coef(lmTotalCoverAge)

# Model to see the effect of fallow age on native cover of fallows
lmNativeCoverAge <- lm(data = data_fallow, native_cover_fallow ~ age_fallow) #lm of Native Cover & Age
summary(lmNativeCoverAge) 
coef(lmNativeCoverAge)
#After solving equation, it takes fallows x = 4.17 years to reach mean of OGs

# Model to see the effect of fallow age on non-native cover of fallows
lmNonNativeCoverAge <- lm(data = data_fallow, non.native_cover_fallow ~ age_fallow) #lm of Non-native Cover & Age
summary(lmNonNativeCoverAge) 
coef(lmNonNativeCoverAge)

#First, plot boxplot of just total, native and non-native cover of the oldgrowth savannahs as a reference 

box_cover <- ggplot(data_OG) +
  geom_boxplot(aes(x = 1, y = Total.Cover , colour = "Total"), 
               width = 0.9, outlier.shape = NA, linewidth = 0.6)+ 
  geom_boxplot(aes(x = 2, y = Native.Cover, colour = "Native"), 
               width = 0.9, outlier.shape = NA, linewidth = 0.6)+ 
  geom_boxplot(aes(x = 3, y = Non.native.Cover, colour = "Non-native"), 
               width = 0.9, outlier.shape = NA, linewidth = 0.6)+
  scale_colour_manual(values = c("Total" = "black", "Native" = "#BA7333", "Non-native" = "#F8B55C")) +
  geom_jitter(aes(x=1, y=Total.Cover, colour = "Total"),width = 0.1, alpha = 0.3, size = 2.5)+
  geom_jitter(aes(x=2, y=Native.Cover, colour = "Native"), width = 0.1, alpha = 0.6, size = 2.5)+
  geom_jitter(aes(x=3, y=Non.native.Cover, colour = "Non-native"), width = 0.1, alpha = 0.3, size = 2.5)+
  stat_summary(aes(x = 1, y = Total.Cover), fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(aes(x = 2, y = Native.Cover), fun = mean, geom = "point", shape = 18, size = 4, color = "#BA7333") +
  stat_summary(aes(x = 3, y = Non.native.Cover), fun = mean, geom = "point", shape = 18, size = 4, color = "#F8B55C") +
  coord_cartesian(ylim = c(0, 150)) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),     # Removes the tick marks
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))

# Now, plot regression of total, native and non-native cover of fallows vs fallow age

regCoverAge <- ggplot(data_fallow, aes(x = age)) +
  annotate("segment", x = 4.17, xend = 4.17, y = 0, yend = -7, #to draw an arrow where Native Richness of fallows reaches that of mean(OGs)
           color = "#BA7333", linetype = "solid", linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  stat_poly_line(aes(y = total_cover_fallow, colour = "Total", fill = "Total"), formula = y ~ x, se = TRUE, alpha = 0.12) +
  geom_point(size = 2.5, alpha = 0.3, aes(y = total_cover_fallow, colour = "Total")) + 
  
  stat_poly_line(aes(y = native_cover_fallow, colour = "Native", fill = "Native"), formula = y ~ x, se = TRUE, alpha = 0.12) +
  geom_point(size = 2.5, alpha = 0.6, aes(y = native_cover_fallow, colour = "Native")) +
  
  stat_poly_line(aes(y = non.native_cover_fallow, colour = "Non-native", fill = "Non-native"),  formula = y ~ x, se = FALSE, 
                 linetype = "dotted" ) + #add dotted line for non-significant relationships
  geom_point(size = 2.5, alpha = 0.3, aes(y = non.native_cover_fallow, colour = "Non-native")) +
  
  scale_colour_manual(name = "", 
                      values = c("Total" = "black", "Native" = "#BA7333", "Non-native" = "#F8B55C"), #use values set color
                      breaks = c("Total", "Native", "Non-native"), #use breaks to set order in legend
                      labels = c("Total" = expression("Total" ~ italic("(R² = 0.08, p = 0.002)")),
                                 "Native" = expression("Native" ~ italic("(R² = 0.08, p = 0.002)")),
                                 "Non-native" = expression("Non-native" ~ italic("(R² = 0.006, p = 0.4)"))))+
  scale_fill_manual(name = "", 
                    values = c("Total" = "black", "Native" = "#BA7333", "Non-native" = "#F8B55C"), #use values set color
                    breaks = c("Total", "Native", "Non-native"), #use breaks to set order in legend
                    labels = c("Total" = expression("Total" ~ italic("(R² = 0.08, p = 0.002)")),
                               "Native" = expression("Native" ~ italic("(R² = 0.08, p = 0.002)")),
                               "Non-native" = expression("Non-native" ~ italic("(R² = 0.006, p = 0.4)"))))+
  theme_classic() +
  theme(axis.title = element_text(size = 18, face = "bold"), #to set title font size
        axis.text = element_text(size = 18, face = "bold", color = "black"), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 11), 
        legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"),  
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.key.size = unit(0.6, "cm"),
        legend.spacing.x = unit(0.2, "cm"),
        axis.title.x = element_blank(),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))+
  coord_cartesian(ylim = c(0, 150), xlim = c(0,50)) +
  scale_y_continuous(breaks = seq(0, 130, by = 20)) +
  labs(y = expression(bold("Herbaceous cover (%)")), x = "")

CoverAge <- (regCoverAge | box_cover) + #Stich all plots together
  plot_layout(widths = c(9, 2)) & theme(plot.margin = margin(0,0,0,0))

#4.3 Regression of Compositional dissimilarity of fallows with fallow age 

# First plot box plot of compositional dissimilarity of old-growth savannahs amongst themselves

og_mean_dissimilarity <- na.omit(dissimilarity_data$dissimilarity_og)
mean(og_mean_dissimilarity) #mean for adding in boxplot  0.634

#Plot

box_comp <- ggplot(data = subset(dissimilarity_data, !is.na(dissimilarity_og))) +
  geom_boxplot(aes(x = 1.3, y = dissimilarity_og, colour = "OG"), 
               width = 0.9, outlier.shape = NA, linewidth = 0.6) +
  scale_colour_manual(values = c("OG" = "black")) +
  geom_jitter(aes(x=1.3, y=dissimilarity_og, colour = "OG"),width = 0.1, alpha = 0.3, size = 2.5)+
  stat_summary(aes(x = 1.3, y = dissimilarity_og), fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  coord_cartesian(ylim = c(0.55, 1.01), xlim = c(0,2.6)) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1),
        plot.caption = element_text(size = 16, 
                                    face = "bold",
                                    hjust = 0.5,
                                    vjust = 13,             
                                    margin = margin(t = 0))) +
  labs(caption = "Old-growth\nsavannah")

# Now, plot regression of Compositional dissimilarity of fallows vs fallow age

CompDis_Age <- ggplot(dissimilarity_data, aes(x = age, y = dissimilarity_fallow)) +
  geom_point(alpha = 0.3, size = 2.5, aes(colour = "Compositional Dissimilarity")) +
  stat_poly_line(aes(color = "Compositional Dissimilarity", fill = "Compositional Dissimilarity"), formula = y ~ x, alpha = 0.12) +
  scale_colour_manual(name = "", values = c("Compositional Dissimilarity" = "black"),
                      breaks = c("Compositional Dissimilarity"),
                      labels = c("Compositional Dissimilarity" = expression(italic("(R² = 0.10, p < 0.001)"))))+
  
  scale_fill_manual(name = "", values = c("Compositional Dissimilarity" = "black"),
                    breaks = c("Compositional Dissimilarity"),
                    labels = c("Compositional Dissimilarity" = expression(italic("(R² = 0.10, p < 0.001)"))))+
  
  coord_cartesian(ylim = c(0.55, 1.01), xlim = c(0,50)) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18, face = "bold", color = "black"), 
    legend.title = element_blank(), 
    legend.text = element_text(size = 11), 
    legend.position = c(0.99, 0.99), 
    legend.justification = c("right", "top"),
    legend.background = element_blank(), 
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0, "cm"),   
    legend.margin = margin(0,0,0,0),     
    legend.spacing.x = unit(0, "cm"),
    panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1)) +
  guides(color = guide_legend(override.aes = list(linetype = 0, linewidth = 0, shape = NA, alpha = 0)))+
  labs(x = expression(bold("Fallow Age (years)")),
       y = expression(bold("Dissimilarity to savannahs")))

DissimilarityAge <- (CompDis_Age | box_comp) + #Stich both plots together
  plot_layout(widths = c(9, 2))& theme(plot.margin = margin(0,0,0,0))

# Arrange all plots as a single panel

Panel1 <- (RichnessAge / CoverAge / DissimilarityAge)  +
  plot_layout(heights = c(1,1,1))+
  plot_annotation(tag_levels = list(c("(A)", "", "(B)", "","(C)", ""))) & 
  theme(plot.tag = element_text(size = 18, face = "bold"),
        plot.margin = margin(3,3,3,3))

Panel1

ggsave(filename = "Figure 2.tiff", #export the panel
       plot = Panel1,
       path = "your folder",
       width = 21, height = 30, units = "cm", dpi = 300)

######################################################################################################################################################### 

#5. Figure 3: Diversity metrics vs Distance to nearest remnant savannah (OG) patch

# Use log10(distance) instead of raw values as they range from 4-26000 metres

#5.1 Regression of herbaceous richness of fallows with distance to nearest remnant savannah 

# Model to see the effect of distance to nearest remnant savanna on total richness of fallows 
lmTotalRichDist <- lm(data = data_fallow, total_richness_fallow ~ log10(dist_fallow)) #lm of Total Richness & Distance
summary(lmTotalRichDist) 
coef(lmTotalRichDist)
#After solving equation by considering y = mean of reference old-growth savannah, fallows reach mean OG values at log10(2.615) or 412m

# Model to see the effect of distance to nearest remnant savanna on native richness of fallows 
lmNativeRichDist <- lm(data = data_fallow, native_richness_fallow ~ log10(dist_fallow)) #lm of native Richness & Distance
summary(lmNativeRichDist)
coef(lmNativeRichDist)
#After solving equation by considering y = mean of reference old-growth savannah, fallows reach mean OG values at log10(1.91) or 81.4m

# Model to see the effect of distance to nearest remnant savanna on non-native richness of fallows 
lmNonNativeRichDist <- lm(data = data_fallow, non.native_richness_fallow ~ log10(dist_fallow)) #lm of non-native Richness & Distance
summary(lmNonNativeRichDist) 
coef(lmNonNativeRichDist)

# Now, plot regression of total, native and non-native richness of fallows vs distance from nearest remnant savannah

regRichnessDist <- ggplot(data_fallow, aes(x=log10(dist_fallow)))+
  annotate("segment", x = 2.615, xend = 2.615, y = 0, yend = -1.5,
           color = "black", linetype = "solid", linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 1.91, xend = 1.91, y = 0, yend = -1.5,
           color = "#BA7333", linetype = "solid", linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  stat_poly_line(aes(y = total_richness_fallow,  colour="Total", fill = "Total"), formula = y ~ x, se = TRUE, alpha = 0.12)+
  geom_point(size = 2.5, alpha = 0.3, aes(y = total_richness_fallow, colour="Total"))+
  
  stat_poly_line(aes(y = native_richness_fallow, colour="Native", fill = "Native"), formula = y ~ x, se = TRUE, alpha = 0.12)+
  geom_point(size = 2.5,alpha = 0.6, aes(y = native_richness_fallow, colour="Native"))+
  
  stat_poly_line(aes (y = non.native_richness_fallow, colour = "Non-native", fill = "Non-native"), formula = y ~ x, se = FALSE, linetype= "dotted")+
  geom_point(size = 2.5,alpha = 0.3, aes(y = non.native_richness_fallow, colour = "Non-native"))+
  
  scale_colour_manual(name = "", values = c("Total" = "black", 
                                            "Native" = "#BA7333", 
                                            "Non-native" = "#F8B55C"),
                      breaks = c("Total", 
                                 "Native", 
                                 "Non-native"),
                      labels = c("Total" = expression("Total" ~ italic("(R² = 0.23, p < 0.001)")),
                                 "Native" = expression("Native" ~ italic("(R² = 0.27, p < 0.001)")),
                                 "Non-native" = expression("Non-native" ~ italic("(R² = 0.02, p = 0.08)")))) +
  scale_fill_manual(name = "", values = c("Total" = "black", 
                                          "Native" = "#BA7333", 
                                          "Non-native" = "#F8B55C"),
                    breaks = c("Total", 
                               "Native", 
                               "Non-native"),
                    labels = c("Total" = expression("Total" ~ italic("(R² = 0.23, p < 0.001)")),
                               "Native" = expression("Native" ~ italic("(R² = 0.27, p < 0.001)")),
                               "Non-native" = expression("Non-native" ~ italic("(R² = 0.02, p = 0.08)")))) +
  theme_classic() +
  theme(axis.title = element_text(size = 18, face = "bold"), #to set title font size
        axis.text = element_text(size = 18, face = "bold", color = "black"), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 11), 
        legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.key.size = unit(0.6, "cm"),
        legend.spacing.x = unit(0.2, "cm"),
        axis.title.x = element_blank(),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))+
  coord_cartesian(ylim = c(0, 35)) +
  scale_y_continuous(breaks = seq(0, 30, 10))+
  scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)),
                     labels = c("10", "100", "1000", "10,000") ) +
  labs(y = expression(bold("Species richness/m²")), x = "")

RichnessDist <- (regRichnessDist | box_rich) + #Stich all plots together
  plot_layout(widths = c(9, 2)) & theme(plot.margin = margin(0, 0, 0, 0))

#5.2 Regression of herbaceous cover of fallows with distance to nearest remnant savannah 

# Model to see the effect of distance to nearest remnant savanna on total cover of fallows 
lmTotalCoverDist <- lm(data = data_fallow, total_cover_fallow ~ log10(dist_fallow)) #lm of Total Cover & Dist
summary(lmTotalCoverDist) 
coef(lmTotalCoverDist)

# Model to see the effect of distance to nearest remnant savanna on native cover of fallows 
lmNativeCoverDist <- lm(data = data_fallow, native_cover_fallow ~ log10(dist_fallow)) #lm of Total Cover & Dist
summary(lmNativeCoverDist) 
coef(lmNativeCoverDist)
#After solving equation by considering y = mean of reference old-growth savannah, fallows reach mean OG values at log10(3.165) or 1462m

# Model to see the effect of distance to nearest remnant savanna on non-native cover of fallows 
lmNonNativeCoverDist <- lm(data = data_fallow, non.native_cover_fallow ~ log10(dist_fallow)) #lm of Total Cover & Dist
summary(lmNonNativeCoverDist)
coef(lmNonNativeCoverDist)
#After solving equation by considering y = mean of reference old-growth savannah, fallows reach mean OG values at log10(1.283) or 19.2m

# Now, plot regression of total, native and non-native richness of fallows vs distance from nearest remnant savannah

regCoverDist <- ggplot(data_fallow, aes(x = log10(dist_fallow)))+
  annotate("segment", x = 3.165, xend = 3.165, y = 0, yend = -7,
           color = "#BA7333", linetype = "solid", linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 1.283, xend = 1.283, y = 0, yend = -7,
           color = "#F8B55C", linetype = "solid", linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  stat_poly_line(aes(y = total_cover_fallow,  colour="Total", fill= "Total"), formula = y ~ x, se = FALSE, linetype = "dotted")+
  geom_point(size = 2.5, alpha = 0.3, aes(y = total_cover_fallow, colour="Total"))+
  stat_poly_line(aes(y = native_cover_fallow, colour="Native", fill = "Native"), formula = y ~ x, se = TRUE, alpha = 0.12 )+
  geom_point(size = 2.5,alpha = 0.6, aes(y = native_cover_fallow, colour="Native"))+
  stat_poly_line(aes (y = non.native_cover_fallow, colour="Non-native", fill = "Non-native"), formula = y ~ x, se = TRUE, alpha = 0.12)+
  geom_point(size = 2.5,alpha = 0.3,aes( y = non.native_cover_fallow,  colour="Non-native"))+
  scale_colour_manual(name = "", values = c("Total" = "black", 
                                            "Native" = "#BA7333", 
                                            "Non-native" = "#F8B55C"),
                      breaks = c("Total", "Native", "Non-native"),
                      labels = c("Total" = expression("Total" ~ italic("(R² = 0.01, p = 0.17)")),
                                 "Native" = expression("Native" ~ italic("(R² = 0.09, p = 0.001)")),
                                 "Non-native" = expression("Non-native" ~ italic("(R² = 0.13, p < 0.001)")))) +
  
  scale_fill_manual(name = "", values = c("Total" = "black", 
                                          "Native" = "#BA7333", 
                                          "Non-native" = "#F8B55C"),
                    breaks = c("Total", "Native", "Non-native"),
                    labels = c("Total" = expression("Total" ~ italic("(R² = 0.01, p = 0.17)")),
                               "Native" = expression("Native" ~ italic("(R² = 0.09, p = 0.001)")),
                               "Non-native" = expression("Non-native" ~ italic("(R² = 0.13, p < 0.001)")))) +
  theme_classic() +
  theme(axis.title = element_text(size = 18, face = "bold"), #to set title font size
        axis.text = element_text(size = 18, face = "bold", color = "black"), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 11), 
        legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.key.size = unit(0.6, "cm"),
        legend.spacing.x = unit(0.2, "cm"),
        axis.title.x = element_blank(),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))+
  coord_cartesian(ylim = c(0, 150)) +
  scale_y_continuous(breaks = seq(0, 130, by = 20))+
  scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)),
                     labels = c("10", "100", "1000", "10,000")) +
  labs(y = expression(bold("Herbaceous cover (%)")), x = "")

CoverDist <- (regCoverDist | box_cover) + #Stich all plots together
  plot_layout(widths = c(9, 2))& theme(plot.margin = margin(0, 0, 0, 0))

#5.3 Regression of compositional dissimilarity of fallows with distance to nearest remnant savannah 

CompDis_dist <- ggplot(dissimilarity_data, aes(x = log10(dist_fallow), y = dissimilarity_fallow)) +
  geom_point(alpha = 0.3, color = "black", size = 2.5, aes(colour = "Compositional Dissimilarity")) +
  stat_poly_line(aes(color = "Compositional Dissimilarity", fill = "Compositional Dissimilarity"), formula = y ~ x, alpha = 0.12) +
  scale_colour_manual(name = "", values = c("Compositional Dissimilarity" = "black"),
                      breaks = c("Compositional Dissimilarity"),
                      labels = c("Compositional Dissimilarity" = expression(italic("(R² = 0.21, p < 0.001)"))))+
  
  
  scale_fill_manual(name = "", values = c("Compositional Dissimilarity" = "black"),
                    breaks = c("Compositional Dissimilarity"),
                    labels = c("Compositional Dissimilarity" = expression(italic("(R² = 0.21, p < 0.001)"))))+
  
  coord_cartesian(ylim = c(0.55, 1.01)) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18, face = "bold", color = "black"), 
    legend.title = element_blank(), 
    legend.text = element_text(size = 11), 
    legend.position = c(0.99, 0.01), 
    legend.justification = c("right", "bottom"), 
    legend.background = element_blank(), 
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0, "cm"),
    legend.spacing.x = unit(0.2, "cm"), 
    panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1)) +
  guides(color = guide_legend(override.aes = list(linetype = 0, linewidth = 0, shape = NA, alpha = 0)))+
  scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)),
                     labels = c("10", "100", "1000", "10,000") ) +
  labs(x = expression(bold("Distance to nearest remnant savannah (m)")),
       y = expression(bold("Dissimilarity to savannahs")))

#Arrange both plots
DissimilarityDist <- (CompDis_dist | box_comp ) + #Stich both plots together
  plot_layout(widths = c(9, 2))& theme(plot.margin = margin(0, 0, 0, 0))

#Arrange as panel and export 

Panel2 <- (RichnessDist / CoverDist / DissimilarityDist)  +
  plot_layout(heights = c(1,1,1))+
  plot_annotation(tag_levels = list(c("(A)", "", "(B)", "","(C)", ""))) & 
  theme(plot.tag = element_text(size = 18, face = "bold"),
        plot.margin = margin(3,3,3,3))

Panel2

ggsave(filename = "Figure 3.tiff", #export the panel
       plot = Panel2,
       path = "your folder",
       width = 21, height = 30, units = "cm", dpi = 300)

####################################################################################################################################################################################

#6. Figure 4: PCA Habitat and soil

#6.1 Soil PCA

soil_data <- read.csv("soil_PCA_master.csv", header = TRUE, row.names = 1)
str(soil_data)

soil_data$landuse <- as.factor(soil_data$landuse)
str(soil_data)
head(soil_data)

soil_data_variables <- subset(soil_data, select = -c(landuse, age, distance)) # remove variables which are not needed for PCA, keep for later

#check distribution by plotting histograms of the raw variables (without non-soil variables):
layout(matrix(1:ncol(soil_data_variables),ncol=1))
par(mar=c(0.1,0.1,0.1,0.1))
for(i in 1:ncol(soil_data_variables)){hist(soil_data_variables[,i],ylab=colnames(soil_data_variables)[i],main="", xlab="")}

#since they are skewed, better to log10(1+x) transform all variables so.
#log 10 transform all variables:
data_soil_log<- log10(1+soil_data_variables[,1:10])

#also need to center all variables to zero by a z-score transformation so they are comparable:
data_soil_log_scaled<- scale(data_soil_log)

#check distribution by plotting histograms again:
layout(matrix(1:ncol(data_soil_log_scaled),ncol=1))
par(mar=c(0.1,0.1,0.1,0.1))
for(i in 1:ncol(data_soil_log_scaled)){hist(data_soil_log_scaled[,i],ylab=colnames(data_soil_log_scaled)[i],main="", xlab="")}
#All variables are centered and seem okayish so proceed.

#re-assing data to the scaled data (can also continue without this step by using 'data_log_scaled' everywhere):
soil_data_variables <- data_soil_log_scaled
head(soil_data_variables)
#looks good. Proceed for the PCA.

#Do the PCA using base 'prcomp' function:

soilPCAout<- prcomp(soil_data_variables)
str(soilPCAout)
summary(soilPCAout)
rownames(soilPCAout$rotation) <- c(
  "pH", 
  "'E. C.'",         
  "'Org. C'",        
  "'Avail. N'",      
  "'Avail. P'",     
  "'Avail. K'",      
  "CEC", 
  "'% Clay'",        
  "'% Silt'",        
  "'% Sand'")

#PC1 explains 25% and PC2 explains 16% variation.

#Now plot the scree plot.
#since the 'par' settings will be copied from the previous plots, adding 'dev.off' resets the settings to default.
dev.off()
plot(soilPCAout)
# plot a cleaner version of the above plot with values linearly transformeE:
barplot(summary(soilPCAout)$importance[2,], ylab="Prop of Variance")

#get PC scores:
soil_scores<- soilPCAout$x
head(soil_scores)

#To export the scores as csv file:
#write.csv(soil_scores,"soil_variables_PCA_axes_scores.csv", row.names = TRUE)

#get PC loadings for each variable:
soil_loadings <- soilPCAout$rotation
soil_loadings

#scale data points as per age values (you can play with coefficients & multipliers):
size_soil<- 7*(age_fallow)/(max(age_fallow)-min(age_fallow)) 

#plotting PC1 and PC2:
#the following code automatically uses x and y axes limits and provides % variation explained
#but some data points fall halfway outside the plot so we manually change x and y lims and provide % variation  

soil_PCA <- ggord(soilPCAout, soil_data$landuse, axes = c("1", "2"), cols = c("grey90", "grey60"), 
                  size=6, alpha=0.9, ellipse=FALSE, hull=FALSE, repel=TRUE, obslab= FALSE, vec_ext=7, txt = 6, veclsz= 0.5, ext=1, arrow= 0.2)+
  scale_x_continuous(limits = c(-4, 6), breaks = c(-4, 0, 4)) +
  scale_y_continuous(limits = c(-4, 4), breaks = c(-4, 0, 4)) +
  geom_point(shape=21, size=6, alpha= 0.2, color="black")+
  theme(legend.position = c(0.98, 0.02), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.justification = c("right", "bottom"),
        axis.title.x=element_text(colour="black", face="bold", size=16), 
        axis.title.y=element_text(colour="black",face="bold",size=16), 
        axis.text.y=element_text(colour="black",face="bold",size=16), 
        axis.text.x=element_text(colour="black",face="bold",size=16), 
        panel.background =element_rect(fill="white",colour="black"), 
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"))+
  guides(shape = "legend", size = "legend")+
  xlab("Soil PC 1 (25.6%)")+ ylab("Soil PC 2 (16.9%)")+
  geom_hline(yintercept = 0, linetype = "dotted", size = 1)+
  geom_vline(xintercept = 0, linetype = "dotted", size =1)

#6.2 Habitat PCA

# read data. The header and row.names argument automatically removes them from the frame!:
data_habitat <- read.csv("habitat variables.csv", header=TRUE, row.names=1)
head(data_habitat)

#first subset the habitat variables from the categorical landuse that will be used to plot later:
data_habitat_variables <- subset(data_habitat, select = -c(landuse, age, distance))
landuse<- as.factor(data_habitat[, 1])

#check distribution by plotting histograms of the raw variables (without landuse variable):
layout(matrix(1:ncol(data_habitat_variables),ncol=1))
par(mar=c(0.1,0.1,0.1,0.1))
for(i in 1:ncol(data_habitat_variables)){hist(data_habitat_variables[,i],ylab=colnames(data_habitat_variables)[i],main="", xlab="")}

#since they are skewed, better to log10(1+x) transform all variables so.
#log 10 transform all variables:
data_habitat_log<- log10(1+data_habitat_variables[,1:6])

#also need to center all variables to zero by a z-score transformation so they are comparable:
data_habitat_log_scaled<- scale(data_habitat_log)

#check distribution by plotting histograms again:
layout(matrix(1:ncol(data_habitat_log_scaled),ncol=1))
par(mar=c(0.1,0.1,0.1,0.1))
for(i in 1:ncol(data_habitat_log_scaled)){hist(data_habitat_log_scaled[,i],ylab=colnames(data_habitat_log_scaled)[i],main="", xlab="")}
#All variables are centered and seem okayish so proceed.

#re-assigning data to the scaled data (can also continue without this step by using 'data_log_scaled' everywhere):
data_habitat_variables <- data_habitat_log_scaled
head(data_habitat_variables)
#looks good. Proceed for the PCA.

#Do the PCA using base 'prcomp' function:

PCAhabitatout<- prcomp(data_habitat_variables)
str(PCAhabitatout)
summary(PCAhabitatout)
rownames(PCAhabitatout$rotation)
rownames(PCAhabitatout$rotation) <- c("'Rock cover'", 
                                      "'Bare ground'", 
                                      "'Large tree basal area'", 
                                      "'Small tree basal area'",
                                      "'Shrub cover'", 
                                      "'Canopy cover'")

#PC1 explains 36.3% and PC2 explains 21% variation.
#Now plot the scree plot.
#since the 'par' settings will be copied from the previous plots, adding 'dev.off' resets the settings to default.
dev.off()
plot(PCAhabitatout)
# plot a cleaner version of the above plot with values linearly transformed:
barplot(summary(PCAhabitatout)$importance[2,], ylab="Prop of Variance")

#get PC scores:
habitat_scores<- #get PC scores:
  habitat_scores<- PCAhabitatout$x
head(habitat_scores)

#To export the scores as csv file:
#write.csv(habitat_scores,"habitat_variables_PCA_axes_scores.csv", row.names = TRUE)

#get PC loadings for each variable:
loadings <- PCAhabitatout$rotation
loadings

#plotting PC1 and PC2:
#the following code automatically uses x and y axes limits and provides % variation explained
#but some data points fall halfway outside the plot so we manually change x and y lims and provide % variation  

habitat_variables_PCA <- ggord(PCAhabitatout, data_habitat$landuse, axes = c("1", "2"), cols = c("grey90", "grey60"), 
                               size=6, alpha=0.9, ellipse=FALSE, hull=FALSE, repel=TRUE, obslab= FALSE, txt = 6, vec_ext=5, veclsz= 0.5, ext=1, arrow= 0.2)+
  geom_hline(yintercept = 0, linetype = "dotted", size = 1)+
  geom_vline(xintercept = 0, linetype = "dotted", size =1)+
  scale_x_continuous(limits = c(-2, 7), breaks = c(-4, 0, 4)) +
  scale_y_continuous(limits = c(-4, 4), breaks = c(-4, 0, 4)) +
  geom_point(shape=21, size=6, alpha= 0.2, color="black")+
  theme(legend.background = element_blank(),
        legend.text = element_text(size = 12), 
        legend.position = c(0.98, 0.02), 
        legend.title = element_blank(),
        legend.justification = c("right", "bottom"),
        axis.title.x=element_text(colour="black",face="bold",size=16),
        axis.title.y=element_text(colour="black",face="bold",size=16),
        axis.text.y=element_text(colour="black",face="bold",size=16), 
        axis.text.x=element_text(colour="black",face="bold",size=16), 
        panel.background =element_rect(fill="white",colour="black"),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1),
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"))+
  guides(shape = "legend", size = "legend")+
  xlab("Habitat structure PC 1 (36.3%)")+ 
  ylab("Habitat structure PC 2 (21%)")

#6.3 Linear models for soil and habitat variables PC1, PC2 and native richness and native cover

#import dataset after manually adding landuse, age, distance and five PC scores each for soil and habitat variables
PCA_scores_df <- read.csv("PCA scores for analysis.csv") 

PCA_scores_df_fallows <- subset(PCA_scores_df, landuse == "fallow") #subset dataset to only include fallows
str(PCA_scores_df_fallows) #check

# Model to see the effect of fallow age on Soil PC1 and PC2 

soil_PC1_age <- lm(PCA_scores_df_fallows$soil_PC1 ~ PCA_scores_df_fallows$age)
summary(soil_PC1_age)  #p<0.001, R²=0.10

soil_PC2_age <- lm(PCA_scores_df_fallows$soil_PC2 ~ PCA_scores_df_fallows$age)
summary(soil_PC2_age) #Distance is p = 0.049, R²=0.03

# Model to see the effect of distance to nearest remnant savannah on Soil PC1 and PC2 

soil_PC1_dist <- lm(PCA_scores_df_fallows$soil_PC1 ~ log10(PCA_scores_df_fallows$distance))
summary(soil_PC1_dist)  #Non significant; p=0.05, R²=0.03

soil_PC2_dist <- lm(PCA_scores_df_fallows$soil_PC2 ~ log10(PCA_scores_df_fallows$distance))
summary(soil_PC2_dist) #Distance is p<0.001, R²=0.13

#Now, check the same for habitat PC1 and habitat PC2

# Model to see the effect of fallow age on Habitat PC1 and PC2 

habitat_PC1_age <- lm(PCA_scores_df_fallows$habitat_PC1 ~ PCA_scores_df_fallows$age)
summary(habitat_PC1_age)  #p<0.001, R²=0.10

habitat_PC2_age <- lm(PCA_scores_df_fallows$habitat_PC2 ~ PCA_scores_df_fallows$age)
summary(habitat_PC2_age) #Non-significant; p=0.3, R²=0.008

# Model to see the effect of distance to nearest remnant savannah on Habitat PC1 and PC2 

habitat_PC1_dist <- lm(PCA_scores_df_fallows$habitat_PC1 ~ log10(PCA_scores_df_fallows$distance))
summary(habitat_PC1_dist)  #p=0.009, R²=0.06

habitat_PC2_dist <- lm(PCA_scores_df_fallows$habitat_PC2 ~ log10(PCA_scores_df_fallows$distance))
summary(habitat_PC2_dist)  #p=0.048, R²=0.03

#Prepare plots

Soil.PCA.Age <- ggplot(data = PCA_scores_df_fallows, aes(x = age_fallow)) +
  stat_poly_line(aes(y = soil_PC1, color = "PC1", fill = "PC1"),
                 formula = y ~ x, se = TRUE, alpha = 0.3) +
  stat_poly_line(aes(y = soil_PC2, color = "PC2", fill = "PC2"),
                 formula = y ~ x, se = TRUE, alpha = 0.6) +
  
  scale_y_continuous(name = "Soil PC1",
                     breaks = c(-2, 0, 2, 4),
                     sec.axis = sec_axis(~., name = "Soil PC2", breaks = c(-2, 0, 2, 4))) +
  
  scale_x_continuous(name = "Age (years)",
                     breaks = c(1, 10, 25, 50)) +
  
  coord_cartesian(xlim = c(0, 50), ylim = c(-2, 5)) +
  
  scale_color_manual(name = NULL,
                     values = c("PC1" = "#7d8ea1", "PC2" = "#95b05f"),
                     labels = c(
                       "PC1" = expression("PC1" ~ italic("(R² = 0.10, p < 0.001)")),
                       "PC2" = expression("PC2" ~ italic("(R² = 0.03, p = 0.049)"))))+
  
  scale_fill_manual(name = NULL,
                    values = c("PC1" = "#7d8ea1", "PC2" = "#95b05f"),
                    labels = c(
                      "PC1" = expression("PC1" ~ italic("(R² = 0.10, p < 0.001)")),
                      "PC2" = expression("PC2" ~ italic("(R² = 0.03, p = 0.049)"))))+
  
  theme_classic() +
  
  theme(axis.title = element_text(size = 16, face = "bold", color = "black"),
        axis.text = element_text(size = 16, face = "bold", color = "black"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 9), 
        legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.key.size = unit(0.4, "cm"), 
        legend.spacing.x = unit(0.2, "cm"),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))

# Plot distance against Soil PCA

Soil.PCA.Dist <- ggplot(data = PCA_scores_df_fallows, aes(x = log10(distance))) +
  stat_poly_line(aes(y = soil_PC1, color = "PC1", fill = "PC1"),
                 formula = y ~ x, se = FALSE, alpha = 0.3, linetype = "dotted") +
  stat_poly_line(aes(y = soil_PC2, color = "PC2", fill = "PC2"),
                 formula = y ~ x, se = TRUE, alpha = 0.6) +
  
  scale_y_continuous(name = "Soil PC1",
                     breaks = c(-2, 0, 2, 4),
                     sec.axis = sec_axis(~., name = "Soil PC2", breaks = c(-2, 0, 2, 4))) +
  
  scale_x_continuous(name = "Distance (m)",
                     breaks = log10(c(10, 100, 10000)),
                     labels = c("10", "100", "10,000")) +
  
  coord_cartesian(xlim = c(0.55, 4.5), ylim = c(-2, 5)) +
  scale_color_manual(name = NULL, 
                     values = c("PC1" = "#7d8ea1", "PC2" = "#95b05f"),
                     labels = c(
                       "PC1" = expression("PC1" ~ italic("(R² = 0.03, p = 0.053)")),
                       "PC2" = expression("PC2" ~ italic("(R² = 0.12, p < 0.001)"))))+
  
  scale_fill_manual(name = NULL,
                    values = c("PC1" = "#7d8ea1", "PC2" = "#95b05f"),
                    labels = c(
                      "PC1" = expression("PC1" ~ italic("(R² = 0.03, p = 0.053)")),
                      "PC2" = expression("PC2" ~ italic("(R² = 0.12, p < 0.001)"))))+
  
  theme_classic() +
  
  theme(axis.title = element_text(size = 16, face = "bold", color = "black"),
        axis.text = element_text(size = 16, face = "bold", color = "black"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 9), 
        legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.key.size = unit(0.4, "cm"), 
        legend.spacing.x = unit(0.2, "cm"),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))

#Now, Habitat PCs

Habitat.PCA.Age <- ggplot(data = PCA_scores_df_fallows, aes(x = age_fallow)) +
  
  stat_poly_line(aes(y = habitat_PC1, color = "PC1", fill = "PC1"),
                 formula = y ~ x, se = TRUE, alpha = 0.3) +
  
  stat_poly_line(aes(y = habitat_PC2, color = "PC2", fill = "PC2"),
                 formula = y ~ x, se = FALSE, alpha = 0.6, linetype = "dotted") +
  
  scale_y_continuous(name = "Structure PC1",
                     breaks = c(-2, 0, 2, 4),
                     sec.axis = sec_axis(~., name = "Structure PC2", breaks = c(-2, 0, 2, 4))) +
  
  scale_x_continuous(name = "Age (years)",
                     breaks = c(1, 10, 25, 50)) +
  
  coord_cartesian(xlim = c(0, 50), ylim = c(-2, 5)) +
  
  scale_color_manual(name = NULL,
                     values = c("PC1" = "#7d8ea1", "PC2" = "#95b05f"),
                     labels = c(
                       "PC1" = expression("PC1" ~ italic("(R² = 0.10, p < 0.001)")),
                       "PC2" = expression("PC2" ~ italic("(R² = 0.008, p = 0.34)"))))+
  
  scale_fill_manual(name = NULL,
                    values = c("PC1" = "#7d8ea1", "PC2" = "#95b05f"),
                    labels = c(
                      "PC1" = expression("PC1" ~ italic("(R² = 0.10, p < 0.001)")),
                      "PC2" = expression("PC2" ~ italic("(R² = 0.008, p = 0.34)"))))+
  
  labs(x = "Age (years)", 
       color = NULL, 
       fill = NULL) +
  
  theme_classic() +
  
  theme(axis.title = element_text(size = 16, face = "bold", color = "black"),
        axis.text = element_text(size = 16, face = "bold", color = "black"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 9), 
        legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.key.size = unit(0.4, "cm"), 
        legend.spacing.x = unit(0.2, "cm"),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))


#Now habitat PCs against distance
Habitat.PCA.Dist <- ggplot(data = PCA_scores_df_fallows, aes(x = log10(distance))) +
  
  stat_poly_line(aes(y = habitat_PC1, color = "PC1", fill = "PC1"),
                 formula = y ~ x, se = TRUE, alpha = 0.3) +
  
  stat_poly_line(aes(y = habitat_PC2, color = "PC2", fill = "PC2"),
                 formula = y ~ x, se = TRUE, alpha = 0.6) +
  
  scale_y_continuous(name = "Structure PC1",
                     breaks = c(-2, 0, 2, 4),
                     sec.axis = sec_axis(~., name = "Structure PC2", breaks = c(-2, 0, 2, 4))) +
  
  scale_x_continuous(name = "Distance (m)",
                     breaks = log10(c(10, 100, 10000)),
                     labels = c("10", "100", "10,000")) +
  
  coord_cartesian(xlim = c(0.55, 4.5), ylim = c(-2, 5)) +
  
  scale_color_manual(name = NULL,
                     values = c("PC1" = "#7d8ea1", "PC2" = "#95b05f"),
                     labels = c(
                       "PC1" = expression("PC1" ~ italic("(R² = 0.06, p = 0.009)")),
                       "PC2" = expression("PC2" ~ italic("(R² = 0.03, p = 0.048)"))))+
  
  scale_fill_manual(name = NULL,
                    values = c("PC1" = "#7d8ea1", "PC2" = "#95b05f"),
                    labels = c(
                      "PC1" = expression("PC1" ~ italic("(R² = 0.06, p = 0.009)")),
                      "PC2" = expression("PC2" ~ italic("(R² = 0.03, p = 0.048)"))))+
  
  theme_classic() +
  
  theme(axis.title = element_text(size = 16, face = "bold", color = "black"),
        axis.text = element_text(size = 16, face = "bold", color = "black"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 9), 
        legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.key.size = unit(0.4, "cm"), 
        legend.spacing.x = unit(0.2, "cm"),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1))

# Combine the plots
Habitat_combined <- habitat_variables_PCA / 
  (Habitat.PCA.Age | Habitat.PCA.Dist) + 
  plot_layout(
    heights = c(2.6, 1)
  ) & 
  theme(aspect.ratio = 1,
        plot.margin = margin(t = 3, r = 3, b = 30, l = 3, unit = "pt"))

# Save the plot
ggsave(filename = "Figure 4a.tiff",
       plot = Habitat_combined,
       scale = 1.15,
       path = "your folder",
       width = 15, height = 20,
       units = "cm", dpi = 300)

# Combine the plots
Soil_combined <- soil_PCA / 
  (Soil.PCA.Age | Soil.PCA.Dist) +
  plot_layout(
    heights = c(2.6, 1) 
  ) & 
  theme(aspect.ratio = 1,
        plot.margin = margin(t = 3, r = 3, b = 30, l = 3, unit = "pt"))

# Save the plot
ggsave(filename = "Figure 4b.tiff",
       plot = Soil_combined,
       scale = 1.15,
       path = "your folder",
       width = 15, height = 20, 
       units = "cm", dpi = 300)

####################################################################################################################################################################################

#7. Figure 5: Future prediction framework

#This figure provides a general framework by visualizing the contributions of fallow age and distance to nearest remnant on compositional dissimilarity 

# Define custom colors
light_blue <- "#6b8dd1" 
dark_orange  <- "#de9c6f" 

AgeVsDist <- ggplot(data = data_fallow, 
                    aes(x = log10(dist_fallow), 
                        y = log10(age_fallow),
                        fill = dissim_fallow)) +
  
  # Horizontal Lines Fallow Age
  geom_hline(yintercept = log10(14.22), linetype = "dashed", color = "black", linewidth = 0.7) + #Global average time of recultivation (years)
  geom_hline(yintercept = log10(5), linetype = "dashed", color = "black", linewidth = 0.7) + #India-level time of recultivation
  
  # Vertical Lines Distances 
  geom_vline(xintercept = log10(1.757), linetype = "dashed", color = "black", linewidth = 0.7) + #Herbs-ballistic 95th #3
  geom_vline(xintercept = log10(17.91), linetype = "dashed", color = "black", linewidth = 0.7) + #Tall wind-dispersed herbs 95th #4
  geom_vline(xintercept = log10(45.93), linetype = "dashed", color = "black", linewidth = 0.7) + #Tall graminoids median #5
  geom_vline(xintercept = log10(123.3), linetype = "dashed", color = "black", linewidth = 0.7) + #Tall graminoids 95th #6
  
  geom_point(shape = 21,    
             size = 8, 
             color = "black", 
             stroke = 0.4,   
             alpha = 0.7) +
  
  theme_classic() +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 18, face = "bold", color = "black"),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1),
        legend.title = element_text(face = "bold", size = 12),
        legend.position = "top") +
  
  scale_fill_gradientn(name = "Compositional dissimilarity",
                       colors = c(light_blue, dark_orange), 
                       limits = c(0.60, 1)) +
  
  scale_y_continuous(breaks = log10(c(1, 2.5, 5, 10, 25, 50)),
                     labels = c("1", "2.5", "5", "10", "25", "50"))+
  scale_x_continuous(breaks = log10(c(10, 100, 1000, 10000)),
                     labels = c("10", "100", "1000", "10,000")) +
  
  labs(y = "", 
       x = "")

AgeVsDist

# Save the plot
ggsave(filename = "Figure 5.tiff",
       plot = AgeVsDist,
       path = "your folder",
       width = 20, height = 15, units = "cm", dpi = 300)

####################################################################################################################################################################################

#8. Table 1: Interaction terms table

#Create linear models for biodiversity metrics

lmTotalRichAgeDist <- lm(total_richness_fallow ~ fallow_age * log10(fallow_distance) )
summary(lmTotalRichAgeDist)

lmNativeRichAgeDist <- lm(native_richness_fallow ~ fallow_age * log10(fallow_distance))
summary(lmNativeRichAgeDist)

lmNonNativeRichAgeDist <- lm(non.native_richness_fallow ~ fallow_age * log10(fallow_distance))
summary(lmNonNativeRichAgeDist)

lmTotalCoverAgeDist <- lm(total_cover_fallow ~ fallow_age * log10(fallow_distance))
summary(lmTotalCoverAgeDist)

lmNativeCoverAgeDist <- lm(native_cover_fallow ~ fallow_age * log10(fallow_distance))
summary(lmNativeCoverAgeDist)

lmNonNativeCoverAgeDist <- lm(non.native_cover_fallow ~ fallow_age * log10(fallow_distance))
summary(lmNonNativeCoverAgeDist)

lmComDisAgeDist <- lm(compDis_fallow ~ fallow_age * log10(fallow_distance))
summary(lmComDisAgeDist)

#Now, create list of all models and name them

model_AgeDist <- list(
  lmTotalRichAgeDist, lmNativeRichAgeDist, lmNonNativeRichAgeDist,
  lmTotalCoverAgeDist, lmNativeCoverAgeDist, lmNonNativeCoverAgeDist,
  lmComDisAgeDist)

#export table

stargazer(model_AgeDist, 
          type = "html",                    
          out = "Table 1.doc", 
          report = "vcsp*",                 
          digits = 3,                       
          single.row = TRUE,              
          star.cutoffs = c(0.05, 0.01, 0.001),
          column.labels = c("Total Richness", "Native Richness", "Non-Native Richness", 
                            "Total Cover", "Native Cover", "Non-Native Cover",
                            "Compositional dissimilarity"),
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep.stat = c("rsq"), # Only keeps R2
          notes.append = FALSE,      # Helps in removing the default note
          notes = "")

####################################################################################################################################################################################
