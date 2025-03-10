#### Outline analysis of all complete bladelets recovered in the studied sequence of Castelcivita ####

#0 Packages used ####
library(Momocs)
library(ggplot2)
library(ggpubr)
library(readxl)
library(forcats)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
library(MetBrewer)
library(rstatix)
library(vegan)
library(pairwiseAdonis)
library(cowplot)

Dataset <- read.csv("data/CTC_Dataset_complete.csv")
Dataset_cores <- read.csv("data/CTC_Dataset_cores.csv")

#1 reading the file names ####
lf <- list.files("data/CTC_Outlines", pattern = "\\.txt$",full.names=T)

#2 modifying the dataset ####
Dataset_2DGM <- filter(Dataset, Blank=="Bladelet", Preservation=="Complete", Layer != "ars") %>%
  unite("Unit_Class", Layer, Class, sep= "_", 
        remove = FALSE) %>%
  unite("Class_Retouch", Class, Retouch_position, sep= "_", 
        remove = FALSE) %>%
  unite("Unit_Class_Retouch", Layer, Class, Retouch_position, sep= "_",
        remove = FALSE) %>%
  mutate(Class_Retouch = recode(Class_Retouch, Blank_NA = "Blank")) %>%
  mutate(Unit_Class_Retouch = recode(Unit_Class_Retouch, `rsa'_Blank_NA` = "rsa'_Blank", `gic_Blank_NA` = "gic_Blank", gic_Tool_Alternate = "gic_Tool_AltInv", gic_Tool_Inverse = "gic_Tool_AltInv", `rsa'_Tool_Alternate` = "rsa'_Tool_AltInv", `rsa'_Tool_Inverse` = "rsa'_Tool_AltInv"))

#3 extracting the coordinates and attaching the IDs ####
Coordinates <- import_txt(lf)

Coordinates_2DGM <- Dataset_2DGM %>%
  pull("ID") %>%
  as.character()

Coordinates_2DGM_out <- Coordinates[Coordinates_2DGM]

GM_bladelets <- Out(Coordinates_2DGM_out, fac = dplyr::select(Dataset_2DGM, c("Unit_Class",  "Class_Retouch", "Unit_Class_Retouch"))) #used to create more factors using dplyr instead of using fac = namedataframe$variable inside

panel(GM_bladelets, names=FALSE) #inspect the outlines

# tiff("figures/2DGM/All_outlines.tiff", width = 2024, height = 2024, res = 300)
# panel(GM_bladelets, names=FALSE) #inspect the outlines
# dev.off() # In case you wanna save it


#4 GMM procedures to centre, scale, and rotare the coordinates
GM_bladelets_centered <- Momocs::coo_centre(GM_bladelets) # Used to center
GM_bladelets_centered_scaled <- Momocs::coo_scale(GM_bladelets_centered) # scale
GM_bladelets_centered_scaled <- Momocs::coo_slidedirection (GM_bladelets_centered_scaled)
GM_bladelets_centered_scaled_rotated <- Momocs::coo_rotatecenter(GM_bladelets_centered_scaled, theta = -pi/2)

# stack inspections
stack(GM_bladelets_centered)
stack(GM_bladelets_centered_scaled)
stack(GM_bladelets_centered_scaled_rotated)

# stack inspection
stack(GM_bladelets_centered_scaled_rotated)

# tiff("Figures/2DGM/GM_bladelets_centered_scaled.tiff", width = 1024, height = 1024, res = 200) #to save plots
# stack(GM_bladelets_centered_scaled_rotated)
# dev.off() # To save the figure

#4 calibrating harmonic power ####
harmonic.power <- calibrate_harmonicpower_efourier(GM_bladelets_centered_scaled_rotated) #Estimates the number of harmonics required for the four Fourier methods implemented in Momocs

#5 EFA analysis ####
GM_bladelets_centered_scaled_rotated.EFA <- efourier(GM_bladelets_centered_scaled_rotated, nb.h=23) #Num. of harmonics set to 10 according to the 99%. Change accoridng to the outcome of the previous test

#6 Mean shapes  ####

GM_bladelets_centered_scaled_rotated.EFA %>% MSHAPES() %>% coo_plot() #to display the mean shape of the whole data set

GM_Class_Mean_Shapes <- MSHAPES(GM_bladelets_centered_scaled_rotated.EFA, ~Unit_Class_Retouch)
Out(GM_Class_Mean_Shapes$shp) %>% panel(names=TRUE, cex.names = 0.5) # to plot this stuff as mean shape per group

# tiff("Figures/2DGM/Mean_shapes_separated.tiff", width = 1824, height = 1824, res = 300) #to save plots
# Out(GM_Class_Mean_Shapes$shp) %>% panel(names=TRUE)
# dev.off() # export the figure

# Two-way comparison plotting them in a matrix, figure further modified in Adobe Illustrator to improve its quality. The raw file is provided in the figures folder
plot_MSHAPES(GM_Class_Mean_Shapes)
#and then save it:
tiff("output/figures/Mean_shapes_raw.png", units = "in", width = 10, height = 10, res = 300)
plot_MSHAPES(GM_Class_Mean_Shapes, size= 0.9, palette=pal_manual(c("darkblue", "red")))
dev.off()

# plot_MSHAPES(GM_Class_Mean_Shapes)
# # to save it:
# tiff("figures/2DGM/Mean_shapes_compared.reduced.tiff", units = "in", width = 3, height =3, res = 200)
# plot_MSHAPES(GM_Class_Mean_Shapes, size= 0.5, palette=pal_manual(c("darkblue", "red")))
# dev.off()

#7 PCA: Principal Component Analysis on EFDs ####
GM_bladelets_centered_scaled_rotated.PCA <- PCA(GM_bladelets_centered_scaled_rotated.EFA)

# #8 Linear discriminant analysis ####
# GM_bladelets_centered_scaled_rotated.LDA <- LDA(GM_bladelets_centered_scaled_rotated.PCA, ~Unit_Class_Retouch)

# #9 Cluster analysis, to see if worth doing it ####
# 
# GM_cluster_analysis <- CLUST(GM_bladelets_centered_scaled_rotated.PCA,
#                           ~Unit_Class_Retouch,
#                           dist_method = "euclidean",
#                           retain = 0.9)


#10 Screeplot ####
GM_bladelets_screeplot <- Momocs::scree_plot(GM_bladelets_centered_scaled_rotated.PCA, nax = 1:8) +
  cowplot::theme_minimal_grid() +
  theme(plot.title = element_blank(),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        text=element_text(size=14)) +
  labs(x = "Components", y = "Proportion") +
  theme(plot.title = element_blank())

GM_bladelets_screeplot

ggsave(GM_bladelets_screeplot,
       filename = file.path("output/figures/Figure_S19.png"),
       width = 4,
       height = 4,
       dpi = 300,
       units = "in",
       device = "png")


#11 Shape variation ####
GM_bladelets_shape_variation <- Momocs::PCcontrib(GM_bladelets_centered_scaled_rotated.PCA,nax = 1:4, sd.r = c(-2,-1,0,1,2))


#to change orientation
GM_bladelets_shape_variation_flipped <- GM_bladelets_shape_variation$gg +
  scale_y_continuous(labels(NULL)) +
  scale_x_reverse(labels(NULL)) +
  coord_flip()

GM_bladelets_shape_variation_flipped <- GM_bladelets_shape_variation$gg +
  scale_y_continuous(labels(NULL)) +
  scale_x_reverse(labels(NULL), sec.axis = sec_axis(~., name = "PC")) +
  coord_flip()


GM_bladelets_shape_variation_flipped <-
  GM_bladelets_shape_variation_flipped +
  labs(title = "Mean + SD") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        text=element_text(size=14))

GM_bladelets_shape_variation_flipped


# ggsave(GM_bladelets_shape_variation_flipped,
#        filename = file.path("Shape_variation_plot.tiff"),
#        width = 4,
#        height = 5,
#        dpi = 300,
#        units = "in",
#        device = "tiff")


#12 Merging datasets to produce figures and run other analyses ####
GM_PCScores <- as.data.frame(GM_bladelets_centered_scaled_rotated.PCA$x)

GM_PCScores$ID <- Dataset_2DGM$ID

Dataset_2DGM <- left_join(Dataset_2DGM, GM_PCScores[ ,c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "ID")], by = "ID") #using diplyr to select certain specific columns
# Dataset <- left_join(BlankTypes, PCScores[ ,c(1:4, 50)], by = "ID") #another way to do it with numbers


#13 Correlation between length and PC1 ####

GM_PC1toLength <- ggplot(data = Dataset_2DGM, aes(x = Length, y = PC1)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) +
  ggpubr::stat_cor(aes(label = after_stat(label)), method = "spearman", color = "red", geom = "label") +
  labs(y= "PC1", x = "Length (mm)") +
  scale_color_met_d("Hokusai1") +
  scale_fill_met_d("Hokusai1") +
  ggthemes::theme_clean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size=12),
        text = element_text(size = 14))


  ggsave(GM_PC1toLength, 
         filename = "output/figures/Figure_S20.png", 
         width = 25, 
         height = 17, 
         dpi = 300, 
         units = "cm", 
         device = 'png')


#Spearman test
Spearman <- cor.test(Dataset_2DGM$Length, Dataset_2DGM$PC1, method=c("spearman"), exact = F)

#14 Calculate the PC means for plotting results ####

Dataset_2DGM$Unit_Class_Retouch <- as.factor(Dataset_2DGM$Unit_Class_Retouch) #define Artifact Class as factor vector    
  
Means_PC1_PC3 <- Dataset_2DGM %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class
    group_by(Unit_Class_Retouch) %>% 
    summarise(PC1 = mean(PC1),
              PC2 = mean(PC2),
              PC3 = mean(PC3))

Means_PC1_PC3_unit_class <- Dataset_2DGM %>%                                      #subset dataset by Artifact Class and calculate means for PCs for each Class
  group_by(Unit_Class) %>% 
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3))


# Means_Length_Width <- Dataset_2DGM %>%                                            #subset dataset by Artifact Class and calculate means for PCs for each Class
#   group_by(Unit_Class_Retouch) %>% 
#   summarise(Length = mean(Length),
#             Width = mean(Width),
#             Thickness = mean(Thickness)) # If needed to plot the PCs with the length


#14 Plotting results of the PCA ####

# Scatterplot PC1-PC2
PC1toPC2 <- ggplot(data = Dataset_2DGM, aes(x = PC1, y = PC2, color = Unit_Class_Retouch)) +
  geom_point(size = 1.5, alpha = 0.2) +
  #stat_ellipse(level = 0.8) +
  # stat_chull(aes(color = Unit_Class_Retouch, fill = Unit_Class_Retouch), alpha = 0.3, geom = "polygon") +
  # scale_color_viridis(discrete = TRUE, direction = 1, option = "C") + scale_fill_viridis(discrete = TRUE, direction = 1, option = "C") +
  labs(y= "PC2 (11% of total variance)", x = "PC1 (67% of total variance)") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +
  geom_point(data = Means_PC1_PC3, 
             size = 4.5, shape = 16) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_pubclean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12)) +
  scale_fill_manual(values=met.brewer("Lakota", 6)) +
  scale_color_manual(labels=c("gic, blank (n = 453)", "gic, alt.-inv. (n = 4)", "gic, direct (n = 107)", "rsa', blank (n = 252)", "rsa', alt.-inv. (n = 15)", "rsa', direct (n = 10)"), values=met.brewer("Lakota",6))

PC1toPC2

# ggsave(PC1toPC2_blanks, 
#        filename = "PC1_PC2_blanks.tiff", 
#        width = 25, 
#        height = 17, 
#        dpi = 300, 
#        units = "cm", 
#        device = 'tiff')


# Boxplots PC1
PC1_2DGM_Boxplot <- Dataset_2DGM %>%
  mutate(Unit_Class_Retouch = recode(Unit_Class_Retouch, gic_Blank = "Blank", gic_Tool_AltInv = "Alt.-inv.", gic_Tool_Direct = "Direct", `rsa'_Blank` = "Blank", `rsa'_Tool_AltInv` = "Alt.-inv.", `rsa'_Tool_Direct` = "Direct")) %>%
  mutate(Unit_Class_Retouch = fct_relevel(Unit_Class_Retouch, "Blank", "Direct", "Alt.-inv.")) %>%
  ggplot(aes(x = Unit_Class_Retouch, y = PC1, fill = Layer)) +
  geom_boxplot(alpha = 0.85) +
  # geom_jitter(width = 0.1, alpha = 0.2) +
  labs(y= "PC1", x = "Class & Retouch") +
  theme_pubclean() +
  scale_fill_manual(values=met.brewer("Isfahan1", 2)) +
  theme(text = element_text(size = 14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 16),
        legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

PC1_2DGM_Boxplot


ggsave(filename = "output/figures/Figure_9.tiff", width = 8, height = 8, units = "in", bg = "white", dpi=300, plot=(
  ggdraw() +
    draw_plot(GM_bladelets_shape_variation_flipped, x = 0, y = 0.6, width = .37, height = .40) +
    draw_plot(PC1_2DGM_Boxplot, x = .4, y = 0.6, width = .6, height = .40) +
    draw_plot(PC1toPC2, x = 0, y = 0, width = 1, height = .60)  +
    draw_plot_label(label = c("a", "b", "c"),
                    x = c(0, .4, 0), y = c(1, 1, .6), size=14)
))


#15 Testing differences ####

# Kruskall-Wallis tests

Dataset_2DGM %>%
kruskal_test(PC1 ~ Layer)

Dataset_2DGM %>%
  kruskal_test(PC2 ~ Layer)

Dataset_2DGM %>%
  kruskal_test(PC3 ~ Layer)

# PERMANOVA and Pairwise

min_n_PCs.2DGM <- Momocs::scree_min(GM_bladelets_centered_scaled_rotated.PCA,
                                                prop = 0.95) 
min_n_PCs.2DGM # 95% of the data variability


# Create the Y matrix of variables under comparison:
Y.PERMANOVA <- Dataset_2DGM[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")]

# Perform a one-way PERMANOVA:
GM_PERMANOVA <- adonis2(Y.PERMANOVA ~ Dataset_2DGM$Unit_Class_Retouch, method = "euclidean",
       permutations = 10000)

# Pairwise differences:
Pairwise_PERMANOVA <- pairwise.adonis(Y.PERMANOVA, Dataset_2DGM$Unit_Class_Retouch, sim.method = "euclidean",
                p.adjust.m = "bonferroni", perm = 10000)