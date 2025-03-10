# 0. Loading necessary packages ----
library(Momocs)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(viridis)
library(cowplot)
library(vegan)
library(pairwiseAdonis)
library(MetBrewer)
library(dispRity)
library(readxl)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(cowplot)
library(MetBrewer)
library(ggpubr)

# New color palette for sea, shells, and sand
sea_shell_sand_palette <- c("#FF6347", "#1F77B4", "#D4AF37")

# 1. Data import ----

dataset <- read_excel("data/dataset.xlsx") %>%
  select(ID, Type, Area.perforation, Circumference.perforation)

write.csv(dataset, "data/dataset.csv", row.names = FALSE)

dataset <- read.csv("data/dataset.csv")

ids_to_keep <- dataset$ID

lf <- list.files("data/outlines", pattern = "\\.txt$", full.names = TRUE)

print(lf)

lf <- list.files("data/outlines", pattern = "\\.txt$", full.names = TRUE)

lf2 <- lf[basename(lf) %in% paste0(ids_to_keep, ".txt")]

print(lf2)

# 2. Extracting coordinates ----
Coordinates <- import_txt(lf2)

Coordinates_perforation <- dataset %>%
  pull("ID") %>%
  as.character()

Coordinates_2DGM <- Coordinates[Coordinates_perforation]

set.seed(123)  # Set seed for reproducibility

GM_perforation <- Out(Coordinates_2DGM, fac = select(dataset, c("Type")))

# 3. GMM procedures to centre, scale, and rotate the coordinates ----
GM_perforation_centered <- Momocs::coo_centre(GM_perforation)
GM_perforation_centered_scaled <- Momocs::coo_scale(GM_perforation_centered)
GM_perforation_centered_scaled <- Momocs::coo_slidedirection(GM_perforation_centered_scaled)

# stack inspections
stack(GM_perforation_centered_scaled)

# 4. Calibrating harmonic power ----
harmonic_power <- calibrate_harmonicpower_efourier(GM_perforation_centered_scaled)

harmonic_power

# 5. EFA analysis ----
GM_perforation_centered_scaled.EFA <- efourier(GM_perforation_centered_scaled, nb.h = 41, smooth.it = 0, norm = T)

# 6. PCA (Principal Component Analysis) ----
GM_perforation_centered_scaled.PCA <- PCA(GM_perforation_centered_scaled.EFA)

# 7. Screeplot ----
GM_screeplot <- Momocs::scree_plot(GM_perforation_centered_scaled.PCA, nax = 1:8) +
  cowplot::theme_minimal_grid() +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        text = element_text(size = 14)) +
  labs(x = "Components", y = "Proportion")

GM_screeplot

ggsave(GM_screeplot,
       filename = "output/figures/Figure_S1.tiff",
       width = 5, height = 5, dpi = 300, units = "in", device = "tif")

# 8. Shape variation plot ----
GM_shape_variation <- Momocs::PCcontrib(GM_perforation_centered_scaled.PCA, nax = 1:3, sd.r = c(-2, -1, 0, 1, 2))

GM_shape_variation$gg

ggsave(GM_shape_variation$gg,
       filename = "output/figures/Figure_shpvar.tif",
       width = 5, height = 5, dpi = 300, units = "in", device = "tif")

ggsave(GM_shape_variation$gg,
       filename = "output/figures/Figure_shpvar.png",
       width = 5, height = 5, dpi = 300, units = "in", device = "png")


#9 Merging datasets to produce figures and run other analyses ####
GM_PCScores <- as.data.frame(GM_perforation_centered_scaled.PCA$x)

GM_PCScores$ID <- dataset$ID

GM_PCScores$ID <- dataset$ID

dataset <- left_join(dataset, GM_PCScores[ ,c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "ID")], by = "ID") 

# 10. Correlation between circumference and principal components ----

cor_circ_pc1 <- cor.test(dataset$Circumference.perforation, dataset$PC1, method = "spearman", exact = FALSE)

cor_circ_pc2 <- cor.test(dataset$Circumference.perforation, dataset$PC2, method = "spearman", exact = FALSE)

cor_circ_pc3 <- cor.test(dataset$Circumference.perforation, dataset$PC3, method = "spearman", exact = FALSE)

#11 Mean shapes  ####

GM_perforation_centered_scaled.EFA %>% MSHAPES() %>% coo_plot() #to display the mean shape of the whole data set

GM_Mean_Shapes_Type <- MSHAPES(GM_perforation_centered_scaled.EFA, ~Type)
Out(GM_Mean_Shapes_Type$shp) %>% panel(names=TRUE, cex.names = 0.5) # to plot this stuff as mean shape per group


mshape_perforation_comparison <- plot_MSHAPES(GM_Mean_Shapes_Type, size = 3/4,
                                              palette=pal_manual(c("orange", "darkblue"))) # Plot the mean shapes

print(mshape_perforation_comparison)

# Open a PDF device to save the plot
pdf("output/figures/Figure_S2.pdf", width = 8, height = 6)  # Adjust width and height as needed

# Create the MSHAPES plot
mshape_perforation_comparison <- plot_MSHAPES(GM_Mean_Shapes_Type, size = 3/4, 
                                              palette = pal_manual(c("orange", "darkblue")))

# Display the plot (it will be rendered to the PDF)
mshape_perforation_comparison

# Close the PDF device to save the plot
dev.off()

# 12. Plotting results of PCA ----

tiff("output/figures/Figure_S3.tiff", width = 8, height = 6, units = "in", res = 300)  # Adjust resolution and dimensions as needed

# Create the PCA plot
Figure_S3 <- plot_PCA(GM_perforation_centered_scaled.PCA, axes = c(1, 2), ~ Type, palette = sea_shell_sand_palette, morphospace = TRUE, points = TRUE, zoom = 0.9, chull = FALSE, legend = TRUE) %>% 
  layer_chullfilled(alpha = 0.8) %>% 
  layer_legend(cex = 2/6)

# Close the TIFF device to save the plot
dev.off()


# 13. PCA with shape and metric data ----

PCA_shell <- dataset %>%
  select(Type, Area.perforation, Circumference.perforation, PC1, PC2, PC3) %>%
  rename(Area = Area.perforation, Circumference = Circumference.perforation, `PC1-Out` = PC1, `PC2-Out` = PC2, `PC3-Out` = PC3) %>%
  na.omit()

PCA_shell_numeric <- PCA_shell[, -1]  # Remove the first column (non-numeric)

set.seed(123)
PCA_shells.morpho.size <- prcomp(PCA_shell_numeric, scale. = TRUE)

summary(PCA_shells.morpho.size)

PCA_shells.morpho.size

# 14. Boxplots metric data ----

circumference <- ggplot(PCA_shell, aes(x = Type , y = log(Circumference), fill = Type)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Create the boxplot and hide default outliers
  geom_jitter(size = 2.5, alpha = 0.5, width = 0.1, height = 0.1) +  # Add jittered points
  labs(y = "Log-Circumference", x = "") +
  scale_fill_manual(values = sea_shell_sand_palette) +  # Apply the custom color palette
  theme_pubclean() +
  theme(legend.position = "",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12)) +
  ggtitle("a")  # Add label "A"


circumference

area <- ggplot(PCA_shell, aes(x = Type , y = log(Area), fill = Type)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Create the boxplot and hide default outliers
  geom_jitter(size = 2.5, alpha = 0.5, width = 0.1, height = 0.1) +  # Add jittered points
  labs(y = "Log-Area", x = "") +
  scale_fill_manual(values = sea_shell_sand_palette) +  # Apply the custom color palette
  theme_pubclean() +
  theme(legend.position = "",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12)) +
  ggtitle("b")  # Add label "B"


area

library(patchwork)
combined_plot <- (circumference | area)  # Use patchwork's pipe operator for side-by-side plots

# Save the combined figure as a high-resolution TIFF
ggsave("output/figures/Figure_S4.tif", combined_plot, width = 10, height = 5, units = "in", dpi = 300)

# 15. Proportion of variance ----

variance <- fviz_eig(PCA_shells.morpho.size, addlabels = T,
                     repel=T,
                     ggtheme=theme(text=element_text(size=14),
                                   plot.title = element_blank(),
                                   axis.title = element_text(size=14),
                                   axis.text = element_text(size=12))) +
  ylim(0, 43) +
  theme_minimal_grid() +
  labs(x = "Principal Components", y = "% Explained Variance") +
  theme(plot.title = element_blank())

variance

# # ggsave(filename="PCA_shells_PC_imp.png", plot=PCA_shells_PC_imp, device="png",
# #        height=4, width=5, units="in", dpi=500)
# 

# 16. Contribution of the quantitative variables to the first two components ----

Figure_AFa <- fviz_pca_var(PCA_shells.morpho.size,
                           col.var = "contrib",
                           axes =c(1,2),
                           labelsize=5,repel = T, ggtheme=theme(text=element_text(size=14),
                                                                axis.title = element_text(size=14),
                                                                axis.text = element_text(size=12),
                                                                legend.position = "bottom")) +
  labs(y = "PC2 (20%)", x = "PC1 (42.2%)", colour = "Contrib") +
  theme_minimal_grid() +
  theme(plot.title = element_blank()) +
  scale_color_gradientn(colors=met.brewer("Isfahan1"))


Figure_AFa

# 17. Plotting PC1 to PC2 visualizing the core type ----

Figure_AFc <- factoextra::fviz_pca_ind(PCA_shells.morpho.size,
                                       axes=c(1, 2), label="none",
                                       addEllipses=T, ellipse.type="confidence",
                                       habillage=PCA_shell$Type, pointsize=2, labelsize=10, repel=T,
                                       ggtheme = theme(text=element_text(size=16),
                                                       axis.title = element_text(size=16),
                                                       axis.text = element_text(size=14)),
                                       legend.title=element_text("")) +
  labs(x="PC1 (42.2%)", y="PC2 (20%)") +
  scale_fill_manual(values=sea_shell_sand_palette) +
  scale_colour_manual(values=sea_shell_sand_palette) +
  theme_minimal_grid() +
  theme(plot.title = element_blank()) +
  theme(plot.title = element_blank(),
        text=element_text(size=16),
        legend.text = element_text(size=16),
        legend.position = "bottom",
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))


Figure_AFc


# 17. Dataset for statistical analysis ----

GM_Size_PCScores <- as.data.frame(PCA_shells.morpho.size$x)

GM_Size_PCScores$ID <- dataset$ID

dataset.2 <- dataset %>%
  select(ID, Type)

dataset.2 <- left_join(dataset.2, GM_Size_PCScores[ ,c("PC1", "PC2", "PC3", "PC4", "PC5", "ID")], by = "ID") 

# 18. PERMANOVA ----

# Create the Y matrix of variables under comparison:
Y.PERMANOVA.size <- dataset.2[, c("PC1", "PC2", "PC3", "PC4")]

# Perform one-way PERMANOVA
PERMANOVA.size <- vegan::adonis2(Y.PERMANOVA.size ~ dataset.2$Type, method = "euclidean", permutations = 10000)

# Pairwise differences:
Pairwise_PERMANOVA.size.type <- pairwiseAdonis::pairwise.adonis(Y.PERMANOVA.size, dataset.2$Type, sim.method = "euclidean", p.adjust.m = "bonferroni", perm = 10000)

Pairwise_PERMANOVA.size.type

# 19. DISPARITY, wilcoxon ----

# Ensure Dataset_2DGM is a data frame or matrix
dataset.2 <- as.data.frame(dataset.2)

# Subset the columns correctly (make sure PC1 to PC4 are the correct column names)
PC.data.subset.size <- dataset.2[, c("PC1", "PC2", "PC3", "PC4")]

# Assign row names based on the 'Type' column in the dataset
rownames_DATASETS.size <- as.factor(dataset.2$Type)

# Disparity test 
data_subsets <- dispRity::custom.subsets(PC.data.subset.size, group = rownames_DATASETS.size)
data_boot <- boot.matrix(data_subsets, bootstraps = 10000)
data_disp <- dispRity(data_boot, metric = c(sum, variances))

# Wilcox.test for pairwise comparisons
pairwise_results.disparity.size <- test.dispRity(data_disp, test = wilcox.test, comparisons = "pairwise", correction = "bonferroni")

# Format and display results
disparity.size.formatted_results <- as.data.frame(pairwise_results.disparity.size)
disparity.size.formatted_results <- disparity.size.formatted_results %>%
  mutate(p.value = format.pval(p.value, digits = 3))

# 19. DISPARITY, plot ----
data_names <- names(data_disp$disparity)
disparity_df_list <- list()
for(i in data_names){
  disparity_df_list[[i]] <- data.frame(Context = paste0(i, 
                                                        "\n(n=", nrow(data_disp$subsets[[i]]$elements), ")"),
                                       disparity = as.vector(data_disp$disparity[[i]][[2]]),
                                       nelements = nrow(data_disp$subsets[[i]]$elements),
                                       TS = i)
}
disparity_df_discrete_PCA <- do.call(rbind.data.frame, disparity_df_list)

Figure_AFb <- 
  disparity_df_discrete_PCA %>%
  ggplot(aes(x = Context, y = disparity)) +
  geom_violin(aes(fill = TS)) +
  geom_boxplot(notch = TRUE, width = 0.1, fill = "white", color = "black") +
  theme_bw() +
  ggtitle(NULL) +
  xlab("") + 
  ylab("Disparity") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 0),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_manual(values = sea_shell_sand_palette) +  
  guides(color = FALSE, fill = FALSE)

Figure_AFb

# 20. Combine the plots ----

library(patchwork)
Figure_AF <- (Figure_AFa + Figure_AFb) / Figure_AFc + 
  plot_annotation(
    tag_levels = 'a'
  )

# Save the combined figure as a high-resolution TIFF
ggsave("output/figures/Figure_AF.tiff", Figure_AF, width = 10, height = 8, units = "in", dpi = 300)

# Save the combined figure as a high-resolution TIFF
ggsave("output/figures/Figure_AF.png", Figure_AF, width = 10, height = 8, units = "in", dpi = 300)
