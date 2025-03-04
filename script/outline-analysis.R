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

dataset <- read_excel("data/dataset.xlsx")

# Assuming the "ID" column contains the IDs you want to use
ids_to_keep <- dataset$ID

set.seed(123)  # Set seed for reproducibility

# 2. Reading file names
lf <- list.files("data/outlines", pattern = "\\.txt$", full.names = TRUE)

# 3. Keep only the files that match IDs from the dataset
lf2 <- lf[basename(lf) %in% paste0(ids_to_keep, ".txt")]

# Output the remaining files after filtering
print(lf2)

# # List of files to be removed (with prefix EXP_)
# files_to_remove <- c("EXP_63", "EXP_82", "EXP_87", "EXP_133", "EXP_138", "EXP_172", "EXP_183")
# 
# # 1. Reading file names ----
# lf <- list.files("data/outlines", pattern = "\\.txt$", full.names = TRUE)
# 
# # Remove the files that are in the 'files_to_remove' list
# lf2 <- lf[!basename(lf) %in% paste0(files_to_remove, ".txt")]
# 
# # Output the remaining files after removal
# print(lf2)

# # Exclude specific IDs
# ids_to_exclude <- c("RB_231", "RB_446", "RB_451", "RB_504", "RB_825") 
# Dataset_2DGM <- Dataset_2DGM %>%
#   filter(!ID %in% ids_to_exclude)

# 3. Extracting coordinates and attaching IDs ----
Coordinates <- import_txt(lf2)
# Coordinates_perforation <- dataset %>%
#   pull("ID") %>%
#   as.character()

# ids_to_remove <- c("EXP_63", "EXP_82", "EXP_87", "EXP_133", "EXP_138", "EXP_172", "EXP_183")

# Filter out the rows with IDs to be removed
dataset.filtered <- dataset
  # filter(!(ID %in% ids_to_remove))

# Pull the IDs from the dataset and convert them to character
# Coordinates_perforation <- dataset %>%
#   pull("ID") %>%
#   as.character()

Coordinates_perforation <- dataset.filtered %>%
  pull("ID") %>%
  as.character()


Coordinates_2DGM <- Coordinates[Coordinates_perforation]
GM_perforation <- Out(Coordinates_2DGM, fac = select(dataset.filtered, c("Type", "Perforation")))

# 4. GMM procedures to centre, scale, and rotate the coordinates ----
GM_perforation_centered <- Momocs::coo_centre(GM_perforation)
GM_perforation_centered_scaled <- Momocs::coo_scale(GM_perforation_centered)
GM_perforation_centered_scaled <- Momocs::coo_slidedirection(GM_perforation_centered_scaled)
# GM_perforation_centered_scaled_rotated <- Momocs::coo_rotatecenter(GM_perforation_centered_scaled, theta = -pi/2)


# GM_perforation_centered_scaled <- Momocs::coo_slidedirection(GM_perforation_centered, direction = "right")

# stack inspections

stack(GM_perforation_centered_scaled)

# 5. Calibrating harmonic power ----
harmonic_power <- calibrate_harmonicpower_efourier(GM_perforation_centered_scaled)

harmonic_power

# 6. EFA analysis ----
GM_perforation_centered_scaled.EFA <- efourier(GM_perforation_centered_scaled, nb.h = 41, smooth.it = 0, norm = T)

# 7. PCA (Principal Component Analysis) ----
GM_perforation_centered_scaled.PCA <- PCA(GM_perforation_centered_scaled.EFA)

# lda_result <- LDA(GM_perforation_centered_scaled.PCA, fac = select(dataset.filtered, c("Type")))
# 
# plot_LDA(lda_result)

# 8. Screeplot ----
GM_screeplot <- Momocs::scree_plot(GM_perforation_centered_scaled.PCA, nax = 1:8) +
  cowplot::theme_minimal_grid() +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        text = element_text(size = 14)) +
  labs(x = "Components", y = "Proportion")

GM_screeplot

# Save screeplot
ggsave(GM_bladelets_screeplot,
       filename = "output/figures/Figure_S7.png",
       width = 4, height = 4, dpi = 300, units = "in", device = "png")

# 9. Shape variation plot ----
GM_shape_variation <- Momocs::PCcontrib(GM_perforation_centered_scaled.PCA, nax = 1:3, sd.r = c(-2, -1, 0, 1, 2))

GM_shape_variation

# Save shape variation plot
ggsave(GM_bladelets_shape_variation_flipped,
       filename = "output/figures/Figure_S8.png",
       width = 5, height = 5, dpi = 300, units = "in", device = "png")

#10 Merging datasets to produce figures and run other analyses ####
GM_PCScores <- as.data.frame(GM_perforation_centered_scaled.PCA$x)

GM_PCScores$ID <- dataset.filtered$ID

GM_PCScores$ID <- dataset.filtered$ID

dataset.filtered <- left_join(dataset.filtered, GM_PCScores[ ,c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "ID")], by = "ID") 

# 11. Correlation between measurements and principal components ----

# Calculate Spearman correlation for Length and PC1
cor_area_pc1 <- cor.test(dataset.filtered$`Circumference perf.`, dataset.filtered$PC1, method = "spearman", exact = FALSE)

cor_area_pc2 <- cor.test(dataset.filtered$`Circumference perf.`, dataset.filtered$PC2, method = "spearman", exact = FALSE)

cor_area_pc3 <- cor.test(dataset.filtered$`Circumference perf.`, dataset.filtered$PC3, method = "spearman", exact = FALSE)


# # Create a data frame to store correlation results
# correlation_results <- data.frame(
#   Variable1 = c("Length", "Width", "Thickness", "Length", "Width", "Thickness", "Length", "Width", "Thickness"),
#   Variable2 = rep(c("PC1", "PC2", "PC3"), each = 3),
#   Spearman_Correlation = c(cor_length_pc1$estimate, cor_width_pc1$estimate, cor_thickness_pc1$estimate,
#                            cor_length_pc2$estimate, cor_width_pc2$estimate, cor_thickness_pc2$estimate,
#                            cor_length_pc3$estimate, cor_width_pc3$estimate, cor_thickness_pc3$estimate),
#   p_value = c(cor_length_pc1$p.value, cor_width_pc1$p.value, cor_thickness_pc1$p.value,
#               cor_length_pc2$p.value, cor_width_pc2$p.value, cor_thickness_pc2$p.value,
#               cor_length_pc3$p.value, cor_width_pc3$p.value, cor_thickness_pc3$p.value)
# )
# 
# # Optional: Round p-values for better readability
# correlation_results$p_value <- round(correlation_results$p_value, 3)
# 
# # Print the table
# print(correlation_results)
# 
# # Clean the column names
# # correlation_results <- clean_names(correlation_results)
# 
# colnames(correlation_results) <- c("Measurement", "Principal Component", "Spearman", "p-value")
# 
# # Save the correlation results as an Rdata file
# save(correlation_results, file = "data/correlation_results_table.Rdata")

# 12. Plotting results of PCA ----

Means_PC1_PC3.Type <- dataset.filtered %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class
  group_by(Type) %>% 
  dplyr::summarise(PC1 = mean(PC1),
                   PC2 = mean(PC2),
                   PC3 = mean(PC3))

Means_PC1_PC3.Perforation <- dataset.filtered %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class
  group_by(Perforation) %>% 
  dplyr::summarise(PC1 = mean(PC1),
                   PC2 = mean(PC2),
                   PC3 = mean(PC3))

PC1toPC2.Type <- ggplot(data = dataset.filtered, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 2.5, alpha = 0.5) +
  labs(y= "PC2 (14% of total variance)", x = "PC1 (62% of total variance)") +
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
  geom_point(data = Means_PC1_PC3.Type,
             aes(x = PC1, y = PC2),
             size = 4.5, shape = 16) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_pubclean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14, hjust = -0.2),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12))

PC1toPC2.Type


library(ggplot2)

# Define the IDs you want to highlight
highlight_ids <- c("ARCH_83", "ARCH_53", "ARCH_52", "ARCH_13")

# Highlighted data subset
highlighted_data <- dataset.filtered[dataset.filtered$ID %in% highlight_ids, ]

# Your existing plot with the highlighted points and names
PC1toPC2.Type <- ggplot(data = dataset.filtered, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 2.5, alpha = 0.5) +
  labs(y = "PC2 (14% of total variance)", x = "PC1 (62% of total variance)") +
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
  geom_point(data = Means_PC1_PC3.Type,
             aes(x = PC1, y = PC2),
             size = 4.5, shape = 16) +
  geom_point(data = highlighted_data, aes(x = PC1, y = PC2), 
             color = "red", size = 4, shape = 17) +  # Customize color and shape here
  # geom_text(data = highlighted_data, aes(x = PC1, y = PC2, label = ID), 
  #           color = "black", size = 4, vjust = -1.5) +  # Display names (IDs) above the points
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_pubclean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14, hjust = -0.2),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12))

# Display the plot
print(PC1toPC2.Type)



PC1toPC2.Perf <- ggplot(data = dataset.filtered, aes(x = PC1, y = PC2, color = Perforation)) +
  geom_point(size = 2.5, alpha = 0.5) +
  labs(y= "PC2 (14% of total variance)", x = "PC1 (62% of total variance)") +
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
  geom_point(data = Means_PC1_PC3.Perforation,
             aes(x = PC1, y = PC2),
             size = 4.5, shape = 16) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_pubclean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14, hjust = -0.2),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12))

PC1toPC2.Perf


PC1_Perf <- ggplot(data = dataset.filtered, aes(x = Perforation, y = PC1, color = Perforation)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Create the boxplot and hide default outliers
  geom_jitter(size = 2.5, alpha = 0.5, width = 0.1, height = 0.1) +  # Add jittered points
  labs(y = "PC1 (62% of total variance)", x = "Perforation") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +
  theme_pubclean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12))

PC1_Perf


PC1_Type <- ggplot(data = dataset.filtered, aes(x = Type, y = PC2, color = Type)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Create the boxplot and hide default outliers
  geom_jitter(size = 2.5, alpha = 0.5, width = 0.1, height = 0.1) +  # Add jittered points
  labs(y = "PC1 (62% of total variance)", x = "Perforation") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +
  theme_pubclean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12))

PC1_Type



library(ggplot2)
library(dplyr)

# Assuming dataset.filtered is already created from previous steps

# Confidence Ellipse (95% confidence interval)
PC1toPC2.Type.Ellipse <- ggplot(data = dataset.filtered, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 2.5, alpha = 0.5) +
  labs(y= "PC2 (14% of total variance)", x = "PC1 (62% of total variance)") +
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
  geom_point(data = Means_PC1_PC3.Type,
             aes(x = PC1, y = PC2),
             size = 4.5, shape = 16) +
  stat_ellipse(level = 0.95) +  # Add confidence ellipse (95%)
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_pubclean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14, hjust = -0.2),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12))


hull <- dataset.filtered %>% group_by(Type) %>% 
  slice(chull(PC1, PC2))

PC1toPC2.Type.Hull <- ggplot(data = dataset.filtered, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 2.5, alpha = 0.5) +
  labs(y= "PC2 (14% of total variance)", x = "PC1 (62% of total variance)") +
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
  geom_point(data = Means_PC1_PC3.Type,
             aes(x = PC1, y = PC2),
             size = 4.5, shape = 16) +
  geom_polygon(data = hull, alpha = 0.2, 
               aes(fill = Type,colour = Type)) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_pubclean() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size = 14, hjust = -0.2),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size=12))


# Print the plots
print(PC1toPC2.Type.Ellipse)
print(PC1toPC2.Type.Hull)


# Principal Component Analysis
library(ggsci)

plot.momocs <-  plot_PCA(GM_perforation_centered_scaled.PCA, axes = c(1, 2), ~ Type, morphospace = TRUE, points = TRUE, zoom = 0.9, chull = FALSE,  palette = pal_jama(), legend = TRUE) %>% layer_chullfilled(alpha = 0.8) %>% layer_legend(cex = 2/6) # Plot the PCA


plot.momocs <-  plot_PCA(GM_perforation_centered_scaled.PCA, axes = c(1, 2), ~ Perforation, morphospace = TRUE, points = TRUE, zoom = 0.9, chull = FALSE,  palette = pal_jama(), legend = TRUE) %>% layer_chullfilled(alpha = 0.8) %>% layer_legend(cex = 2/6) # Plot the PCA

plot.momocs <-  plot_PCA(GM_perforation_centered_scaled.PCA, axes = c(1, 2), ~ Perforation, morphospace = TRUE, points = TRUE, zoom = 0.9, chull = FALSE, legend = T) %>% layer_chullfilled(alpha = 0.8) %>% layer_legend(cex = 2/6) # Plot the PCA


# Open a PDF device to save the plot
pdf("pca_plot.pdf", width = 8, height = 6)  # You can adjust width and height as needed

# Create the PCA plot
plot.momocs <- plot_PCA(GM_perforation_centered_scaled.PCA, axes = c(1, 2), ~ Type, morphospace = TRUE, 
                        points = TRUE, zoom = 0.9, chull = FALSE, palette = pal_jama(), 
                        legend = TRUE) %>% 
  layer_chullfilled(alpha = 0.8) %>% 
  layer_legend(cex = 2/6)

# Close the PDF device to save the plot
dev.off()




#6 Mean shapes  ####

GM_perforation_centered_scaled.EFA %>% MSHAPES() %>% coo_plot() #to display the mean shape of the whole data set

GM_Mean_Shapes_Type <- MSHAPES(GM_perforation_centered_scaled.EFA, ~Type)
Out(GM_Mean_Shapes_Type$shp) %>% panel(names=TRUE, cex.names = 0.5) # to plot this stuff as mean shape per group


mshape_perforation_comparison <- plot_MSHAPES(GM_Mean_Shapes_Type, size = 3/4,
                                                 palette=pal_manual(c("orange", "darkblue"))) # Plot the mean shapes

mshape_perforation_comparison

print(mshape_perforation_comparison)


# Open a PDF device to save the plot
pdf("mshape_perforation_comparison.pdf", width = 8, height = 6)  # Adjust width and height as needed

# Create the MSHAPES plot
mshape_perforation_comparison <- plot_MSHAPES(GM_Mean_Shapes_Type, size = 3/4, 
                                              palette = pal_manual(c("orange", "darkblue")))

# Display the plot (it will be rendered to the PDF)
mshape_perforation_comparison

# Close the PDF device to save the plot
dev.off()




# tiff("Figures/2DGM/Mean_shapes_separated.tiff", width = 1824, height = 1824, res = 300) #to save plots
# Out(GM_Class_Mean_Shapes$shp) %>% panel(names=TRUE)
# dev.off() # export the figure

# Two-way comparison plotting them in a matrix, figure further modified in Adobe Illustrator to improve its quality. The raw file is provided in the figures folder
# Set up the output file for saving the plot as a TIFF image
tiff("output/figures/Mean_shapes_raw.png", 
     units = "in", 
     width = 16,    # Increase width (adjust as necessary)
     height = 16,   # Increase height (adjust as necessary)
     res = 600)

# Generate the plot, adjust size and palette as desired
plot_MSHAPES(GM_Mean_Shapes_Type, size = 0.9, palette = pal_manual(c("darkblue", "red")))

# Close the device to save the file
dev.off()


# Save the figure with png()
png("output/figures/Mean_shapes_raw.png", 
    units = "in", 
    width = 18,    # Increase width
    height = 18,   # Increase height
    res = 300)

# Adjust margins before plotting
par(mar = c(6, 6, 3, 3))  # Increase bottom and left margins

# Generate the plot
plot_MSHAPES(GM_Mean_Shapes_Type, size = 0.9, palette = pal_manual(c("darkblue", "red")))

# Close the device to save the image
dev.off()





# plot_MSHAPES(GM_Class_Mean_Shapes)
# # to save it:
# tiff("figures/2DGM/Mean_shapes_compared.reduced.tiff", units = "in", width = 3, height =3, res = 200)
# plot_MSHAPES(GM_Class_Mean_Shapes, size= 0.5, palette=pal_manual(c("darkblue", "red")))
# dev.off()







# PC1toPC2.provenience <- Dataset_2DGM %>%
#   mutate(raw.material.lumped = fct_relevel(raw.material.lumped, "Local", "Circum.local", "Distant", "Very.distant")) %>%
#   mutate(raw.material.lumped = recode(raw.material.lumped, Circum.local = "Circum-local", Very.distant = "Very distant")) %>%
#   ggplot(aes(x = PC1, y = PC2, color = raw.material.lumped)) +
#   geom_point(size = 1.5, alpha = 0.5) +
#   labs(y= "PC2 (14% of total variance)", x = "PC1 (62% of total variance)") +
#   geom_hline(yintercept = 0, 
#              linetype = "dashed", 
#              color = "black",
#              size = 1, 
#              alpha = 0.5) +
#   geom_vline(xintercept = 0, 
#              linetype = "dashed", 
#              color = "black",
#              size = 1, 
#              alpha = 0.5)  +
#   geom_point(data = Means_PC1_PC3.source,
#              aes(x = PC1, y = PC2),
#              size = 4.5, shape = 16) +
#   guides(colour = guide_legend(override.aes = list(size=5), ncol = 2)) +
#   theme_pubclean() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         legend.text = element_text(size=14),
#         axis.title.x = element_text(size = 14, hjust = -0.2),
#         axis.title.y = element_text(size = 14),
#         axis.text = element_text(size=12)) +
#   scale_color_manual(labels=c("Local (n = 121)", "Circum-local (n = 44)", "Distant (n = 95)", "Very distant (n = 109)"), 
#                      values=zapotec_palette)
# 
# 
# # Combine the plots into a single figure
# PC1toPC2.plots.combined <- ggarrange(PC1toPC2.layer, PC1toPC2.provenience,
#                                      labels = c("a", "b"),
#                                      nrow = 1,
#                                      widths = c(1, 1),
#                                      legend = "bottom",
#                                      align = "h") + 
#   theme(
#     text = element_text(size = 18),
#     axis.text = element_text(size = 16),
#     axis.title = element_text(size = 18),
#     legend.text = element_text(size = 16),
#     legend.title = element_text(size = 18),
#     plot.title = element_text(size = 20)
#   )
# 
# 
# ggsave("output/figures/Figure_7.tiff", PC1toPC2.plots.combined, width = 11.7, height = 6.3, units = "in")


# 13. PERMANOVA ----
min_n_PCs.2DGM <- Momocs::scree_min(GM_perforation_centered_scaled.PCA, prop = 0.95) 

min_n_PCs.2DGM

# Create the Y matrix of variables under comparison:
Y.PERMANOVA <- dataset.filtered[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")]

# Perform one-way PERMANOVA
GM_PERMANOVA.type <- vegan::adonis2(Y.PERMANOVA ~ dataset.filtered$Type, method = "euclidean", permutations = 10000)

GM_PERMANOVA.perforation <- vegan::adonis2(Y.PERMANOVA ~ dataset.filtered$Perforation, method = "euclidean", permutations = 10000)

# GM_PERMANOVA.rawmatprov <- vegan::adonis2(Y.PERMANOVA ~ Dataset_2DGM$raw.material.provenience, method = "euclidean", permutations = 10000)

# Pairwise differences:
Pairwise_PERMANOVA.type <- pairwiseAdonis::pairwise.adonis(Y.PERMANOVA, dataset.filtered$Type, sim.method = "euclidean", p.adjust.m = "bonferroni", perm = 10000)

Pairwise_PERMANOVA.type

Pairwise_PERMANOVA.perf <- pairwiseAdonis::pairwise.adonis(Y.PERMANOVA, dataset.filtered$Perforation, sim.method = "euclidean", p.adjust.m = "bonferroni", perm = 10000)

# 14. Disparity Test ----
# dataset.filtered$Layer_RawMat <- paste(Dataset_2DGM$Layer, Dataset_2DGM$raw.material.lumped, sep = ", ")
# data$Layer_RawMat <- dplyr::recode(Dataset_2DGM$Layer_RawMat, "A1, Local" = "A1, Local", "A2, Local" = "A2, Local", "A1, Circum.local" = "A1, Non-local", "A2, Circum.local" = "A2, Non-local", "A1, Distant" = "A1, Non-local", "A2, Distant" = "A2, Non-local", "A1, Very.distant" = "A1, Non-local", "A2, Very.distant" = "A2, Non-local") %>%
#   fct_relevel("A1, Local", "A2, Local", "A1, Non-local", "A2, Non-local")

# Ensure Dataset_2DGM is a data frame or matrix
dataset.filtered <- as.data.frame(dataset.filtered)

# Subset the columns correctly (make sure PC1 to PC9 are the correct column names)
PC.data.subset.GM <- dataset.filtered[, c("PC1", "PC2", "PC3", "PC4", "PC5")]

rownames_DATASETS <- as.factor(dataset.filtered$Type)

# Disparity test 
data_subsets <- dispRity::custom.subsets(PC.data.subset.GM, group = rownames_DATASETS)
data_boot <- boot.matrix(data_subsets, bootstraps = 10000)
data_disp <- dispRity(data_boot, metric = c(sum, variances))

# Wilcox.test
pairwise_results.disparity.shape <- test.dispRity(data_disp, test = wilcox.test, comparisons = "pairwise", correction = "bonferroni")

# Format and display results
disparity.shape.formatted_results <- as.data.frame(pairwise_results.disparity.shape)
disparity.shape.formatted_results <- disparity.shape.formatted_results %>%
  mutate(p.value = format.pval(p.value, digits = 3))

# Disparity Plot
data_names <- names(data_disp$disparity)
disparity_df_list <- list()
for(i in data_names){
  disparity_df_list[[i]] <- data.frame(Context = paste0(i, 
                                                        "\n(n=",nrow(data_disp$subsets[[i]]$elements),")"),
                                       disparity = as.vector(data_disp$disparity[[i]][[2]]),
                                       nelements = nrow(data_disp$subsets[[i]]$elements),
                                       TS = i)
}
disparity_df_discrete_GM <- do.call(rbind.data.frame, disparity_df_list)

# Plot with the updated categories (Local vs Non-local)
disparity_df_discrete_GM.plot <- 
  disparity_df_discrete_GM %>%
  ggplot(aes(x = Context, y = disparity)) +
  geom_violin(aes(fill = TS)) + 
  geom_boxplot(notch = TRUE, width = 0.1, fill = "white", color = "black") +
  theme_bw() +
  ggtitle(NULL) +
  xlab("") + 
  ylab("Disparity") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5, size = 16),
        axis.title.y = element_text(vjust = 0),
        axis.title = element_text(size = 18)) +
  # scale_fill_manual(values = c("A1, Local" = "#C75C4A",  # Second Zapotec color for Local
  #                              "A2, Local" = "#C75C4A",  # Second Zapotec color for Local
  #                              "A1, Non-local" = "#A58D65",  # Third Zapotec color for Non-local
  #                              "A2, Non-local" = "#A58D65"   # Third Zapotec color for Non-local
  # )) +
  guides(color = FALSE, fill = FALSE)


# Display the plot
disparity_df_discrete_GM.plot

# Save the disparity test plot
ggsave("output/figures/Figure_8.tiff", disparity_df_discrete_GM.plot, width = 6, height = 4.5, units = "in")
