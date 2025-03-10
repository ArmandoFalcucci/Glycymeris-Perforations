####--------Packages needed--------------####

library(FactoMineR)
library(factoextra)
library(tidyverse)
library(cowplot)
library(MetBrewer)
library(ggpubr)

Dataset <- read.csv("data/CTC_Dataset_complete.csv")
Dataset_shells <- read.csv("data/CTC_Dataset_shells.csv")




####--------PCA of blade and bladelet shells--------------####

#1 Create the dataset ####
PCA_shell <- dataset.filtered %>%
  # filter(Laminar_y_n == "TRUE",
  #        Core_classification_2 != "Shatter" & Core_classification_2 != "Initial",
  #        Layer != "ars"
  #        ) %>%
  select(Type, `Area perforation m2`, `Circumference perf.`, PC1, PC2, PC3) %>%
  rename(`Area` = `Area perforation m2`, Circumference = `Circumference perf.`, `PC1-Out` = PC1, `PC2-Out` = PC2, `PC3-Out` = PC3) %>%
  na.omit()

# PCA_shell <- dataset.filtered %>%
#   # filter(Laminar_y_n == "TRUE",
#   #        Core_classification_2 != "Shatter" & Core_classification_2 != "Initial",
#   #        Layer != "ars"
#   #        ) %>%
#   select(Type, `Area perforation m2`, PC1, PC2, PC3) %>%
#   rename(`Area` = `Area perforation m2`) %>%
#   na.omit()


#2 Correlation between the quantitative variables selected for analysis ####
cor_matrix_core <- cor(PCA_shell[,2:6]) 
cor_matrix_core

## if 3 does not work, do this: 
PCA_shell_numeric <- PCA_shell[, -1]  # Remove the first column (non-numeric)

PCA_shells.morpho.size <- prcomp(PCA_shell_numeric, scale. = TRUE)

# Summary of PCA to see variance explained by each principal component
summary(PCA_shells.morpho.size)

PCA_shells.morpho.size


# Visualizing the Contribution Circle with Arrows
# In this case, we are plotting the first two principal components (PC1 and PC2)
fviz_pca_var(PCA_shells.morpho.size,
             axes = c(1, 2),       # First two principal components (PC1, PC2)
             col.var = "black",    # Color for variable arrows
             gradient.cols = c("blue", "red"), # Color gradient for contribution
             geom = "arrow",       # Use arrows to represent variable contributions
             repel = TRUE)         # Repel to avoid label overlap

# You can also adjust parameters like axis numbers, colors, etc., to customize your plot.




# #3 PCA in the FactoMineR package ####
# PCA_shells.morpho.size <- FactoMineR::PCA(PCA_shell, quali.sup=c(1), scale.unit = T, graph = F)
# 
# PCA_shells.morpho.size$eig # Eigenvalues

# #4 Results of the PCA ####
# dimdesc_PCA_shells <- dimdesc(PCA_shells.morpho.size, axes=c(1:3))
# print(dimdesc_PCA_shells)
# 
# #5 First visualization plot ####
# windows()
# plot.PCA(PCA_shells.morpho.size)
# 
#6 Importance of the diffeent components ####

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

#7 Contribution of the quantitative variables to the first two components ####

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


fviz_pca_var(PCA_shells.morpho.size,
             col.var = "contrib",
             axes =c(1,3),
             labelsize=5,repel = T, ggtheme=theme(text=element_text(size=14),
                                                  axis.title = element_text(size=14),
                                                  axis.text = element_text(size=12),
                                                  legend.position = "bottom")) +
  # labs(y = "PC2 (21.6%)", x = "PC1 (47.2%)", colour = "Contrib") +
  theme_minimal_grid() +
  theme(plot.title = element_blank()) +
  scale_color_gradientn(colors=met.brewer("Isfahan1"))


# ggsave(filename="PCA_shells_PCs_expl.png", plot=PCA_shells_PCs_expl, device="png",
#        height=4, width=5, units="in", dpi=500)

#8 Plotting PC1 to PC2 visualizing the core type ####

Figure_AFc <- factoextra::fviz_pca_ind(PCA_shells.morpho.size,
             axes=c(1, 2), label="none",
             addEllipses=T, ellipse.type="confidence",
             habillage=PCA_shell$Type, pointsize=2, labelsize=10, repel=T,
             ggtheme = theme(text=element_text(size=16),
                             axis.title = element_text(size=16),
                             axis.text = element_text(size=14)),
             legend.title=element_text("")) +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  # scale_fill_manual(values=met.brewer("Lakota", 3)) +
  # scale_color_manual(values=met.brewer("Lakota",3)) +
  # scale_color_met_d("Veronese") +
  # scale_fill_met_d("Veronese") +
  labs(x="PC1 (42.2%)", y="PC2 (20%)") +
  scale_fill_manual(values=sea_shell_sand_palette) +
  scale_colour_manual(values=sea_shell_sand_palette) +
  theme_minimal_grid() +
  # ggthemes::theme_clean() +
  theme(plot.title = element_blank()) +
  theme(plot.title = element_blank(),
        text=element_text(size=14),
        legend.position = "bottom",  # Position legend at the bottom
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))


Figure_AFc


# Define the IDs you want to highlight
highlight_ids <- c("ARCH_83", "ARCH_53", "ARCH_52", "ARCH_13")

# Highlighted data subset
highlighted_data_2 <- dataset.filtered_2[dataset.filtered_2$ID %in% highlight_ids, ]



PCA1.3a <- factoextra::fviz_pca_ind(PCA_shells.morpho.size,
                                    axes=c(1, 3), label="none",
                                    addEllipses=T, ellipse.type="confidence",
                                    habillage=PCA_shell$Type, pointsize=2, labelsize=10, repel=T,
                                    ggtheme = theme(text=element_text(size=16),
                                                    axis.title = element_text(size=16),
                                                    axis.text = element_text(size=14)),
                                    legend.title=element_text("Type")) +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  # scale_fill_manual(values=met.brewer("Lakota", 3)) +
  # scale_color_manual(values=met.brewer("Lakota",3)) +
  # scale_color_met_d("Veronese") +
  # scale_fill_met_d("Veronese") +
  labs(x="PC1", y="PC3") +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  theme_minimal_grid() +
  # ggthemes::theme_clean() +
  theme(plot.title = element_blank()) +
  theme(plot.title = element_blank(),
        text=element_text(size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))


PCA1.3a


#9 Plotting PC1 to PC2 visualizing the unit of provenience ####

# PCA1.2b <- fviz_pca_ind(PCA_core,
#              axes=c(1, 2), label="none",
#              addEllipses=T, ellipse.type="confidence",
#              habillage=c(1), pointsize=3, labelsize=10, repel=T,
#              ggtheme = theme(text=element_text(size=16),
#                              axis.title = element_text(size=16),
#                              axis.text = element_text(size=14)),
#              legend.title=element_text("Layer")) +
#   # scale_fill_manual(values=cbPalette) +
#   # scale_colour_manual(values=cbPalette) +
#   scale_fill_manual(values=met.brewer("Navajo", 2)) +
#   scale_color_manual(values=met.brewer("Navajo",2)) +
#   labs(x="PC1 (47.2%)", y="PC2 (21.6%)") +
#   # scale_fill_manual(values=cbPalette) +
#   # scale_colour_manual(values=cbPalette) +
#   theme_minimal_grid() +
#   # ggthemes::theme_clean() +
#   theme(plot.title = element_blank(),
#         text=element_text(size=14),
#         axis.title = element_text(size=14),
#         axis.text = element_text(size=14))
#   
#10 Merging and saving the figures for publication ####

ggsave(filename = "output/figures/Figure_6.tiff", width = 8, height = 10, units = "in", bg = "white", dpi=300, plot=(
  ggdraw() +
    draw_plot(PCA_shells_PC_imp, x = 0, y = 0.7, width = .5, height = .30) +
    draw_plot(PCA_shells_PCs_expl, x = .5, y = 0.7, width = .5, height = .30) +
    draw_plot(PCA1.2b, x = 0, y = 0.35, width = 0.84, height = .35)  +
    draw_plot(PCA1.2a, x=0, y=0, width=1, height= .35)+
    draw_plot_label(label = c("a", "b", "c", "d"),
                    x = c(0, .5, 0,0), y = c(1, 1, .7, .35), size=14)
))


#11 Contribution of the quantitative variables to the first and third components ####

PCA_shells_PCs1.3_expl <- fviz_pca_var(PCA_shell,
                                   col.var = "contrib",
                                   axes =c(1,3),
                                   labelsize=5,repel = T, ggtheme=theme(text=element_text(size=14),
                                                                        axis.title = element_text(size=14),
                                                                        axis.text = element_text(size=12),
                                                                        legend.position = "bottom")) +
  labs(y = "PC3 (19.7%)", x = "PC1 (47.2%)", colour = "Contrib") +
  theme_minimal_grid() +
  theme(plot.title = element_blank()) +
  scale_color_gradientn(colors=met.brewer("Isfahan1")) #Change name according to the SI file


PCA_shells_PCs1.3_expl

# ggsave(filename="PCA_shells_PCs_expl.png", plot=PCA_shells_PCs_expl, device="png",
#        height=4, width=5, units="in", dpi=500)

#8 Plotting PC1 to PC2 visualizing the core type ####

PCA1.3a <- factoextra::fviz_pca_ind(PCA_core,
                                    axes=c(1, 3), label="none",
                                    addEllipses=T, ellipse.type="confidence",
                                    habillage=c(2), pointsize=3, labelsize=10, repel=T,
                                    ggtheme = theme(text=element_text(size=16),
                                                    axis.title = element_text(size=16),
                                                    axis.text = element_text(size=14)),
                                    legend.title=element_text("Core type")) +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  scale_fill_manual(values=met.brewer("Lakota", 5)) +
  scale_color_manual(values=met.brewer("Lakota",5)) +
  # scale_color_met_d("Veronese") +
  # scale_fill_met_d("Veronese") +
  labs(x="PC1 (47.2%)", y="PC3 (19.7%)") +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  theme_minimal_grid() +
  # ggthemes::theme_clean() +
  theme(plot.title = element_blank()) +
  theme(plot.title = element_blank(),
        text=element_text(size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))

#9 Plotting PC1 to PC2 visualizing the unit of provenience ####

PCA1.3b <- fviz_pca_ind(PCA_core,
                        axes=c(1, 3), label="none",
                        addEllipses=T, ellipse.type="confidence",
                        habillage=c(1), pointsize=3, labelsize=10, repel=T,
                        ggtheme = theme(text=element_text(size=16),
                                        axis.title = element_text(size=16),
                                        axis.text = element_text(size=14)),
                        legend.title=element_text("Unit")) +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  scale_fill_manual(values=met.brewer("Navajo", 2)) +
  scale_color_manual(values=met.brewer("Navajo",2)) +
  labs(x="PC1 (47.2%)", y="PC3 (19.7%)") +
  # scale_fill_manual(values=cbPalette) +
  # scale_colour_manual(values=cbPalette) +
  theme_minimal_grid() +
  # ggthemes::theme_clean() +
  theme(plot.title = element_blank(),
        text=element_text(size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))

#10 Merging and saving the figures for publication ####

ggsave(filename = "output/figures/Figure_S6.png", width = 8, height = 10, units = "in",dpi=300, plot=(
  ggdraw() +
    draw_plot(PCA_shells_PCs1.3_expl, x = 0, y = 0.7, width = 1, height = .30) +
    draw_plot(PCA1.3b, x = 0, y = 0.35, width = 0.84, height = .35)  +
    draw_plot(PCA1.3a, x=0, y=0, width=1, height= .35)+
    draw_plot_label(label = c("A", "B", "C"),
                    x = c(0.2, 0,0), y = c(1, .7, .35), size=14)
))

## NEW ----

#10 Merging datasets to produce figures and run other analyses ####
GM_Size_PCScores <- as.data.frame(PCA_shells.morpho.size$x)

GM_Size_PCScores$ID <- dataset.filtered$ID

# GM_PCScores$ID <- dataset.filtered$ID

dataset.filtered_2 <- dataset.filtered %>%
  select(ID, Type)

dataset.filtered_2 <- left_join(dataset.filtered_2, GM_Size_PCScores[ ,c("PC1", "PC2", "PC3", "PC4", "PC5", "ID")], by = "ID") 

## PERMANOVA ----

# 13. PERMANOVA ----
# min_n_PCs.size <- Momocs::scree_min(PCA_shells.morpho.size, prop = 0.95) 
# 
# min_n_PCs.2DGM

# Create the Y matrix of variables under comparison:
Y.PERMANOVA.size <- dataset.filtered_2[, c("PC1", "PC2", "PC3", "PC4")]

# Perform one-way PERMANOVA
PERMANOVA.size <- vegan::adonis2(Y.PERMANOVA.size ~ dataset.filtered_2$Type, method = "euclidean", permutations = 10000)

# GM_PERMANOVA.rawmatprov <- vegan::adonis2(Y.PERMANOVA ~ Dataset_2DGM$raw.material.provenience, method = "euclidean", permutations = 10000)

# Pairwise differences:
Pairwise_PERMANOVA.size.type <- pairwiseAdonis::pairwise.adonis(Y.PERMANOVA.size, dataset.filtered_2$Type, sim.method = "euclidean", p.adjust.m = "bonferroni", perm = 10000)

Pairwise_PERMANOVA.size.type

# Pairwise_PERMANOVA.perf <- pairwiseAdonis::pairwise.adonis(Y.PERMANOVA, dataset.filtered$Perforation, sim.method = "euclidean", p.adjust.m = "bonferroni", perm = 10000)


## DISPARITY ----

# Ensure Dataset_2DGM is a data frame or matrix
dataset.filtered_2 <- as.data.frame(dataset.filtered_2)

# Ensure dataset.filtered_2 is a data frame or matrix
dataset.filtered_2 <- as.data.frame(dataset.filtered_2)

# Subset the columns correctly (make sure PC1 to PC4 are the correct column names)
PC.data.subset.size <- dataset.filtered_2[, c("PC1", "PC2", "PC3", "PC4")]

# Assign row names based on the 'Type' column in the dataset
rownames_DATASETS.size <- as.factor(dataset.filtered_2$Type)

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

# Disparity Plot
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

# Plot with updated categories (Local vs Non-local)
disparity_df_discrete_PCA.plot <- 
  disparity_df_discrete_PCA %>%
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
  guides(color = FALSE, fill = FALSE)

# Display the plot
disparity_df_discrete_PCA.plot


# Custom color palette for higher contrast
sea_shell_sand_palette <- c("Sea" = "#FF6347", "Shell" = "#F8E1A1", "Sand" = "#D4AF37")

# Violin plot with custom color palette
Figure_AFb <- 
  disparity_df_discrete_PCA %>%
  ggplot(aes(x = Context, y = disparity)) +
  geom_violin(aes(fill = TS)) +  # Fill with the TS variable
  geom_boxplot(notch = TRUE, width = 0.1, fill = "white", color = "black") +
  theme_bw() +
  ggtitle(NULL) +
  xlab("") + 
  ylab("Disparity") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5, size = 16),
    axis.title.y = element_text(vjust = 0),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_manual(values = sea_shell_sand_palette) +  # Apply the custom color palette to TS
  guides(color = FALSE, fill = FALSE)

# Display the plot
Figure_AFb

# Load necessary package
library(patchwork)

# Assuming Figure_AFa, Figure_AFb, and Figure_AFc are ggplot objects
# You can use patchwork to combine them with the specified layout.

Figure_AF <- (Figure_AFa + Figure_AFb) / Figure_AFc + 
  plot_annotation(
    tag_levels = 'a'  # Adds tags 'a', 'b', 'c' to the respective plots
  )

# Display the combined plot
Figure_AF

# Save the combined figure as a high-resolution TIFF
ggsave("output/figures/Figure_AF.tif", Figure_AF, width = 10, height = 8, units = "in", dpi = 300)

ggsave("output/figures/Figure_AF.png", Figure_AF, width = 10, height = 8, units = "in", dpi = 300)

#merge Figure_AFc, Figure_AFb and Figure_AFa with 

# boxplot circumference ----

# Create the Circumference plot with the new palette
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

# Create the Area plot with the new palette
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

# Combine the plots using patchwork (to arrange side by side)
combined_plot <- (circumference | area)  # Use patchwork's pipe operator for side-by-side plots

# Save the combined figure as a high-resolution TIFF
ggsave("output/figures/Figure_S5.tif", combined_plot, width = 10, height = 5, units = "in", dpi = 300)
