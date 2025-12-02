# Preliminary Analysis

# Preliminary data analysis (~0.5 page + tables/figures) Provide a description of the data,
# including but not limited to the source (e.g., thesis data, published data), any data wrangling
# undertaken to prepare the data for analysis, summary statistics for relevant variables,
# response and predictor variable types (e.g., categorical vs. continuous), potential challenges in
# analysis (e.g., violations of normality; pseudoreplication), and pertinent graphical depictions.
# Create and include the link to a public GitHub repository for your project.


# I conducted Phomopsis Fungicide Trials at the Lake Erie Grape Research and Extension Center 
# in North East, PA, in 2025. I collected the data during the summer after spray 
# treatments were complete. The trials had the following design. Two vineyard varieties were 
# tested in separate plots: Concord trial and Niagara trial. The Concord trial had 5 blocks and 6
# fungicide treatments that included both a positive control (standard fungicide) and negative control 
# (water spray) set up as a randomized complete block design (RCBD). The Niagara trial, also a RCBD, 
# had 4 blocks and 5 treatments that included two positive controls and a negative control. The 
# predictor variables for both trials were a blocking affect and a treatment affect, both categorical variables. There are six 
# response variables can be divided into two groups: incidence and severity of phomopsis disease. Ten 
# shoots were randomly collected from each experimental unit of 8 vines and the shoot's nodes, leaves and clusters
# were rated for both incidence and severity. Incidence is a percentage of nodes, leaves, or cluster that
# are infected and is expressed as an integer. Severity is percentage of the total area of the nodes, 
# leaves, or clusters infected and is continuous. Most of the data wrangling was done in excel before 
# importing into R. This included calculating the the incidence and severity values from the Barrat 
# Horsfall tally data. Once in R, I changed the two predictor variables to factors. 

setwd("C:/Users/jib5787/OneDrive - The Pennsylvania State University/Documents/2025 Trials/Phomopsis/Concord")
getwd()

#install.packages("psych")
library(psych)
library(readxl)
ConcordR <- read_excel("~/2025 Trials/Phomopsis/Concord/ConcordR.xlsx")
View(ConcordR)

str(ConcordR)
# change Block and Trt to factors
ConcordR$Block <- as.factor(ConcordR$Block)
ConcordR$Trt <- as.factor(ConcordR$Trt)

#Summary Statistics for revelant variables------------------------------
summary(ConcordR)


#library(psych)
psych::describe(ConcordR)
desc <- psych::describe(ConcordR)
# Keep only selected columns
# desc_select <- desc[, c("mean","se", "sd", "min", "max", "skew", "kurtosis")]
desc_select <- desc[, c("mean","se", "sd", "min", "max")]

desc_select

# Challenges in analysis: Violation of assumptions
# Concord Trial Histograms-----------------------------------
library(tidyverse)

par(mfrow = c(3, 2),          # 3 × 2 layout
    oma = c(0, 0, 3, 0))      # outer margins (top margin increased for title)

hist(ConcordR$LeafSev, main = "Leaf Severity", col = "lightblue", xlab = "Severity")
hist(ConcordR$LeafInc, main = "Leaf Incidence", col = "lightgray", xlab = "Incidence")
hist(ConcordR$NodeSev, main = "Node Severity", col = "lightgreen", xlab = "Severity")
hist(ConcordR$NodeInc, main = "Node Incidence", col = "lightyellow", xlab = "Incidence")
hist(ConcordR$ClusterSev, main = "Cluster Severity", col = "lightpink", xlab = "Severity")
hist(ConcordR$ClusterInc, main = "Cluster Incidence", col = "lavender", xlab = "Incidence")

# Add overall title
mtext("Histograms Concord Trial", outer = TRUE, cex = 1.6, font = 1.5)

# Concord Trial Boxplots-------------------------------------------

library(ggplot2)
library(patchwork)

p1 <- ggplot(data = ConcordR, aes(x = Trt, y = LeafSev, color = Trt)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7) +
  theme_classic() + theme(legend.position = "none")

p2 <- ggplot(data = ConcordR, aes(x = Trt, y = LeafInc, color = Trt)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7) +
  theme_classic() + theme(legend.position = "none")

p3 <- ggplot(data = ConcordR, aes(x = Trt, y = NodeSev, color = Trt)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7) +
  theme_classic() + theme(legend.position = "none")

p4 <- ggplot(data = ConcordR, aes(x = Trt, y = NodeInc, color = Trt)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7) +
  theme_classic() + theme(legend.position = "none")

p5 <- ggplot(data = ConcordR, aes(x = Trt, y = ClusterSev, color = Trt)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7) +
  theme_classic() + theme(legend.position = "none")

p6 <- ggplot(data = ConcordR, aes(x = Trt, y = ClusterInc, color = Trt)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7) +
  theme_classic() + theme(legend.position = "none")

# Combine 6 plots in 3×2 layout
combined_plot <- (p1 | p2 ) / (p3 | p4) / (p5 | p6)

combined_plot +
  plot_annotation(
    title = "Concord Trial Boxplots: Disease Incidence and Severity",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  )


dev.off()  # closes the current plot device

# check for collinearity--------------------------------------
plot(ConcordR[,1:8], pch = 19)
cor(ConcordR[,3:8], method = c("pearson"), use = "complete.obs")

# pertinent graphical depictions (scatterplot, boxplots with jitters, residual plots)

# create and include the link to a public GitHub repository for project.