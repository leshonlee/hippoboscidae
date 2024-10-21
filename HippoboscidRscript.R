###### SPECIES ACCUMULATION CURVE ######
#Install packages
install.packages("ggplot2")
install.packages("vegan")

#Load the following packages:
library("ggplot2")
library("vegan")

#Set your working directory
setwd("/Users/Leshon/Desktop/Desktop/HippoboscidaeProject")

#Loading community matrix for region species accumulation curve
comm_all <- read.csv("commatrix_allbirds.csv", header=T)
rownames(comm_all) <- comm_all$X

#Remove the original "x" column
comm_all <- comm_all[, -which(names(comm_all) == "X")]

#Loading community matrix for region species accumulation curve
comm_pigeon <- read.csv("commatrix_pigeons.csv", header=T)
rownames(comm_pigeon) <- comm_pigeon$X

#Remove the original "x" column
comm_pigeon <- comm_pigeon[, -which(names(comm_pigeon) == "X")]

#specaccum
comm_pigeon_t=t(comm_pigeon)
x=specaccum(comm_pigeon_t, method = "exact", permutations = 1000,
            conditioned =TRUE, gamma = "jack1",  w = NULL)
comm_all_t=t(comm_all)
y=specaccum(comm_all_t, method = "exact", permutations = 1000,
            conditioned =TRUE, gamma = "jack1",  w = NULL)

#Plotting the species accumulation curve
tiff("specaccumCurve.tiff", units="in", width=12, height=8, res=300)

plot(y, add = FALSE, ci = 1,
     col = "black", lty = 1,
     ci.type = "polygon", ci.col = rgb(0, 158/255, 115/255, 0.5), ci.lty = 0,
     xlab = "Number of birds sampled", 
     ylab = "Number of Hippoboscidae species",
     cex.lab = 1,  # Increase font size of axis labels
     cex.axis = 1.5  # Increase font size of axis numbers
)

plot(x, add = TRUE, ci = 1, 
     col = "black", lty = 2,
     ci.type = "polygon",ci.col= rgb(213/255, 94/255, 0, 0.5),ci.lty = 0,
     cex.lab = 2,  # Increase font size of axis labels
     cex.axis = 1.5  # Increase font size of axis numbers
)

dev.off()


###### BIPARTITE NETWORK ######
# Load necessary packages
library(bipartite)
library(dplyr)
library(RColorBrewer)
library(bipartiteD3)

# Load and organize bipartite network data
data_common <- read.csv("bipartite_data_commononly.csv", header = TRUE) %>%
  rename(higher = Parasite, lower = Host) %>%
  mutate(webID = "web1")

# Consolidate frequencies by higher and lower levels
consolidated_data <- data_common %>%
  group_by(higher, lower, webID) %>%
  summarise(freq = n(), .groups = 'drop')

# Convert consolidated data frame to a list of webs
web_list <- bipartite::frame2webs(consolidated_data)

# Access the matrix for the specific webID
common_parasite_host_matrix <- web_list$web1

# Create bipartite network plot
bipartite_D3(common_parasite_host_matrix, colouroption = "brewer", ColourBy = 2, 
             PrimaryLab = 'Bird Host', SecondaryLab = 'Hippoboscid Fly', SiteNames = ' ')