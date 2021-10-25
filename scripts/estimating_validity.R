# Inicialização ====
load_libraries <- function(){
  if (!require("devtools"))
    install.packages("devtools"); library(devtools)
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if(!require("EGAnet"))
    install.packages("EGAnet"); library(EGAnet)
  if (!require("fitdistrplus"))
    install.packages("fitdistrplus"); library(fitdistrplus)
  if (!require("ggpubr"))
    install.packages("ggpubr"); library(ggpubr)
  if(!require("psych"))
    install.packages("psych"); library(psych)
  if(!require("psychTools"))
    install.packages("psychTools"); library(psychTools)
}

load_libraries()

set.seed(6724)

# Checking redundancy ====
# Select Five Factor Model personality items only
idx <- na.omit(match(gsub("-", "", unlist(spi.keys[1:5])), colnames(spi)))
items <- spi[,idx]

# Identify redundant nodes
redund <- node.redundant(items, method = "wTO", type = "adapt")

# Change names in redundancy output to each item’s description
key.ind <- match(colnames(items), as.character(spi.dictionary$item_id))
key <- as.character(spi.dictionary$item[key.ind])

# Use key to rename variables
named.nr <- node.redundant.names(redund, key)

# Combining redundant responses
combined.nr <- node.redundant.combine(named.nr, type = "latent")

# New items
new.items <- combined.nr$data

# Checking dimensionality ====
ega <- EGA(items, model = "glasso", algorithm = "louvain")

# View dimensions
View(ega$dim.variables)

# Compute standardized node strength
net.loads(ega)$std

# Compute bootstrap
boot <- bootEGA(items, iter = 10, model = "glasso",
                type = "parametric", plot.typicalStructure = FALSE)

# Compute structural consistency
sc <- dimStability(boot, orig.wc = ega$wc)

# Print structural consistency
sc$dimensions

# Item stability statistics plot
sc$items$plot.itemStability

# View item stability across dimensions
View(sc$items$item.dim.rep)