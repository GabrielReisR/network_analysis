# Please, specify the folder on your computer in which you want to work.
# Including the data file in the same folder is also suggested.
setwd("C:\\yourpath\\")

# Uncomment the following lines to install the required packages
# install.packages("devtools", dep = TRUE)
# install.packages("parcor", dep = TRUE)
# install.packages("Matrix", dep = TRUE)
# install.packages("sna", dep = TRUE)
# install.packages("psych", dep = TRUE)
# install.packages("lavaan", dep = TRUE)
# install.packages("qgraph", dep = TRUE)
# install.packages("GPArotation", dep = TRUE)
# install.packages("ggplot2", dep = TRUE)
# install.packages("dplyr", dep = TRUE)

# You need at least version 1.2.5 of qgraph. Check this with:
packageDescription("qgraph")$Version

# If you have an older version, install the developmental version via GitHub:
# devtools:::install_github("qgraph", "sachaepskamp")
# This requires Rtools (http://cran.r-project.org/bin/windows/Rtools/) on Windows
# Or Xcode (https://developer.apple.com/xcode/) on Mac

# load the required packages
library("qgraph")
library("parcor")
library("Matrix")
library("psych")
library("dplyr")
library("ggplot2")

#############################################
# A toy example, for illustrative purposes ##
#############################################

# define the weight matrix
mat <- matrix(c(0,  0.3,  0, -0.3,  0.2,  0.3,
                0.3,  0, -0.9,  0,  0,  0,
                0, -0.9,  0, 0.8,  0,  0,
                -0.3, 0,  0.8,  0, 0.3,  0,
                0.2,  0, 0,  0.3,  0,  0,
                0.3, 0,  0,  0,  0,  0), ncol=6, nrow=6, byrow=TRUE)

# plot the network
qgraph(mat, layout="spring", edge.labels=TRUE, labels=LETTERS[1:6], fade=FALSE)

# compute centrality
centrality_auto(mat)
# compute clustering coefficient
clustcoef_auto(mat)
# compute transitivity
set.seed(100)
smallworldness(mat)["trans_target"]

###############################################
# HEXACO-60 data: exploratory factor analysis #
###############################################
# load the data in R
Data <- read.csv("mmc1.csv")
str(Data)
# parallel analysis
paran <- fa.parallel(Data)
# exploratory factor analysis
fan <- fa(r=Data, nfactors=6, rotate="Varimax")
# compute squared multiple correlations
SMC <- smc(Data)


#######################################
# Network analysis on the HEXACO-60 ###
#######################################
# plot the correlation network
groups <- c(rep("Honesty Humility", 4), rep("Emotionality", 4), rep("Extraversion", 4), rep("Agreeableness vs. Anger", 4), rep("Conscientiousness", 4), rep("Openness to experience", 4))
netCor <- qgraph(cor(Data), layout = "spring", labels = colnames(Data), groups = groups)

# plot the partial correlation network
netPcor <- qgraph(cor(Data), layout = "spring", labels = colnames(Data), groups = groups, graph = "concentration")

# estimate and plot the adaptive lasso network
set.seed(100)
adls <- adalasso.net(Data)
network <- as.matrix(forceSymmetric(adls$pcor.adalasso))
colnames(network) <- rownames(network) <- colnames(Data)
netqg <- qgraph(network, layout = "spring", labels = colnames(Data), groups=groups)

# In figure 2 these are plotted using the layout of the third network:
L <- netqg$layout
qgraph(netCor, layout = L)
qgraph(netPcor, layout = L)

# Descriptive statistics
ew <- network[upper.tri(network)] # edge weights are saved in a vector
sum(ew!=0) # the number of edges
sum(ew>0) # the number of positive edges
sum(ew<0) # the number of negative edges
t.test(abs(ew[ew>0]), abs(ew[ew<0]) , var.equal=TRUE) # weights of positive vs. negative edges

# centrality analysis
centrality <- centrality_auto(network)
centralityPlot(network)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality
# show the 10 most betweenness-central edges in decreasing order of centrality
head(ebc,10)

# clustering coefficients
clustcoef <- clustcoef_auto(network)
clusteringPlot(network, signed = TRUE)

# correlations of centrality indices (Table 1)
corr.test(cbind(nc, fan$complexity, SMC), adjust = "none")
corr.test(cbind(nc, fan$complexity, SMC), method ="spearman", adjust = "none")

# correlations of clustering coefficients (Table 2)
corr.test(clustcoef, adjust = "none")
corr.test(clustcoef, adjust = "none", method = "spearman")

# correlation of centrality and clustering coefficients
corr.test(clustcoef, nc, method="pearson", adjust="none")

# scatterplots of betweenness and clustering coefficient
# 1) Betweenness 
plot(clustcoef$signed_clustZhang, nc$Betweenness, col="white", las=1, xlab="clustering coefficient", ylab="betweenness")
text(clustcoef$signed_clustZhang, nc$Betweenness, rownames(nc))
abline(h=median(nc$Betweenness), col="grey")
abline(v=median(clustcoef$signed_clustZhang), col="grey")

# 2) Closeness
plot(clustcoef$signed_clustZhang, nc$Closeness*1000, col="white", las=1, xlab="clustering coefficient", ylab="closeness")
text(clustcoef$signed_clustZhang, nc$Closeness*1000, rownames(nc))
abline(h=median(nc$Closeness*1000), col="grey")
abline(v=median(clustcoef$signed_clustZhang), col="grey")

# 2) Strength
plot(clustcoef$signed_clustZhang, nc$Strength, col="white", las=1, xlab="clustering coefficient", ylab="strength")
text(clustcoef$signed_clustZhang, nc$Strength, rownames(nc))
abline(h=median(nc$Strength), col="grey")
abline(v=median(clustcoef$signed_clustZhang), col="grey")

## smallworldness
set.seed(100)
smallworldness(network)

# inspect the shortest paths that involve node Hfa #
sp <- centrality(netqg)$ShortestPaths # compute all the shortest paths with qgraph
# the following for loops creates a matrix (netHfa) in which only the shortest paths
# that involve Hfa are included. The other edges are zeros.
# The shortest paths involving node Hfa are also printed in the R console.
netHfa <- matrix(0, ncol=ncol(network), nrow=nrow(network))
node <- match("Hfa", colnames(network))
for(i in 1:nrow(sp))
{
  for (j in 1:ncol(sp))
  {
    if (j > i)
    {
      sps <- sp[[i, j]]
      
      for (k in 1:length(sps))
      {
        if (length(sps[[k]]) > 2)
        {
          if (node %in% sps[[k]][-c(1, length(sps[[k]]))])
          {
            print(colnames(network)[sps[[k]]])
            for (q in 1:(length(sps[[k]] - 1)))
            {
              netHfa[sps[[k]][q], sps[[k]][q + 1]] <-
                netHfa[sps[[k]][q + 1], sps[[k]][q]] <-
                network[sps[[k]][q], sps[[k]][q + 1]]
            }
          }
        }
      }
    }
  }
}

# plot the network in a layout that highlights the shortest paths that pass trough Hfa
netqgnofade <- qgraph(network,
                      layout = netqg$layout,
                      labels = colnames(Data),
                      groups=groups,
                      fade = FALSE,
                      fade = FALSE,
                      legend = FALSE,
                      vsize = ifelse(colnames(Data) == "Hfa", 8, 5),
                      DoNotPlot = TRUE)

netqgredblack <- qgraph(network,
                        layout = netqg$layout,
                        labels = colnames(Data),
                        groups=groups,
                        fade = FALSE,
                        edge.color = ifelse(netHfa==0,"red","black"),
                        DoNotPlot = TRUE)

netqgnofade$graphAttributes$Edges$color <- qgraph:::Fade(netqgnofade$graphAttributes$Edges$color,ifelse(netqgredblack$graphAttributes$Edges$color == "black", 1, 0.2), "white")
netqgnofade$graphAttributes$Graph$edgesort <- c(netqgnofade$graphAttributes$Graph$edgesort[netqgnofade$graphAttributes$Graph$edgesort %in% which(netqgredblack$graphAttributes$Edges$color == "red")],
                                                netqgnofade$graphAttributes$Graph$edgesort[netqgnofade$graphAttributes$Graph$edgesort %in% which(netqgredblack$graphAttributes$Edges$color == "black")])
netqgnofade$graphAttributes$Edges$lty <- ifelse(netqgredblack$graphAttributes$Edges$color == "black", 1, 3)
plot(netqgnofade)

#############################
# Stability of the results ##
#############################
B <- 10 # number of repetitions.

# This code takes a very long time to run for B = 900 networks,
# even more than one day, depending on the hardware and on the operating system.
# We suggest to try and run the code with a lower value of B first.

# extract B networks, each with a different seed
NETWORKS <- mclapply(1:B, function(x) {set.seed(x); adalasso.net(Data)$pcor.adalasso})

# Number of edges histogram:
nEdges <- sapply(NETWORKS,function(x)sum(x[upper.tri(x)]!=0))
plot(hist(nEdges, breaks = 120:160))
median(nEdges)

# Number of edges Hmo:
names(Data[4])
Hmo <- sapply(NETWORKS, function(x)sum(x[4,-4]!=0))
range(Hmo)

# compute tables:
centTab <- centralityTable(NETWORKS,labels=names(Data),relative=FALSE)
clustTabUS <- clusteringTable(NETWORKS,labels=names(Data),relative=FALSE)
clustTabS <- clusteringTable(NETWORKS,labels=names(Data),relative=FALSE,signed=TRUE)

# compute centralities of the network used in the paper
centNet <- centralityTable(network, labels = names(Data), relative = FALSE)
clustNet <- clusteringTable(network, labels = names(Data), relative = FALSE, signed = TRUE)

# Reorder dfs:
centTab <- centTab[order(centTab$node),] 
centNet <- centNet[order(centNet$node),] 
clustTabS <- clustTabS[order(clustTabS$node),] 
clustNet <- clustNet[order(clustNet$node),] 

clustNet <- clustNet %.% filter(measure %in% c("Zhang", "Onnela"))
clustTabS <- clustTabS %.% filter(measure %in% c("Zhang", "Onnela"))

# Plot:
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
col <-  gg_color_hue(1)

g <- ggplot(rbind(centTab,clustTabS), aes(x = value, y = node, group = type)) +
  geom_path(alpha = 0.05) +  xlab("") + ylab("") +
  facet_grid( ~ measure, scales = "free")

g <- g + geom_path(data=rbind(centNet,clustNet), alpha = 1, colour = col)
g


#######################
# Network simulations #
#######################
library(psych)
library(qgraph)
library(lavaan)

#### Function ####
simulator <- function(k, m, s, n, centrals, n.targets){  
  
  # ====
  #' *k*: number of nodes
  #' *m*: mean of weights (or edges)
  #' *s*: standard deviation of weights (or edges)
  #' *n*: noise parameter
  #' *centrals*: identification of central nodes
  #' *n.targets*: number of target nodes
  # ====
  
  y <- rnorm(k)
  
  ## We first creat a matrix of connection weights and fill it with random values 
  
  w <- matrix(rnorm(k*k, m, s), k) 
  
  ## Here we create an universal distance matrix
  ## The distances will later influence the connection weights
  
  d <- as.matrix(dist(1:k))
  
  ## Then we introduce some randomness into the distance matrix, which means in substantive terms:
  ##  ... There will be some consistency and some idiosyncracy across agents in their network architecture
  
  d <- d * matrix(abs(rnorm(k*k, mean=1, sd=n)), k)
  d <- d/max(d)                                      ## This normalizes the distnace matrix
  
  ## The effective weigths are inversely weighed by the distance between nodes
  
  w <- w / d
  
  ## Here the nodes will influence each other according to the connection weights, which are weighed by distances
  ## For the eas of understanding, this is a step-by-step process
  ## The central nodes will activate one-by-one and each will influence its targets
  ## Node scores are updated according to weights
  
  for(i in centrals) {
    t <- sample(k-1, n.targets)  
    y[-i][t] <- y[-i][t] + w[,-i][i,t] * y[i]   
  }  
  
  ## Return updated scores  
  
  y 
  
}

#### The simplest simulation ####


agents <- replicate(5000, simulator(k = 10, m=.3, s=.1, n=.3, centrals=5, n.targets=9))
agents <- t(agents)

## Apply principal component analyses (PCA) on these data, as customary in personality research

fa.parallel(agents) # Parallel analyses to detect the appropriate number of components to retain
principal(agents,1)$loadings # PCA with one component

## Use the qgraph package to draw a network graph of the data and calculate centralities

qgraph(cor(agents), layout="spring", graph = "pcor") # Draws a "concentration" qgraph

c <- centrality(qgraph(cor(agents), layout="spring")) # Creates a qgraph object
c$Betweenness; plot(c$Closeness, lab=c(21,5,7), type="l", main="Closeness", xlab="Nodes", las=2, ylab="") # Reports centralities

## Apply Confirmatory Factor Analysis (CFA) on these data

colnames(agents) = paste("Q", 1:ncol(agents), sep="") # Names for the variables
model <- "F =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10" # Create a unidimensional latent factor model
fit <- cfa(model, data.frame(agents), std.lv=T) # Fit the model (may not always fit)
summary(fit, fit.measures=T, standardized=T)

## One may vary the "noise" parameter n and see what happens

#### The slightly more complex simulation ####

k = 30

agents <- replicate(5000, simulator(k,
                                    n = .25,
                                    
                                    #' For any simulation, return 1 weight value
                                    #' with mean .005 and sd .001
                                    m = rnorm(1,.005,.001),
                                    s = .001,
                                    
                                    #' For any simulation, return a number of
                                    #' specific central columns from 1 to 30 
                                    #' that are random and vary between 1 & 30
                                    centrals = sample(k, sample(k, 1)),
                                    
                                    #' For any simulation, return a number of
                                    #' targets that is random and varies
                                    #' between 1 and 29
                                    n.targets = sample(k-1, 1)))

agents <- t(agents) 

fa.parallel(agents) 

## Try different PCA soltuions

principal(agents,1)$loadings
principal(agents,2)$loadings
principal(agents,3)$loadings
principal(agents,4)$loadings
principal(agents,5)$loadings

qgraph(cor(agents), layout="spring", graph = "pcor") 

## Fit a five-factor CFA model on the data

colnames(agents) = paste("Q", 1:ncol(agents), sep="") 
model <- "F1 =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6
F2 =~ Q7 + Q8 + Q9 + Q10 + Q11 + Q12
F3 =~ Q13 + Q14 + Q15 + Q16 + Q17 + Q18
F4 =~ Q19 + Q20 + Q21 + Q22 + Q23 + Q24
F5 =~ Q25 + Q26 + Q27 + Q28 + Q29 + Q30"

fit <- cfa(model, data.frame(agents), std.lv=T) 
summary(fit, fit.measures=T, standardized=T)


