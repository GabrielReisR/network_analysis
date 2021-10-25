# Inicialização ====
load_libraries <- function(){
  if (!require("bootnet"))
    install.packages("bootnet"); library(bootnet)
  if (!require("corpcor"))
    install.packages("corpcor"); library(corpcor)
  if (!require("devtools"))
    install.packages("devtools"); library(devtools)
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("ggplot2"))
    install.packages("ggplot2"); library(ggplot2)
  if(!require("lavaan"))
    install.packages("lavaan"); library(lavaan)
  if(!require("magrittr"))
    install.packages("magrittr"); library(magrittr)
  if(!require("psych"))
    install.packages("psych"); library(psych)
  if(!require("qgraph"))
    install.packages("qgraph"); library(qgraph)
  if(!require("semPlot"))
    install.packages("semPlot"); library(semPlot)
  if(!require("tidyr"))
    install.packages("tidyr"); library(tidyr)
}
load_libraries()

# Gerando matriz da rede ====
mat <- matrix(
  c(
       0,  0.3,    0,    0,
     0.3,    0, 0.20, 0.25,
       0, 0.20,    0, 0.35,
       0, 0.25, 0.35,    0
  ),
  ncol = 4, nrow = 4,
  byrow = TRUE)

qgraph(mat,
       theme = 'gray')

# Transformação inicial ====
initial_eigenvalues <- eigen(mat)
lowest_eigen <- min(initial_eigenvalues$values)

diag(mat) <- -1 * min(initial_eigen$values)

# Gerando matriz de eigenvectors ====
Q <- eigen(mat)$vectors
Q

# Gerando vetor de eigenvalues (com p - 1) ====
r <- eigen(mat)$values
r[4] <- 0
r

# Gerando equivalência fatorial ====
A <- matrix(0, ncol = ncol(Q), nrow = nrow(Q))
for(i in 1:nrow(Q)){
  for(j in 1:length(r)){
    A[i, j] <- -2* sqrt(r[j]/2) * Q[i, j]
  }
}

A

# CFA model ====
mat %<>% as.data.frame()

model <- '
eta1 =~ V1 + V2 + V3 + V4
eta2 =~ V1 + V2 + V3 + V4
eta3 =~ V1 + V2 + V3 + V4
'
model <- '
eta1 =~ V1 + V2 + V3 + V4
'
fit <- cfa(model, data = mat)
fit

semPaths(fit)

# Replicando exemplo do artigo (n = 414) ====
mat <- matrix(
  c(
        1,  0.187,  0.476, 0.517, 0.393, 0.399, 0.520, 0.529, 0.272,
    0.187,      1, -0.097, 0.304, 0.047, 0.439, 0.264, 0.309, 0.426,
    0.476, -0.097,      1, 0.436, 0.557, 0.260, 0.394, 0.395, 0.086,
    0.517,  0.304,  0.436,     1, 0.476, 0.351, 0.576, 0.520, 0.320,
    0.393,  0.047,  0.557, 0.476,     1, 0.422, 0.508, 0.502, 0.178,
    0.399,  0.439,  0.260, 0.351, 0.422,     1, 0.492, 0.531, 0.449,
    0.520,  0.264,  0.394, 0.576, 0.508, 0.492, 1,     0.740, 0.291,
    0.529,  0.309,  0.395, 0.520, 0.502, 0.531, 0.740,     1, 0.368,
    0.272,  0.426,  0.086, 0.320, 0.178, 0.449, 0.291, 0.368,      1
  ),
  ncol = 9, nrow = 9,
  byrow = TRUE)

colnames(mat) <- c('V1', 'V2', 'V3', 'V4',
                   'V5', 'V6', 'V7', 'V8', 'V9')
mat

# Covariance matrix
s <- sqrt(diag(mat))
cov_mat <- cor2cov(mat, sigma = s)

# Estimate UFM
ufm <- 'procr =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9'
cfa_procr <- cfa(ufm, sample.cov = mat, sample.nobs = 414)

# Estimate SNM
EBIC <- EBICglasso(mat, n = 414)

diag(EBIC) <- 1 # model implied partial correlation matrix

# Setting hyper-parameters ====
n <- 10000 #number of iterations
nobs <- 414 #number of observations
nV <- 9 #number of variables
cov_FA <- fitted(cfa_procr)$cov # implied covariance matrix UFM
cor_FA <- cov2cor(cov_FA)
cor_NW <- pcor2cor(EBIC)
cor_data <- mat
pcor_data <- cor2pcor(mat)
U_data_cor <- cor_data[lower.tri(cor_data, diag = F)] 

# Getting proportion of variables with different signs OR stronger pcors ====
U_data_pcor <- pcor_data[lower.tri(pcor_data, diag =
                                   F)] #unique elements in partial correlation matrix
signswitch_data <- which(U_data_pcor * U_data_cor <
                           0) #number of correlations that have different sign than pcor
stronger_data <- which((U_data_pcor ^ 2) > (U_data_cor ^ 2))
together_data <- union(signswitch_data, stronger_data)
total_data <- length(together_data)
propData <- total_data / length(U_data_cor) #proportion of correlations that have different sign than pcor

# Creating dataset ====
propFA <- rep(NA, n)
propNW <- rep(NA, n)

for(i in 1:n) {
  DataNW <- data.frame(mvrnorm(n = nobs, mu =
                               rep(0, nV), Sigma = cor_NW, empirical = F))
  DataFA <- data.frame(mvrnorm(n = nobs, mu =
                               rep(0,nV), Sigma = cor_FA, empirical = F))
  corFA <- cor(DataFA)
  pcorFA <- cor2pcor(corFA)
  corNW <- cor(DataNW)
  pcorNW <- cor2pcor(corNW)
  U_FA_cor <- corFA[lower.tri(corFA, diag = F)]
  U_FA_pcor <- pcorFA[lower.tri(pcorFA, diag = F)]
  U_NW_cor <- corNW[lower.tri(corNW, diag = F)]
  U_NW_pcor <- pcorNW[lower.tri(pcorNW, diag = F)]
  signswitch_FA <- which(U_FA_pcor * U_FA_cor < 0)
  signswitch_NW <- which(U_NW_pcor * U_NW_cor
                         < 0)
  stronger_FA <- which((U_FA_pcor ^ 2) >(U_FA_cor
                                         ^2))
  stronger_NW <- which((U_NW_pcor ^ 2)
                       >(U_NW_cor^2))
  together_FA <- union(signswitch_FA, stronger_FA)
  together_NW <- union(signswitch_NW, stronger_NW)
  total_FA <- length(together_FA)
  total_NW <- length(together_NW)
  propFA[i] <- total_FA/length(U_FA_cor)
  propNW[i] <- total_NW/length(U_NW_cor)
}

#' Proportions of partial correlations that have different sign than cor or are
#' stronger than cor, implied by SNM
propNW

#' Proportions of partial correlations that have different sign than cor or are
#' stronger than cor, implied by UFM
propFA

#' Proportions of partial correlations that have different sign than cor or are 
#' stronger than cor, in data
propData

df <- data.frame(Network = propNW,
                 Factor = propFA)

df %<>%
  pivot_longer(
    cols = Network:Factor,
    names_to = 'Type',
    values_to = 'Proportion'
  )

df %>%
  # Initial plotting
  ggplot(aes(x = Proportion, fill = Type)) +
  
  # Making histogram
  geom_histogram(aes(y=..count../sum(..count..)),
                     color= "#e9ecef", alpha = 0.6,
                     position = 'identity') +
  
  # Fill arguments
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  
  # Drawing vertical line
  geom_vline(xintercept = propData, size = 1.2, alpha = 0.7) +
  
  # Axis, labels and themes
  ylim(c(0, 0.20)) +
  labs(y = "", fill = "Model Type") +
  theme_classic()
  







