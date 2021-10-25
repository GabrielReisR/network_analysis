# Inicialização ====
load_libraries <- function(){
  if (!require("bootnet"))
    install.packages("bootnet"); library(bootnet)
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
  if(!require("tidyr"))
    install.packages("tidyr"); library(tidyr)
}
load_libraries()

# Getting dataset ====
FullData <- read.csv('.//data//qs.csv')

Data <- FullData %>%
  filter(EPOCH == "BASELINE",
         grepl("^PSSR\\d+A$",QSTESTCD)) %>%
  select(USUBJID,QSTEST,QSORRES) %>%
  spread(QSTEST, QSORRES) %>%
  select(-USUBJID) %>%
  mutate_each(funs(replace(.,.=="NOT ANSWERED",NA))) %>%
  mutate_each(funs(ordered(.,c("NOT AT ALL",
                               "ONCE A WEEK",
                               "2-4 TIMES PER WEEK/HALF THE TIME",
                               "5 OR MORE TIMES PER WEEK/ALMOST ALWAYS"))))
names(Data) <- seq_len(ncol(Data))

Data %<>% mutate_if(is.factor, as.character)

Data[Data == "NOT AT ALL"] <- "0"
Data[Data == "ONCE A WEEK"] <- "1"
Data[Data == "2-4 TIMES PER WEEK/HALF THE TIME"] <- "2"
Data[Data == "5 OR MORE TIMES PER WEEK/ALMOST ALWAYS"] <- "3"

Data %<>% mutate_if(is.character, as.numeric)

# Estimating network ====
Network <- estimateNetwork(Data,
                           default = "EBICglasso")

plot(Network, 
     layout = "spring",
     labels = TRUE)

centralityPlot(Network, include = 'all')

# Getting bootstrapped CI intervals for edge weights ====
boot1 <- bootnet(Network, nBoots = 2500,
                 nCores = 8)

plot(boot1, labels = FALSE,
     order = "sample")

print(boot1)

# Getting case-dropping bootstraps for centrality indices stability ====
boot2 <- bootnet(Network, nBoots = 2500,
                 type = "case", nCores = 8,
                 statistics = c(
                   'strength',
                   'expectedInfluence',
                   'betweenness',
                   'closeness'
                 ))

plot(boot2, 'all')

centralityPlot(network_antes,
               include = 'all',
               orderBy = 'ExpectedInfluence')

corStability(boot2, statistics = "all", verbose = T)

# Testing for significant differences of strength indices ====
# Do nodes 3 and 17 differ in strength centrality?
differenceTest(boot1, 3, 17,
               "strength")

plot(boot1,
     "edge",
     plot = "difference",
     onlyNonZero = TRUE,
     order = "sample")

plot(boot1, "strength")
