# Inicialização ====
load_libraries <- function(){
  if (!require("devtools"))
    install.packages("devtools"); library(devtools)
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("forcats"))
    install.packages("forcats"); library(forcats)
  if (!require("ggplot2"))
    install.packages("ggplot2"); library(ggplot2)
  if (!require("GPArotation"))
    install.packages("GPArotation"); library(GPArotation)
  if(!require("igraph"))
    install.packages("igraph"); library(igraph)
  if(!require("lavaan"))
    install.packages("lavaan"); library(lavaan)
  if(!require("Matrix"))
    install.packages("Matrix"); library(Matrix)
  if(!require("psych"))
    install.packages("psych"); library(psych)
  if(!require("sna"))
    install.packages("sna"); library(sna)
  if(!require("qgraph"))
    install.packages("qgraph"); library(qgraph)
}
load_libraries()

# Exemplo inicial ====
mat <- matrix(
  c(
       0,  0.3,    0, -0.3, 0.2,
     0.3,    0, -0.9,    0,   0,
       0, -0.9,    0,  0.8,   0,
    -0.3,    0,  0.8,    0, 0.3,
     0.2,    0,    0,  0.3,   0
  ),
  ncol = 5, nrow = 5,
  byrow = TRUE)

# Criando a rede
network <- qgraph(mat,
                  layout = 'spring',
                  edge.labels = TRUE,
                  labels = LETTERS[1:5],
                  theme = 'colorblind')

# Centralidade ====
centrality <- centrality_auto(network)
centrality

nc <- centrality$node.centrality
View(nc) # Table A.1 do artigo "State of the aRt..."

# Tabelas e figuras de centralidade
centralityTable(network, standardized = F) #' valores brutos
centralityTable(network) #' valores padronizados (escores z)

centralityPlot(network,
               include = 'all',
               orderBy = 'ExpectedInfluence')

#' Para mais informações, rodar `?centralityPlot`

#' Cálculo de *strength* (força)
#' Somar as associações absolutas de um nodo

#' Cálculo de *expected influence* (influência esperada)
#' Somar as associações de um nodo

#' Cálculo de *betweeness* (conectividade)
#' Somar quantas vezes o nodo é o caminho mais curto entre outros dois nodos

#' Cálculo de *closeness* (proximidade)
#' Distâncias de 
#' D -> A: 1/0.3 = 3.333333
#' D -> B: 1/0.8 + 1/0.9 = 2.361111
#' D -> C: 1/0.8 = 1.25
#' D -> E: 1/0.3 = 3.333333
#' Somar e inverter as distâncias de um nodo a outros nodos

# Pathways - caminho de um nodo a outro (ou de um nodo a todos outros) ====
#' A pergunta aqui é: qual o caminho mais curto do nodo X ao nodo Y?
#' Ver: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5216845/pdf/sbw055.pdf 

# Quais as conexões mais curtas de D com todos os nodos da rede?
pathways(network, #' nome do objeto da rede
         from = 'D', #' nome do nodo (da coluna) que queremos fixar
         #fading = 0.25, #' nível de transparência, o padrão é 0.25
         to = 1:5) #' 1:5 pois queremos todas as 5 variáveis A, B, C, D e E

# Quais as conexões mais curtas de D com um nodo específico da rede?
pathways(network, #' nome do objeto da rede
         from = 'D', #' nome do nodo (da coluna) que queremos fixar
         #fading = 0.25, #' nível de transparência, o padrão é 0.25
         to = 'B') #' 1:5 pois queremos todas as 5 variáveis A, B, C, D e E

# A pergunta aqui é: quais as conexões diretas que X possui? E indiretas?
# Criando rede de flow
flow(network, from = 'D')

# Plotando clustering e centralidade juntos ====
#' Clustering se refere a quantos triângulos um nodo consegue formar em uma rede
clustcoef <- clustcoef_auto(network)
clusteringPlot(network, signed = TRUE)

#' 1) Betweenness 
plot(clustcoef$signed_clustZhang, nc$Betweenness,
     col = "white", las = 1,
     xlab = "clustering coefficient", ylab = "betweenness")
text(clustcoef$signed_clustZhang, nc$Betweenness, rownames(nc))
abline(h = median(nc$Betweenness), col = "grey")
abline(v = median(clustcoef$signed_clustZhang), col = "grey")

#' 2) Closeness
plot(clustcoef$signed_clustZhang, nc$Closeness*1000,
     col = "white", las = 1,
     xlab = "clustering coefficient",
     ylab = "closeness")
text(clustcoef$signed_clustZhang, nc$Closeness*1000, rownames(nc))
abline(h = median(nc$Closeness*1000), col = "grey")
abline(v = median(clustcoef$signed_clustZhang), col = "grey")

#' 3) Strength
plot(clustcoef$signed_clustZhang, nc$Strength,
     col = "white", las = 1,
     xlab = "clustering coefficient", ylab = "strength")
text(clustcoef$signed_clustZhang, nc$Strength, rownames(nc))
abline(h=median(nc$Strength), col = "grey")
abline(v=median(clustcoef$signed_clustZhang), col = "grey")

#' 4) Expected Influence
plot(clustcoef$signed_clustZhang, nc$ExpectedInfluence,
     col = "white", las = 1,
     xlab = "clustering coefficient", ylab = "expected influence")
text(clustcoef$signed_clustZhang, nc$ExpectedInfluence, rownames(nc))
abline(h=median(nc$Strength), col = "grey")
abline(v=median(clustcoef$signed_clustZhang), col = "grey")