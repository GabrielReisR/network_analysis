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
       0,  0.3,    0, -0.3, 0.2, 0.3,
     0.3,    0, -0.9,    0,   0,    0,
       0, -0.9,    0,  0.8,   0,    0,
    -0.3,    0,  0.8,    0, 0.3,    0,
     0.2,    0,    0,  0.3,   0,    0,
     0.3,    0,    0,    0,   0,    0
    ),
  ncol = 6, nrow = 6,
  byrow = TRUE)

qgraph(mat,
       layout = 'spring',
       edge.labels = TRUE,
       labels = LETTERS[1:6],
       fade = FALSE)

# Estimando redes psicométricas com HEXACO-60 ====
Data <- read.csv('HEXACOfacet.csv')
#View(Data)

#' Criando grupos no objeto *groups*
groups <- factor(c(
  rep("Honesty Humility", 4),
  rep("Emotionality", 4),
  rep("Extraversion", 4),
  rep("Agreeableness vs. Anger", 4),
  rep("Conscientiousness", 4),
  rep("Openness to experience", 4)))

#' Rodando a *network de correlações*
rede_cor <-
  qgraph(cor(Data), # correlações
       layout = "spring",
       labels = colnames(Data), #' cada nodo terá o nome da coluna de *Data*
       groups = groups, #' nodos serão agrupados de acordo com *groups*
       legend = F)

#' Rodando a *network de correlações parciais*
qgraph(cor(Data), # correlações
       layout = rede_cor$layout,
       labels = colnames(Data), #' cada nodo terá o nome da coluna de *Data*
       groups = groups, #' nodos serão agrupados de acordo com *groups*
       graph = 'pcor', #' especifica *correlações parciais* (*partial* correl.)
       legend = F) 

#' Rodando a *network regularizada de correlações parciais*
network <- 
  qgraph(cor(Data), # correlações
         layout = rede_cor$layout,
         labels = colnames(Data), #' cada nodo terá o nome da coluna de *Data*
         groups = groups, #' nodos serão agrupados de acordo com *groups*
         graph = 'glasso', #' *correlações parciais com regularização glasso*
         sampleSize = 964, #' tamanho amostral
         legend = F)

# Estatísticas descritivas da rede Pt. 1 - Pesos das arestas ====
#' Vamos extrair os *edge weights* (ew), que são os *pesos das arestas*
ew <- network$Edgelist$weight
ew

#' *Quantas* associações existem?
length(ew) 

#' *Quantas* associações acima de 0 existem? (associações positivas)
sum(ew > 0)

#' *Quantas* associações abaixo de 0 existem? (associações negativas)
sum(ew < 0) 

#' *Arestas positivas*
mean(abs(ew[ew > 0])) #' média do valor absoluto de associação; M = 0,09
sd(abs(ew[ew > 0])) #' desvio-padrão do valor absoluto de associação; DP = 0,08

#' *Arestas negativas*
mean(abs(ew[ew < 0])) #' média do valor absoluto de associação; M = 0,04
sd(abs(ew[ew < 0])) #' desvio-padrão do valor absoluto de associação; DP = 0,04

#' Há diferenças significativas na média da força de associação entre
#' arestas positivas (ew > 0) e arestas negativas (ew < 0)?
t.test(abs(ew[ew > 0]),
       abs(ew [ew < 0]),
       var.equal = TRUE)

#' Há diferenças significativas, sendo t(109) = 3,16, p = 0,002. Arestas
#' positivas estão relacionadas a valores maiores de associação (M = 0,09, 
#' DP = 0,08) quando comparadas a arestas negativas (M = 0,04, DP = 0,04).
#' Pode-se concluir que arestas positivas tendem a possuir maior força de
#' associação na rede estimada.

# Estatísticas descritivas da rede Pt. 2 - Centralidade ====
centrality <- centrality_auto(network)

# Centralidade
nc <- centrality$node.centrality
View(nc) # Table A.1

# Visualizar centralidade - jeito fácil, usando qgraph
centralityPlot(network,
               include = 'all',
               orderBy = 'ExpectedInfluence')

# Visualizar centralidade - jeito difícil, usando ggplot2
centralityTable(network) %>% 
  
  mutate(node = as.factor(node)) %>% 
  
  mutate(node = fct_reorder(node, value)) %>% 
  
  ggplot(aes(x = node, y = value, color = measure, group = measure)) +
  
  geom_line(size = 1) +
  
  labs(x = '', y = '') +
  
  scale_color_discrete(name = 'Measures of centrality') +
  
  coord_flip() +
  
  theme_bw()

# Estatísticas descritivas da rede Pt. 3 - Small worlds e shortest paths ====
smallworldness(network) # smallworldness > 3: possui a propriedade small-world

# Criando rede
network <- 
  qgraph(cor(Data), # correlações
         layout = 'spring',
         labels = colnames(Data), #' cada nodo terá o nome da coluna de *Data*
         groups = groups, #' nodos serão agrupados de acordo com *groups*
         graph = 'glasso', #' *correlações parciais com regularização glasso*
         sampleSize = 964, #' tamanho amostral
         legend = F,
         theme = 'colorblind',
         vsize = ifelse(colnames(Data) == "Hfa", 8, 5))

# Criando rede de pathways
#' Quais as conexões de Hfa com todos os nodos da rede?
pathways(network, #' nome do objeto da rede
         from = 'Hfa', #' nome do nodo (da coluna) que queremos fixar
         #fading = 0.25, #' nível de transparência, o padrão é 0.25
         to = 1:24) #' 1:24 pois queremos todas as 24 variáveis de `Data`

#' Quais as conexões de Hfa com o nodo Efe?
pathways(network, #' nome do objeto da rede
         from = 'Hfa', #' nome do nodo (da coluna) que queremos fixar
         #fading = 0.25, #' nível de transparência, o padrão é 0.25
         to = 'Efe') #' 1:24 pois queremos todas as 24 variáveis de `Data`

# Criando rede de flow
# A pergunta aqui é: quais as conexões diretas que Hfa possui? E indiretas?
flow(network, from = 'Hfa')

# Estatísticas descritivas da rede Pt. 4 - Clustering e centralidade ====
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
