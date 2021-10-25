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

# Estimando redes psicométricas com HEXACO-60 ====
Data <- read.csv('HEXACOfacet.csv')sp <- centrality_auto(network)$ShortestPathLengths # compute all shortest paths

#' "The following for loops creates a matrix (netHfa) in which only the 
#' shortest paths that involve Hfa are included. *The other edges are zeros.*
#' The shortest paths involving node Hfa are also printed in the R console."

#' Aqui a gente vai escolher Hfa como sendo o principal pois é o que possui
#' maior influência esperada na rede.

network <- 
  qgraph(cor(Data), # correlações
         layout = 'spring',
         labels = colnames(Data), #' cada nodo terá o nome da coluna de *Data*
         groups = groups, #' nodos serão agrupados de acordo com *groups*
         graph = 'glasso', #' *correlações parciais com regularização glasso*
         sampleSize = 964, #' tamanho amostral
         legend = F)

# Criando matriz inicial
netHfa <- matrix(0,
                 ncol = length(network$Edgelist$from),
                 nrow = length(network$Edgelist$to))

node <- match("Hfa", colnames(Data))

# Criando matriz
for(i in 1:nrow(sp)){
  
  for (j in 1:ncol(sp)){
    
    if (j > i){
      sps <- sp[[i, j]]
      
      for (k in 1:length(sps)){
        
        if (length(sps[[k]]) > 2){
          
          if (node %in% sps[[k]][-c(1, length(sps[[k]]))]){
            print(colnames(network)[sps[[k]]])
            
            for (q in 1:(length(sps[[k]] - 1))){
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

# O vértice Hfa tem seu tamanho aumentado
netqgnofade <- qgraph(network,
                      layout = 'spring',
                      labels = colnames(Data),
                      groups = groups,
                      fade = FALSE,
                      legend = FALSE,
                      vsize = ifelse(colnames(Data) == "Hfa", 8, 5),
                      DoNotPlot = TRUE)

# Qualquer aresta diferente de 0 recebe a cor vermelha
netqgredblack <- qgraph(network,
                        layout = 'spring',
                        labels = colnames(Data),
                        groups = groups,
                        fade = FALSE,
                        edge.color = ifelse(netHfa == 0, "red", "black"),
                        DoNotPlot = TRUE)

#' Se a cor em netqgredblack for preta, recebe transparência 1 (completamente
#' visível). Se a cor em netqgredblack for vermelha (isso acontece quando
#' netHfa == 0), a transparência recebe o valor de 0.2 (esse alfa é arbitrário).
netqgnofade$graphAttributes$Edges$color <- 
  qgraph:::Fade(netqgnofade$graphAttributes$Edges$color,
                ifelse(netqgredblack$graphAttributes$Edges$color == "black",
                       1,
                       0.2), #' valor de *alfa* (transparência)
                "white")

#' Unindo as arestas de netqgnofade que foram classificadas por netqgredblack
#' como vermelhas às arestas de netqgnofade que foram classificadas por 
#' netqgredblack pretas, nessa ordem.
netqgnofade$graphAttributes$Graph$edgesort <- 
  c(netqgnofade$graphAttributes$Graph$edgesort
    [netqgnofade$graphAttributes$Graph$edgesort %in% 
        which(netqgredblack$graphAttributes$Edges$color == "red")],
    netqgnofade$graphAttributes$Graph$edgesort
    [netqgnofade$graphAttributes$Graph$edgesort %in%
        which(netqgredblack$graphAttributes$Edges$color == "black")])

#' Unindo as arestas de netqgnofade que foram classificadas por netqgredblack
#' como vermelhas às arestas de netqgnofade que foram classificadas por 
#' netqgredblack pretas, nessa ordem.
netqgnofade$graphAttributes$Edges$lty <- 
  ifelse(netqgredblack$graphAttributes$Edges$color == "black",
         1,
         3)

plot(netqgnofade)