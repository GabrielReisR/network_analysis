# Lendo qgraph ====
# Se não tiver, instalar usando: install.packages('qgraph', dep = T)
library(qgraph) # carrega o pacote

#' Documentação aqui:
#' https://cran.r-project.org/web/packages/qgraph/qgraph.pdf

# Criando input para qgraph ====
input <- 
  matrix(c(0,  1,  5,
           1,  0, -2,
           5, -2,  0),
         nrow = 3, ncol = 3,
         byrow = T)

qgraph(input)

# Escalando valores ====
input <-
  matrix(c(   0,  -0.7,   0.7,  -0.2,
           -0.7,     0,   0.2,     0,
            0.7,   0.2,     0,   0.4,
           -0.2,     0,   0.4,     0),
         nrow = 4, ncol = 4,
         byrow = T)

qgraph(matriz,
       edge.labels = T,
       edge.label.cex = 2) # adicionar `edge.labels` e `edge.label.cex`

# Modificando layout ====
L <- matrix(
  c(-1,   1, # nodo 1
     0,   0, # nodo 2
     1, 0.5, # nodo 3
    -1,   0), # nodo 4
  ncol = 2, byrow = T)

qgraph(input, layout = L)

# Mantendo o mesmo layout ====
rede_1 <- qgraph(input)
rede_1$layout

input <-
  matrix(c(  0,  -0.9,   0.4,  -0.2,
          -0.9,     0,   0.1,     0,
           0.4,   0.1,     0,   0.4,
          -0.2,     0,   0.4,     0),
         nrow = 4, ncol = 4,
         byrow = T)

input_2 <-
  matrix(c(  0,  -0.4,   0.4,  -0.2,
             -0.4,     0,   0.1,     0,
             0.4,   0.1,     0,   0.4,
             -0.2,     0,   0.4,     0),
         nrow = 4, ncol = 4,
         byrow = T)

qgraph(input,
       layout = rede_1$layout)

# Mantendo a mesma escala ====
qgraph(input,
       layout = rede_1$layout,
       minimum = 0.1, # `minimum` especifica o mínimo para entrar no grafo
       cut = 0.4, # `cut` delimita a partir de qual valor a grossura muda
       maximum = 0.7) # `maximum` delimita o valor da maior grossura

# Salvando grafos ====
#' O pessoal que trabalhou no qgraph não sugerem salvar o grafo a partir
#' do comando "Export".
#' Salvar então diretamente como comando no R.

# Possibilidade 1
qgraph(input,
       layout = rede_1$layout,
       filetype = 'png',
       filename = 'grafo')

# Possibilidade 2
qgraph(input,
       layout = rede_1$layout,
       filetype = 'pdf',
       filename = 'grafo_quadrado',
       width = 1.4 * 40,
       height = 40,
       nodeNames = c('Item 1', 'Item 2', 'Item 3', 'Item 4'))

#' Na documentação é tratada as formas de salvar em `qgraph(filetype)`
#' https://cran.r-project.org/web/packages/qgraph/qgraph.pdf

#' Argumentos para dimensões da imagem, em inches (polegadas)
#' width = largura
#' height = altura
#' 
#' *IMPORTANTE*
#' Se houver legenda, width deve ser 1.4 vezes o tamanho de height
#' para que o grafo seja quadrado.

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
         sampleSize = 964,
         legend = F,
         filetype = 'png',
         filename = 'rede')



