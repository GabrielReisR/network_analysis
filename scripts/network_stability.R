# Inicialização ====
load_libraries <- function(){
  if (!require("bootnet"))
    install.packages("bootnet"); library(bootnet)
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("ggplot2"))
    install.packages("ggplot2"); library(ggplot2)
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

# Criando data ====
data <- na.omit(psych::bfi[,1:25])

# Estimando rede ====
network <- estimateNetwork(data,
                           default = 'Borsboom', # estimação do modelo
                           corMethod = 'cor_auto') # tipo de correlação

# Visualizando rede ====
items <- scan(".\\data\\bfi_items.txt",
              what = 'character',
              sep = '\n')

traits <- rep(c('Agreeableness',
                'Conscientiousness',
                'Extraversion',
                'Neuroticism',
                'Openness'),
              each = 5)

plot(network,
     labels = names(data),
     label.cex = 0.75, # ajusta tamanho do texto dos nodos
     legend.cex = 0.3, # ajusta tamanho do texto da legenda
     groups = traits,
     nodeNames = items,
     height = 5,
     width = 5 * 1.4,
     filename = '.\\figures\\bfi_EBICglasso',
     filetype = 'png'
     )

#' *IMPORTANTE*: rodar diferentes modelos e ir salvando!

# Estabilidade Pt. 1: Estabilidade das arestas ====
boot_non_par <- bootnet(network,
                        type = 'nonparametric',
                        nBoots = 500, # é aconselhado 2500 no artigo
                        nCores = 7)

# Plot do intervalo de confiança de 95% para as arestas ordenado por 'sample'
plot(boot_non_par,
     order = "sample")

# Estabilidade Pt. 2: Estabilidade da centralidade ====
boot_case <- bootnet(network,
                     type = 'case',
                     nBoots = 2500, # é aconselhado 2500 no artigo
                     nCores = 8,
                     statistics = c(
                       'strength',
                       'expectedInfluence',
                       'betweenness',
                       'closeness'))

# Plot da estabilidade das medidas de centralidade
plot(boot_case, statistics = 'all')

# Estabilidade das correlações
#' *CS-Coefficient*
#' Porcentagem da amostra que pode ser excluída para se manter, com 95% de IC,
#' valores de correlação igual ou acima a r = 0.7 com a amostra original.
#' Ideal que CS-Coefficient seja a partir de 0.5 ou pelo menos 0.25.
cs_coefficients <- corStability(boot_case, statistics = 'all')
cs_coefficients

# Centralidade
centralityPlot(network,
               include = 'all',
               orderBy = 'ExpectedInfluence')

# Estabilidade Pt. 3: Acurácia das diferenças ====
# Quais arestas são estatisticamente diferentes das outras?
plot(boot_non_par, 
     statistics = "edge",
     plot = "difference",
     onlyNonZero = T,
     order = "sample")

# Diferenças nas forças
plot(boot_non_par, 
     statistics = "strength",
     plot = "difference",
     order = "sample",
     differenceShowValue = T) # mostra os valores de centralidade de cada nodo

centrality_indices <- centralityTable(network, standardized = F)
View(centrality_indices)

# Diferenças significativas entre arestas
differenceTest(boot_non_par,
               x = 'N1--N2',
               y = 'N4--N5',
               measure = "edge")

# Diferenças significativas entre nodos e centralidade
differenceTest(boot_non_par,
               "C4",
               "C1",
               measure = "strength")
