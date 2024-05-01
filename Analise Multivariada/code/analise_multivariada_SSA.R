########################################################################
## Trabalho de Estatistica Multivariada - Dados da Cidade de Salvador ##
## Alunos: Felipe do Carmo, Leandro Barros e Victor Telles            ##
########################################################################

rm(list=ls(all=TRUE))

#Carregamento dos Pacotes
library(corrplot)
library(ggplot2)
library(ggspatial)
library(raster)
library(sf)
library(sp)
library(tidyverse)
library(geosphere)
library(nortest)
library(conflicted)
library(dplyr)
library(MASS)
library(mclust)
library(ks)
library(GGally)
library(RVAideMemoire)
library(QuantPsyc)
#library(caret)
#library(rgdal)
#library(ggsn)

setwd("/Users/victortelles/Documents/Coursera/Especializacao - UFMG/05 - Analise Multivariada/Trabalho")
source("code/_src/src.R")

set.seed(42)

dados = read.csv("data/SES_Salvador.csv")

# checando IDs/unidades de observação (áreas de ponderação/APs)
dados$area_de_ponderacao
class(dados$area_de_ponderacao)
dados$ID = dados$area_de_ponderacao %% 100
dados$ID
dados = dados %>% relocate(ID, .before = UF)

## carregando shapefile de SSA 
# - necessário para produzir os mapas
merge.shp = raster::shapefile(
  x = paste0('data/shapefiles/Salvador AP.shp'))
#merge.shp <- st_read('C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\Salvador AP.shp')
#str(merge.shp)
# convertendo para sf
sf.obj = st_as_sf(merge.shp)
# ordenando por AP
sf.obj = sf.obj[order(sf.obj$code), ] #COD_AREA_P]

## Parte 1 - Analise descritiva dos dados da cidade de Salvador
## Recomendações: tamanho populacional, vetor de médias e matriz de correlações dos indicadores em cada dimensões das variáveis disponíveis para a cidade escolhida. 
## Discorra sucintamente sobre os resultados.
## Opcional: faça também um mapa das áreas de ponderação.

saneamento.corr = c('P_AGUAENC','P_AGUAENCDENTRO','P_AGUAREDE', 'P_ESGOTOPUB','P_ESGOTOQUAL')
moradia.corr = c('P_MATPAREDES','P_OVERCROWDING')
emprego.corr = c('P_DESEMP','P_FORTRAB')
educacao.corr = c('P_ENSFUND','P_ENSMED','P_ENSSUP','P_FREQESCOLA')
dados[, c('ID', 'N')]
domiciliosSSA <- sum(dados$N) #866.956

domiciliosSSA
# ############################################### #
# Analise Descritiva de saneamento                #
# ############################################### #

## vetor de médias
muSaneamento = colMeans(dados[, saneamento.corr])
muSaneamento
round(as.matrix(muSaneamento), 2)#truque para mostrar colunado

# P_AGUAENC   P_AGUAENCDENTRO    P_AGUAREDE     P_ESGOTOPUB    P_ESGOTOQUAL 
# 99.25052        95.63421        98.74381        89.36727        91.84527 

## descritivas básicas
summary(dados[, saneamento.corr])

# P_AGUAENC       P_AGUAENCDENTRO     P_AGUAREDE      P_ESGOTOPUB      P_ESGOTOQUAL   
#Min.   : 84.97   Min.   : 75.84   Min.   : 82.02   Min.   : 1.471   Min.   : 12.31  
#1st Qu.: 99.33   1st Qu.: 94.52   1st Qu.: 98.80   1st Qu.:88.464   1st Qu.: 90.20  
#Median : 99.60   Median : 96.79   Median : 99.26   Median :94.886   Median : 96.37  
#Mean   : 99.25   Mean   : 95.63   Mean   : 98.74   Mean   :89.367   Mean   : 91.85  
#3rd Qu.: 99.89   3rd Qu.: 98.40   3rd Qu.: 99.64   3rd Qu.:97.878   3rd Qu.: 98.91  
#Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :99.611   Max.   :100.00 

#Agua encanada dentro de casa e esgoto público ou qualquer são variaveis com maiores valiabilidades. 
#Em geral, o acesso a agua encanada e acesso a uma rede pública de agua, apesar de não ser universal em Salvador, mas grande parte da população tem acesso.

## matriz de covariâncias
covSan = cov(dados[, saneamento.corr])
round(covSan, 2)

#                P_AGUAENC P_AGUAENCDENTRO P_AGUAREDE P_ESGOTOPUB P_ESGOTOQUAL
#P_AGUAENC            3.84            4.92       4.46       16.05        12.64
#P_AGUAENCDENTRO      4.92           18.22       5.67       50.35        43.39
#P_AGUAREDE           4.46            5.67       5.58       18.10        14.25
#P_ESGOTOPUB         16.05           50.35      18.10      245.37       206.52
#P_ESGOTOQUAL        12.64           43.39      14.25      206.52       177.01

#conforme identificado pela analise anterior, ratificado nesta. A variabilidade, agua encanada e acesso a rede de agua não possuem grandes variabilidades nas AP de SSA.
#Porém, Acesso a Rede Publica de Esgoto ou Esgoto Qualquer possuem grandes variações entre as APs de salvador e quando combinado com outras variaveis potencializa essa variabilidade


## matriz de correlações
corrSan = cor(dados[, saneamento.corr])
round(corrSan, 2)

#                P_AGUAENC P_AGUAENCDENTRO P_AGUAREDE P_ESGOTOPUB P_ESGOTOQUAL
#P_AGUAENC            1.00            0.59       0.96        0.52         0.48
#P_AGUAENCDENTRO      0.59            1.00       0.56        0.75         0.76
#P_AGUAREDE           0.96            0.56       1.00        0.49         0.45
#P_ESGOTOPUB          0.52            0.75       0.49        1.00         0.99
#P_ESGOTOQUAL         0.48            0.76       0.45        0.99         1.00

#Nota-se aqui que acesso a uma rede publica de agua e ter (ou nao) agua encanada é algo altissimo correlacionado, o mesmo se observa a Esgoto - Publico ou Qualquer.
#Agua encanada dentro de casa é altamente correlacionado a pessoa ter algum tipo de Esgoto
#Todas as demais correlações são moderadas - não havendo, para salvador correlação fraca para a dimensão esgoto!

## corrplot
corrplot(corrSan,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = TRUE,
         # número de dígitos
         number.digits = 2
)


## matriz de dispersão via GGally::ggpairs
# formatando nomes das variáveis
mX = dados[, saneamento.corr]
colnames(mX) = str_trunc(colnames(mX), 12, side = 'right')
# plot base
g = ggpairs(mX,
            lower = list(continuous = wrap('points', color = '#3177b4')),
            diag = list(continuous = wrap('barDiag', color = '#3177b4', fill = '#3177b4')),
            upper = list(continuous = wrap('points', color = '#3177b4')),
            switch = 'both'
)
# adicionando elementos do ggplot
p = g + 
  # tema
  theme(
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # facetas
    strip.placement = 'outside',
    strip.background = element_blank(),
    strip.text = element_text(size = 7, face = 'bold')
  )
print(p)

X = dados[, saneamento.corr]
# matriz transformada (formato wide)
X.wide_aux = gather(X)
X.wide = X.wide_aux
# transformando a ind. de cada variável em um fator
X.wide$key = as.factor(X.wide_aux$key)


## invocando o ggplot
p = ggplot(data = X.wide, aes(x = key, y = value)) +
  # violinos
  geom_violin(aes(fill = key), show.legend = FALSE) +
  # mudança de coordenada x <-> y
  coord_flip() +
  # reordenando o eixo x
  scale_x_discrete(limits = rev(levels(X.wide$key))) +
  # label eixo x
  xlab('') +
  # label eixo y
  ylab('') +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
print(p)

mqqnorm(X, main = "Multi-normal Q-Q Plot")
mult.norm(X)$mult.test


# ############################################### #
# Analise Descritiva de moradia                   #
# ############################################### #

muMoradia = colMeans(dados[, moradia.corr])
muMoradia
round(as.matrix(muMoradia), 2) #truque para mostrar colunado

# P_MATPAREDES P_OVERCROWDING 
#  99.205558       5.198068

## descritivas básicas
summary(dados[, moradia.corr])

#  P_MATPAREDES    P_OVERCROWDING  
#Min.   : 96.15   Min.   :0.1626  
#1st Qu.: 98.97   1st Qu.:4.0520  
#Median : 99.43   Median :5.3988  
#Mean   : 99.21   Mean   :5.1981  
#3rd Qu.: 99.76   3rd Qu.:6.9676  
#Max.   :100.00   Max.   :9.0514  

#Em geral a população de salvador tem casas com paredes de materiais duraveis, tambem observa-se um baixo, porém nao desprezivel indice de overcrownding.

## matriz de covariâncias
covMor = cov(dados[, moradia.corr])
round(covMor, 2)

#                  P_MATPAREDES P_OVERCROWDING
#P_MATPAREDES           0.67          -0.75
#P_OVERCROWDING        -0.75           5.58


#A variabilidade, independente da AP para o indice de Materiais de parede, ratificam que a população de SSA de fato tem casas com materiais duraveis.
#Nota-se que a variancia desta variavel chega a ser superior que a média


## matriz de correlações
corrMor = cor(dados[, moradia.corr])
round(corrMor, 2)

#                  P_MATPAREDES P_OVERCROWDING
#P_MATPAREDES           1.00          -0.39
#P_OVERCROWDING        -0.39           1.00

#Correlação moderada e negativa entre os indicadores desta dimensão - indicando uma maior probabilidade de overcrownding nas casas construidas com materiais NAO duraveis

## corrplot
corrplot(corrMor,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = TRUE,
         # número de dígitos
         number.digits = 2
)

## matriz de dispersão via GGally::ggpairs
# formatando nomes das variáveis
mX = dados[, moradia.corr]
colnames(mX) = str_trunc(colnames(mX), 12, side = 'right')
# plot base
g = ggpairs(mX,
            lower = list(continuous = wrap('points', color = '#3177b4')),
            diag = list(continuous = wrap('barDiag', color = '#3177b4', fill = '#3177b4')),
            upper = list(continuous = wrap('points', color = '#3177b4')),
            switch = 'both'
)
# adicionando elementos do ggplot
p = g + 
  # tema
  theme(
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # facetas
    strip.placement = 'outside',
    strip.background = element_blank(),
    strip.text = element_text(size = 7, face = 'bold')
  )
print(p)

X = dados[, moradia.corr]
# matriz transformada (formato wide)
X.wide_aux = gather(X)
X.wide = X.wide_aux
# transformando a ind. de cada variável em um fator
X.wide$key = as.factor(X.wide_aux$key)


## invocando o ggplot
p = ggplot(data = X.wide, aes(x = key, y = value)) +
  # violinos
  geom_violin(aes(fill = key), show.legend = FALSE) +
  # mudança de coordenada x <-> y
  coord_flip() +
  # reordenando o eixo x
  scale_x_discrete(limits = rev(levels(X.wide$key))) +
  # label eixo x
  xlab('') +
  # label eixo y
  ylab('') +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
print(p)

mqqnorm(X, main = "Multi-normal Q-Q Plot")
mult.norm(X)$mult.test

# ############################################### #
# Analise Descritiva de emnprego                  #
# ############################################### #
muEmprego = colMeans(dados[, emprego.corr])
muEmprego
round(as.matrix(muEmprego), 2) #truque para mostrar colunado

# P_DESEMP P_FORTRAB 
# 12.79582  67.17485 

#taxa de desemprego alta - maior que a média nacional a época - 6,7% - fonte BBC/IBGE

## descritivas básicas
summary(dados[, emprego.corr])
#      P_DESEMP       P_FORTRAB    
#Min.   : 4.18   Min.   :57.91  
#1st Qu.:10.52   1st Qu.:65.03  
#Median :13.03   Median :67.13  
#Mean   :12.80   Mean   :67.17  
#3rd Qu.:15.46   3rd Qu.:70.03  
#Max.   :19.57   Max.   :77.65 

#Alta variabilidade da taxa de desemprego chegando em alguma AP a quase 20%. 


## matriz de covariâncias
covEmp = cov(dados[,emprego.corr])
round(covEmp, 2)

#           P_DESEMP P_FORTRAB
#P_DESEMP     13.75     -2.70
#P_FORTRAB    -2.70     13.19


#ratificado o achado do item anterior, vemos uma alta variabilidade no desemprego ao longo das APs
#nota-se que isto causa uma pequena variabilidade negativa em força de trabalho. 


## matriz de correlações
corrEmp = cor(dados[, emprego.corr])
round(corrEmp, 2)

#             P_DESEMP P_FORTRAB
#P_DESEMP       1.0      -0.2
#P_FORTRAB     -0.2       1.0

#Correlação fraca e negativa entre as variáveis desta dimensão - o desemprego não tem força de influência na força de trabalho e vice versa

## corrplot
corrplot(corrEmp,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = TRUE,
         # número de dígitos
         number.digits = 2
)

## matriz de dispersão via GGally::ggpairs
# formatando nomes das variáveis
mX = dados[, emprego.corr]
colnames(mX) = str_trunc(colnames(mX), 12, side = 'right')
# plot base
g = ggpairs(mX,
            lower = list(continuous = wrap('points', color = '#3177b4')),
            diag = list(continuous = wrap('barDiag', color = '#3177b4', fill = '#3177b4')),
            upper = list(continuous = wrap('points', color = '#3177b4')),
            switch = 'both'
)
# adicionando elementos do ggplot
p = g + 
  # tema
  theme(
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # facetas
    strip.placement = 'outside',
    strip.background = element_blank(),
    strip.text = element_text(size = 7, face = 'bold')
  )
print(p)

X = dados[, emprego.corr]
# matriz transformada (formato wide)
X.wide_aux = gather(X)
X.wide = X.wide_aux
# transformando a ind. de cada variável em um fator
X.wide$key = as.factor(X.wide_aux$key)


## invocando o ggplot
p = ggplot(data = X.wide, aes(x = key, y = value)) +
  # violinos
  geom_violin(aes(fill = key), show.legend = FALSE) +
  # mudança de coordenada x <-> y
  coord_flip() +
  # reordenando o eixo x
  scale_x_discrete(limits = rev(levels(X.wide$key))) +
  # label eixo x
  xlab('') +
  # label eixo y
  ylab('') +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
print(p)

mqqnorm(X, main = "Multi-normal Q-Q Plot")
mult.norm(X)$mult.test #para emprego, a hipotese nula de normalidade não pode ser rejeitada seguindo o teste de Mardia - https://www.statology.org/multivariate-normality-test-r/

# ############################################### #
# Analise Descritiva de educacao                  #
# ############################################### #

muEducacao = colMeans(dados[, educacao.corr])
muEducacao
round(as.matrix(muEducacao), 2) #truque para mostrar colunado

#P_ENSFUND     P_ENSMED     P_ENSSUP P_FREQESCOLA 
# 66.80541     51.60251     13.70342     85.97744 

#Salvador tem um baixo indice de pessoas com ensino fundamental completo, em contra partida a frequencia escolar em cerca de 86% não parece ser um problema para a cidade

## descritivas básicas
summary(dados[, educacao.corr])
#   P_ENSFUND        P_ENSMED        P_ENSSUP        P_FREQESCOLA  
#Min.   :29.17   Min.   :12.40   Min.   : 0.5232   Min.   :73.80  
#1st Qu.:58.32   1st Qu.:39.40   1st Qu.: 3.3279   1st Qu.:83.74  
#Median :64.91   Median :47.38   Median : 6.3163   Median :86.33  
#Mean   :66.81   Mean   :51.60   Mean   :13.7034   Mean   :85.98  
#3rd Qu.:77.05   3rd Qu.:63.25   3rd Qu.:17.8407   3rd Qu.:88.78  
#Max.   :94.00   Max.   :89.54   Max.   :64.1231   Max.   :97.98  

#Todos os indicadores de completude de ensino tem um alto grau de variabilidade, com destaque para Ensino Fundamental, que 25% das AP tem mais de 40% das pessoas sem este ensino completo
#Os indices de educação, dão a dimensão do tamanho da desigualdade de salvador, não podendo ser vista tão claramente em indices de moradia e emprego.

## matriz de covariâncias
covEdu = cov(dados[,educacao.corr])
round(covEdu, 2)

#              P_ENSFUND P_ENSMED P_ENSSUP P_FREQESCOLA
#P_ENSFUND       178.41   219.62   182.80        30.37
#P_ENSMED        219.62   277.01   241.15        38.22
#P_ENSSUP        182.80   241.15   255.03        36.24
#P_FREQESCOLA     30.37    38.22    36.24        28.12

#ratificado o achado do item anterior, vemos uma alta variabilidade em todos os indicadores de completude educacional ao longo das APs, e quando eles causam variabilidade nos demais
#a frequencia escolar apesar de uma variabilidade não desprezivel, a DP é baixo quando comparado com a média, mostra que independende da formação dos pais as crianças (em 2010) em Salvador estão frequentando a escola, da mesma forma. 


## matriz de correlações
corrEdu = cor(dados[, educacao.corr])
round(corrEdu, 2)

#              P_ENSFUND P_ENSMED P_ENSSUP P_FREQESCOLA
#P_ENSFUND         1.00     0.99     0.86         0.43
#P_ENSMED          0.99     1.00     0.91         0.43
#P_ENSSUP          0.86     0.91     1.00         0.43
#P_FREQESCOLA      0.43     0.43     0.43         1.00

#alta correlação entre todos os indicadores de completude de ensino - beirando uma correlação perfeita para ensino fundamental e ensino médio. 
#A frequencia escolar tem uma correlão moderada e de mesma força com todos os indicadores de formação - 0.43, e positiva, o que indica independe da formação dos pais, e possivelmente depende de outros fatores

## corrplot
corrplot(corrEdu,
         # customizando cores
         # - ver função 'mat.colors' em "_src"
         method = 'color', col = mat.colors(200),
         # lower
         type = 'lower',
         # texto nos coeficientes
         addCoef.col = 'black',
         # cor do texto
         tl.col = 'black',
         # rotação do texto
         tl.srt = 90, 
         # cor legenda
         cl.pos = 'b',
         # correlações diagonais
         diag = TRUE,
         # número de dígitos
         number.digits = 2
)

## matriz de dispersão via GGally::ggpairs
# formatando nomes das variáveis
mX = dados[, educacao.corr]
colnames(mX) = str_trunc(colnames(mX), 12, side = 'right')
# plot base
g = ggpairs(mX,
            lower = list(continuous = wrap('points', color = '#3177b4')),
            diag = list(continuous = wrap('barDiag', color = '#3177b4', fill = '#3177b4')),
            upper = list(continuous = wrap('points', color = '#3177b4')),
            switch = 'both'
)
# adicionando elementos do ggplot
p = g + 
  # tema
  theme(
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # facetas
    strip.placement = 'outside',
    strip.background = element_blank(),
    strip.text = element_text(size = 7, face = 'bold')
  )
print(p)

X = dados[, educacao.corr]
# matriz transformada (formato wide)
X.wide_aux = gather(X)
X.wide = X.wide_aux
# transformando a ind. de cada variável em um fator
X.wide$key = as.factor(X.wide_aux$key)


## invocando o ggplot
p = ggplot(data = X.wide, aes(x = key, y = value)) +
  # violinos
  geom_violin(aes(fill = key), show.legend = FALSE) +
  # mudança de coordenada x <-> y
  coord_flip() +
  # reordenando o eixo x
  scale_x_discrete(limits = rev(levels(X.wide$key))) +
  # label eixo x
  xlab('') +
  # label eixo y
  ylab('') +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
print(p)

mqqnorm(X, main = "Multi-normal Q-Q Plot")
mult.norm(X)$mult.test #para educação rejeita-se H0, ou seja não a multinormalidade

## Parte 2 - Analise de PCA 
## Recomendações: Construa as componentes principais para cada dimensão. 
## Faça uma breve análise qualitativa dos pesos obtidos em termos das variáveis originais.

# ############################## #
# PCA: Indicadores de Saneamento #
# ############################## #

## indicadores de saneamento
# labels
lab.corr = c('P_AGUAENC', 'P_AGUAENCDENTRO',
             'P_AGUAREDE', 'P_ESGOTOPUB',
             'P_ESGOTOQUAL')


## matriz de observações
X = dta[, lab.corr]
# matriz de observações padronizadas
Z = scale(X)


## PCA
PCA = princomp(Z)
summary(PCA)
# matriz de pesos
PCA$loadings[]
# salvando os resultados em um arquivo de texto
sink(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\PCA_SANEAMENTO.txt')
print(summary(PCA))
cat('\n PCA loadings: \n')
print(PCA$loadings[])
sink()


## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figScreePlot_SANEAMENTO.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)
# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = PCA$scores[, 1]
# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `Comp. 1_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores da 1ª Componente da PCA')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
# ggsn::north(sf.obj)
print(p)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figMap_PCA_SANEAMENTO.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ########################### #
# PCA: Indicadores de Moradia #
# ########################### #

## indicadores de moradia
# labels
lab.corr = c('P_MATPAREDES', 'P_OVERCROWDING')


## matriz de observações
X = dta[, lab.corr]
# matriz de observações padronizadas
Z = scale(X)


## PCA
PCA = princomp(Z)
summary(PCA)
# matriz de pesos
PCA$loadings[]
# salvando os resultados em um arquivo de texto
sink(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\PCA_MORADIA.txt')
print(summary(PCA))
cat('\n PCA loadings: \n')
print(PCA$loadings[])
sink()


## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figScreePlot_MORADIA.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)
# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = PCA$scores[, 1]
# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `Comp. 1_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores da 1ª Componente da PCA')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figMap_PCA_MORADIA.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ########################### #
# PCA: Indicadores de Emprego #
# ########################### #

## indicadores de emprego
# labels
lab.corr = c('P_DESEMP', 'P_FORTRAB')


## matriz de observações
X = dta[, lab.corr]
# matriz de observações padronizadas
Z = scale(X)


## PCA
PCA = princomp(Z)
summary(PCA)
# matriz de pesos
PCA$loadings[]
# salvando os resultados em um arquivo de texto
sink(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\PCA_EMPREGO.txt')
print(summary(PCA))
cat('\n PCA loadings: \n')
print(PCA$loadings[])
sink()


## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figScreePlot_EMPREGO.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)
# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = PCA$scores[, 1]
# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `Comp. 1_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores da 1ª Componente da PCA')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
# ggsn::north(sf.obj)
print(p)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figMap_PCA_EMPREGO.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ############################ #
# PCA: Indicadores de Educação #
# ############################ #

## indicadores de educação
# labels
lab.corr = c('P_ENSFUND', 'P_ENSMED',
             'P_ENSSUP', 'P_FREQESCOLA')


## matriz de observações
X = dta[, lab.corr]
# matriz de observações padronizadas
Z = scale(X)


## PCA
PCA = princomp(Z)
summary(PCA)
# matriz de pesos
PCA$loadings[]
# salvando os resultados em um arquivo de texto
sink(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\PCA_EDUCACAO.txt')
print(summary(PCA))
cat('\n PCA loadings: \n')
print(PCA$loadings[])
sink()


## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figScreePlot_EDUCACAO.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)
# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = PCA$scores[, 1]
# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `Comp. 1_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores da 1ª Componente da PCA')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figMap_PCA_EDUCACAO.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ######################### #
# PCA: Todos os Indicadores #
# ######################### #

## todos os indicadores
# labels
lab.corr = c('P_AGUAENC', 'P_AGUAENCDENTRO',
             'P_AGUAREDE', 'P_ESGOTOPUB',
             'P_ESGOTOQUAL',
             'P_MATPAREDES', 'P_OVERCROWDING',
             'P_DESEMP', 'P_FORTRAB',
             'P_ENSFUND', 'P_ENSMED',
             'P_ENSSUP', 'P_FREQESCOLA')


## matriz de observações
X = dta[, lab.corr]
# matriz de observações padronizadas
Z = scale(X)


## PCA
PCA = princomp(Z)
summary(PCA)
# matriz de pesos
PCA$loadings[]
# salvando os resultados em um arquivo de texto
sink(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\PCA_TODOS.txt')
print(summary(PCA))
cat('\n PCA loadings: \n')
print(PCA$loadings[])
sink()


## scree plot
screeplot(PCA, main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser
abline(h = 1, lty = 2)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figScreePlot_TODOS.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## mapa: scores da PCA
# visualizando os 6 primeiros scores de cada componente
head(PCA$scores)
# salvando os scores da 1ª componente principal no objeto sf
sf.obj$`Comp. 1` = PCA$scores[, 1]
# classificando em quintis
sf.obj$`Comp. 1_cat` = quant.class(sf.obj$`Comp. 1`, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = `Comp. 1_cat`)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores da 1ª Componente da PCA')) +
  # legenda
  guides(fill = guide_legend('Comp. 1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.4
dev.print(file = 'C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\figMap_PCA_TODOS.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Parte 3 - Analise Fatorial Exploratória 
## Recomendações: Estime (pelo menos um) modelo EFA para cada dimensão. 
## Faça uma breve análise qualitativa das cargas fatoriais estimadas em termos das variáveis originais.
## Opcional: compare brevemente os resultados da EFA com os da PCA

lab.corr = c('P_AGUAENC', 'P_AGUAENCDENTRO',
             'P_AGUAREDE', 'P_ESGOTOPUB',
             'P_ESGOTOQUAL',
             'P_MATPAREDES', 'P_OVERCROWDING',
             'P_DESEMP', 'P_FORTRAB',
             'P_ENSFUND', 'P_ENSMED',
             'P_ENSSUP', 'P_FREQESCOLA')

## matriz de observações
X = dados[, lab.corr]

## scree plot - Pelo Scree Plot 3 ou 6 fatores são os ideais
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser - pela regra de Kaiser - 4 Fatores é o ideal
abline(h = 1, lty = 2)

EFA1 = factanal(X, factors = 1, scores = 'regression',  start = diag(5, nrow = NCOL(X), ncol = NCOL(X)))
EFA1

#Uniquenesses:
#  P_AGUAENC P_AGUAENCDENTRO      P_AGUAREDE     P_ESGOTOPUB    P_ESGOTOQUAL    P_MATPAREDES  P_OVERCROWDING        P_DESEMP 
#0.903           0.689           0.925           0.694           0.674           0.898           0.342           0.353 
#P_FORTRAB       P_ENSFUND        P_ENSMED        P_ENSSUP    P_FREQESCOLA 
#0.926           0.021           0.005           0.188           0.809 

#Loadings:
#  Factor1
#P_AGUAENC        0.311 
#P_AGUAENCDENTRO  0.558 
#P_AGUAREDE       0.274 
#P_ESGOTOPUB      0.553 
#P_ESGOTOQUAL     0.571 
#P_MATPAREDES     0.320 
#P_OVERCROWDING  -0.811 
#P_DESEMP        -0.805 
#P_FORTRAB        0.273 
#P_ENSFUND        0.990 
#P_ENSMED         0.998 
#P_ENSSUP         0.901 
#P_FREQESCOLA     0.437 

#Test of the hypothesis that 1 factor is sufficient.
#The chi square statistic is 542.47 on 65 degrees of freedom.
#The p-value is 1.74e-76 

sf.obj$Factor1 = EFA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores do 1º Fator (EFA c/ m = 1)')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)

##############################################################################
## Rodar para dimensões apartadas, para ver se da pra usar mais fatores ######
## TODOS AS TENTATIVAS COM TODOS AS VARIAVEIS DERAM ERRO!               ######
##############################################################################

#m = 1
saneamento.corr
Xsan = dados[, saneamento.corr]
## scree plot - Pelo Scree Plot 3 ou 6 fatores são os ideais
screeplot(princomp(scale(Xsan)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser - pela regra de Kaiser - 4 Fatores é o ideal
abline(h = 1, lty = 2)

EFA1 = factanal(Xsan, factors = 1, scores = 'regression',  start = diag(5, nrow = NCOL(Xsan), ncol = NCOL(Xsan)))
EFA1
#Uniquenesses:
#  P_AGUAENC P_AGUAENCDENTRO      P_AGUAREDE     P_ESGOTOPUB    P_ESGOTOQUAL 
#   0.733           0.423           0.765           0.005           0.013 

#Loadings:
#  Factor1
#P_AGUAENC       0.517  
#P_AGUAENCDENTRO 0.760  
#P_AGUAREDE      0.484  
#P_ESGOTOPUB     0.998  
#P_ESGOTOQUAL    0.993  

#Factor1
#SS loadings      3.061
#Proportion Var   0.612

#Test of the hypothesis that 1 factor is sufficient.
#The chi square statistic is 156.43 on 5 degrees of freedom.
#The p-value is 5.72e-32 

# m = 2
EFA2 = factanal(Xsan, factors = 2, scores = 'regression',  start = diag(5, nrow = NCOL(Xsan), ncol = NCOL(Xsan)))
EFA2

#factanal(x = Xsan, factors = 2, start = diag(5, nrow = NCOL(Xsan),     ncol = NCOL(Xsan)), scores = "regression")

#Uniquenesses:
#  P_AGUAENC P_AGUAENCDENTRO      P_AGUAREDE     P_ESGOTOPUB    P_ESGOTOQUAL 
#   0.005           0.360           0.067           0.011           0.005 

#Loadings:
#  Factor1 Factor2
#P_AGUAENC       0.293   0.953  
#P_AGUAENCDENTRO 0.691   0.404  
#P_AGUAREDE      0.266   0.929  
#P_ESGOTOPUB     0.962   0.252  
#P_ESGOTOQUAL    0.975   0.209  

#                Factor1 Factor2
#SS loadings      2.510   2.042
#Proportion Var   0.502   0.408
#Cumulative Var   0.502   0.910

#Test of the hypothesis that 2 factors are sufficient.
#The chi square statistic is 3.94 on 1 degree of freedom.
#The p-value is 0.0471 

## mapa: scores da EFA
# visualizando os 6 primeiros scores de cada componente
sf.obj$Factor1 = EFA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores do 1º Fator (EFA c/ m = 1)')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
  # rosa dos ventos (ggsn)
  #ggsn::north(sf.obj)
print(p)

## mapa: scores da EFA
# visualizando os 6 primeiros scores de cada componente
sf.obj$Factor2 = EFA2$scores[, 2]
# classificando em quintis
sf.obj$Factor2_cat = quant.class(sf.obj$Factor2, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor2_cat)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores do 2º Fator (EFA c/ m = 2)')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)


#m = 1
moradia.corr
Xmor = dados[, moradia.corr]
## scree plot - Pelo Scree Plot 3 ou 6 fatores são os ideais
screeplot(princomp(scale(Xmor)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser - pela regra de Kaiser - 4 Fatores é o ideal
abline(h = 1, lty = 2)

EFA1 = factanal(Xmor, factors = 1, scores = 'regression')
EFA1
#Error in factanal(Xmor, factors = 1, scores = "regression") : 
#  factor analysis requires at least three variables

#Tentativa de concatenar as dimensoes Moradia e Emprego - visto que a EFA roda apenas com 3 variaveis ou mais
lab.corr = c('P_MATPAREDES', 'P_OVERCROWDING',
             'P_DESEMP', 'P_FORTRAB')

## matriz de observações
X = dados[, lab.corr]

## scree plot - Pelo Scree Plot 3 ou 6 fatores são os ideais
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser - pela regra de Kaiser - 4 Fatores é o ideal
abline(h = 1, lty = 2)
EFA1 = factanal(X, factors = 1, scores = 'regression',  start = diag(5, nrow = NCOL(X), ncol = NCOL(X)))
EFA1

#Uniquenesses:
#  P_MATPAREDES P_OVERCROWDING       P_DESEMP      P_FORTRAB 
#   0.850          0.005          0.445          0.935 

#Loadings:
#  Factor1
#P_MATPAREDES   -0.388 
#P_OVERCROWDING  0.997 
#P_DESEMP        0.745 
#P_FORTRAB      -0.254 

#Factor1
#SS loadings      1.765
#Proportion Var   0.441

#Test of the hypothesis that 1 factor is sufficient.
#The chi square statistic is 1.36 on 2 degrees of freedom.
#The p-value is 0.507 

sf.obj$Factor1 = EFA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores do 1º Fator (EFA c/ m = 1)')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)


educacao.corr
Xedu = dados[, educacao.corr]
## scree plot - Pelo Scree Plot 3 ou 6 fatores são os ideais
screeplot(princomp(scale(Xedu)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser - pela regra de Kaiser - 4 Fatores é o ideal
abline(h = 1, lty = 2)

EFA1 = factanal(Xedu, factors = 1, scores = 'regression',  start = diag(5, nrow = NCOL(Xedu), ncol = NCOL(Xedu)))
EFA1

#factanal(x = Xedu, factors = 1, start = diag(5, nrow = NCOL(Xedu),     ncol = NCOL(Xedu)), scores = "regression")

#Uniquenesses:
#  P_ENSFUND     P_ENSMED     P_ENSSUP P_FREQESCOLA 
#   0.021        0.005        0.188        0.811 

#Loadings:
#  Factor1
#P_ENSFUND    0.989  
#P_ENSMED     0.998  
#P_ENSSUP     0.901  
#P_FREQESCOLA 0.435  

#Factor1
#SS loadings      2.975
#Proportion Var   0.744

#Test of the hypothesis that 1 factor is sufficient.
#The chi square statistic is 31.8 on 2 degrees of freedom.
#The p-value is 1.25e-07 

sf.obj$Factor1 = EFA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores do 1º Fator (EFA c/ m = 1)')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)


## Parte 4 - Explicar as diferenças de resultados obtidos partindo da mesma base de dados usando técnicas diferentes
## Recomentaçoes: Exemplifique como as diferentes regras de seleção podem afetar a escolha do número de componentes e/ou fatores em cada dimensão. 
## Opcional: faça também uma conexão com a regra de aceitação/rejeição de H0 no teste da qualidade de ajuste do(s) modelo(s) EFA ajustado. 




## Parte 5 - Construa um “índice de status socioeconômico” utilizando a PCA e/ou a EFA (utilizando pelo menos um indicador de cada dimensão). 
## Recomendação: Interprete os resultados em termos da dimensão correspondente e do padrão de “status socioeconômico” para a cidade escolhida.
## Opcional: elabore também um mapa com o índice obtido pela PCA e/ou EFA.

lab.corr = c('P_AGUAENC', 'P_AGUAENCDENTRO',
             'P_AGUAREDE', 'P_ESGOTOPUB',
             'P_ESGOTOQUAL',
             'P_MATPAREDES', 'P_OVERCROWDING',
             'P_DESEMP', 'P_FORTRAB',
             'P_ENSFUND', 'P_ENSMED',
             'P_ENSSUP', 'P_FREQESCOLA')

## matriz de observações
X = dados[, lab.corr]

## scree plot - Pelo Scree Plot 3 ou 6 fatores são os ideais
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser - pela regra de Kaiser - 4 Fatores é o ideal
abline(h = 1, lty = 2)

EFA1 = factanal(X, factors = 1, scores = 'regression',  start = diag(5, nrow = NCOL(X), ncol = NCOL(X)))
EFA1
#Loadings:
#  Factor1
#Dim - Saneamento 
#P_AGUAENC        0.311 #Remover - Carga abaixo de 0.45
#P_AGUAENCDENTRO  0.558 
#P_AGUAREDE       0.274 #Remover - Carga abaixo de 0.45
#P_ESGOTOPUB      0.553 
#P_ESGOTOQUAL     0.571 
#Dim - Moradia
#P_MATPAREDES     0.320 #Remover - Carga abaixo de 0.45
#P_OVERCROWDING  -0.811 
#Dim - Emprego
#P_DESEMP        -0.805 
#P_FORTRAB        0.273 #Remover - Carga abaixo de 0.45
#Dim - Educacao
#P_ENSFUND        0.990 
#P_ENSMED         0.998 
#P_ENSSUP         0.901 
#P_FREQESCOLA     0.437 #Remover - Carga abaixo de 0.45

lab.corr = c('P_AGUAENCDENTRO', 'P_ESGOTOPUB','P_ESGOTOQUAL',
             'P_OVERCROWDING',
             'P_DESEMP',
             'P_ENSFUND', 'P_ENSMED', 'P_ENSSUP')

## matriz de observações
X = dados[, lab.corr]

## scree plot - Pelo Scree Plot 3 ou 6 fatores são os ideais
screeplot(princomp(scale(X)), 
          main = 'Scree Plot (dashed line = Kaiser rule)', type = 'l')
# regra de Kaiser - pela regra de Kaiser - 4 Fatores é o ideal
abline(h = 1, lty = 2)

EFA1 = factanal(X, factors = 1, scores = 'regression',  start = diag(5, nrow = NCOL(X), ncol = NCOL(X)))
EFA1

#factanal(x = X, factors = 1, start = diag(5, nrow = NCOL(X),     ncol = NCOL(X)), scores = "regression")

#Uniquenesses:
#  P_AGUAENCDENTRO     P_ESGOTOPUB    P_ESGOTOQUAL  P_OVERCROWDING        P_DESEMP       P_ENSFUND        P_ENSMED        P_ENSSUP 
#       0.691           0.695           0.676           0.344           0.352           0.021           0.005           0.187 

#Loadings:
#  Factor1
#P_AGUAENCDENTRO  0.556 
#P_ESGOTOPUB      0.552 
#P_ESGOTOQUAL     0.569 
#P_OVERCROWDING  -0.810 
#P_DESEMP        -0.805 
#P_ENSFUND        0.990 
#P_ENSMED         0.998 
#P_ENSSUP         0.902 

#Factor1
#SS loadings      5.030
#Proportion Var   0.629

#Test of the hypothesis that 1 factor is sufficient.
#The chi square statistic is 334.62 on 20 degrees of freedom.
#The p-value is 6.51e-59 

sf.obj$Factor1 = EFA1$scores[, 1]
# classificando em quintis
sf.obj$Factor1_cat = quant.class(sf.obj$Factor1, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor1_cat)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores do 1º Fator (EFA c/ m = 1)')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)

EFA2 = factanal(X, factors = 2, scores = 'regression',  start = diag(5, nrow = NCOL(X), ncol = NCOL(X)))
EFA2

#factanal(x = X, factors = 2, start = diag(5, nrow = NCOL(X),     ncol = NCOL(X)), scores = "regression")

#Uniquenesses:
#  P_AGUAENCDENTRO     P_ESGOTOPUB    P_ESGOTOQUAL  P_OVERCROWDING        P_DESEMP       P_ENSFUND        P_ENSMED        P_ENSSUP 
#     0.395           0.013           0.005           0.290           0.260           0.015           0.005           0.139 

#Loadings:
#                 Factor1 Factor2
#P_AGUAENCDENTRO  0.272   0.729 
#P_ESGOTOPUB      0.150   0.982 
#P_ESGOTOQUAL     0.168   0.983 
#P_OVERCROWDING  -0.831  -0.138 
#P_DESEMP        -0.857         
#P_ENSFUND        0.867   0.483 
#P_ENSMED         0.908   0.413 
#P_ENSSUP         0.909   0.185 

#Factor1 Factor2
#SS loadings      3.954   2.924
#Proportion Var   0.494   0.366
#Cumulative Var   0.494   0.860

#Test of the hypothesis that 2 factors are sufficient.
#The chi square statistic is 35.27 on 13 degrees of freedom.
#The p-value is 0.00077 

sf.obj$Factor2 = EFA2$scores[, 1]
# classificando em quintis
sf.obj$Factor2_cat = quant.class(sf.obj$Factor2, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor2_cat)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores do 1º Fator (EFA c/ m = 2)')) +
  # legenda
  guides(fill = guide_legend('Factor1')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)

sf.obj$Factor2 = EFA2$scores[, 2]
# classificando em quintis
sf.obj$Factor2_cat = quant.class(sf.obj$Factor2, c = 5)
# invocando ggplot
p = ggplot(data = sf.obj) + 
  # raster geom
  geom_sf(aes(fill = Factor2_cat)) +
  # tema
  theme(
    # legenda
    legend.title = element_text(face = 'bold'),
    legend.key = element_blank(),
    legend.background = element_rect(colour = 'black'),
    # painéis
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    # título
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  # título
  ggtitle(paste('Scores do 2º Fator (EFA c/ m = 2)')) +
  # legenda
  guides(fill = guide_legend('Factor2')) +
  # paleta de cores
  scale_fill_brewer(palette = 'RdYlBu') +
  # barra de escala (ggspatial)
  ggspatial::annotation_scale() #+
# rosa dos ventos (ggsn)
#ggsn::north(sf.obj)
print(p)



EFA3 = factanal(X, factors = 3, scores = 'regression',  start = diag(5, nrow = NCOL(X), ncol = NCOL(X)))
EFA3

#Test of the hypothesis that 3 factors are sufficient.
#The chi square statistic is 7.32 on 7 degrees of freedom.
#The p-value is 0.397 