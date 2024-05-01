
# =====================================
# Módulo #2. EST582 – Est. Mult. Comp.
# Ex. 4 – PCA (Exemplo Prático)
# Especialização em Estatística, 2023/2
# Essa versão: 25/10/2023      
# =====================================




# ############ #
# Preliminares #
# ############ #

## limpando o workspace
rm(list = ls())


## carregando pacotes necessários
## Nota: aqui podemos usar tanto 'library' quanto 'require'
# matrizes de correlação
library(corrplot)
# gráficos diversos
library(ggplot2)
# mapas e shapefiles
library(ggspatial)
library(ggsn)
library(raster)
library(rgdal)
library(sf)
library(sp)
# manipulação de bases de dados
library(tidyverse)


## mudando diretório de trabalho
## Nota: lembre-se de sempre alterar esse caminho!
setwd("C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA")


## carregando funções auxiliares
source("C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\src.R")


## carregando dados: SES Belo Horizonte
## - indicadores do censo por *áreas de ponderação*
## - os dados são carregados no objeto "dta"
dta = read.csv("C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\SES_Salvador.CSV")


## visualizando os dados
## (e checando para ver se está tudo OK)
View(dta)
# checando IDs/unidades de observação (áreas de ponderação/APs)
dta$area_de_ponderacao
class(dta$area_de_ponderacao)
# se as APs parecem ser as mesmas: 
# - aumentar a precisão do display
# - o padrão/default é options(digits = 7)
options(digits = 16)
dta$area_de_ponderacao
# retornando ao padrão
options(digits = 7)
# gerando IDs
# - os IDs são os 2 últimos dígitos das APs
dta$ID = dta$area_de_ponderacao %% 100
dta$ID
# rearranjando colunas
# - usando o 'pipe operator'
dta = dta %>% relocate(ID, .before = UF)
# - código equivalente (s/ o operador)
# dta = relocate(ID, .data = dta, .before = UF)


## carregando shapefile de BH 
# - necessário para produzir os mapas
merge.shp = raster::shapefile(
  x = paste0('C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\Salvador AP.shp'))
#merge.shp <- st_read('C:\\Users\\felipe\\Documents\\trabalho_est\\MULTIVARIADA\\Salvador AP.shp')
#str(merge.shp)
# convertendo para sf
sf.obj = st_as_sf(merge.shp)
# ordenando por AP
sf.obj = sf.obj[order(sf.obj$code), ] #COD_AREA_P]




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

