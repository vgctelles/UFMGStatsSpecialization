#Analise Exploratoria de Series Temporais
# =====================================
# Trabalho Final de Series Temporais
# Aluno: Victor Telles
# Especialização em Estatística, 2024/1
# Essa versão: 06/04/2024      
# =====================================

library(corrplot)
library(ggplot2)
require(car) # pacote car - para analise de colinearidade entre variavies 
#require(rgl) # pacote rgl
library(tidyverse)
library(ks)
library(tseries)
library(moments)
library(stats)
library(forecast)
library(astsa)
library(dlm)

#Definicao do Ambiente de Trabalho
setwd("/Users/victortelles/Documents/Coursera/Especializacao - UFMG/06 - Analise de Series Temporais/Trabalho Pratico Final/serie2")

#Leitura e checagem
stEmp <- read.csv("SerieIEF.csv", sep=";", header = T)
nEmp = length(stEmp$yt)
nEmp

ts.plot(stEmp$yt, xlab = "tempo", ylab = "índice de emprego formal em MG")

#definindo a Serie Tempotal - usando o pacote tseries 
tsEmp <- ts(stEmp$yt, start = c(1999,4), frequency = 12)
tsEmp
plot(tsEmp)

#Analise Exploratoria da Série Temporal
#Coeficiente de Variação da Serie
cv = sd(tsEmp)/mean(tsEmp)
cv #em media a serie varia em 4,7%

skewness(tsEmp) #0.8733008 simetria em torno da média - valor de referencia é 0
kurtosis(tsEmp) #3.275672 tem a ver com a possibilidade de gerar bastante valores nas caldas para valores de Curtoses alto - valor de referencia é 3

summary(tsEmp)  # Calcula algumas estatisticas descritivas para a serie.
boxplot(tsEmp) # um boxplot

fatorSazonal=rep(4:12,1)
fatorSazonal2=rep(1:12,4)
fatorSazonal3=rep(1:8,1)
fatorSazo = append(fatorSazonal, fatorSazonal2)
fatorSazo = append(fatorSazo, fatorSazonal3)

boxplot(tsEmp~fatorSazo,xlab="Meses",ylab="índice de emprego formal em MG") #observando o boxplot mensal, é possivel observar as diferenças pluviometricas ao longo do ano
hist(tsEmp) #ratifica-se o valor de curtose, observando caldas longas
qqnorm(tsEmp); qqline(tsEmp) #observando o qqplot, pode-se dizer que os dados não respeitam a distribuição normal
shapiro.test(tsEmp) 
#Shapiro-Wilk normality test - Ha evidencias para rejeitar H0 (H0: de que os dados sao normalmente distribuidos)
#data:  tsEmp
#W = 0.92401, p-value = 0.0006615


#Analise de Autocorrelação
acf(tsEmp) #Observa-se que a serie tem memoria longa - alem de vermos as ondas tradicionais de sazonalidade 
pacf(tsEmp) #Observa-se que a ordem 1 ultrapassa as bandas limites

#Teste de Memoria da Serie
Box.test(tsEmp, lag = 1, type="Ljung-Box")#p-value = 7.76e-14 - ha evidencias para rejeitar H0 (H0: o conjunto de correlações de ordem 1 é igual zero)
Box.test(tsEmp, lag = 6, type="Ljung-Box")  #p-value = 2.2e-16 - ha evidencias para rejeitar H0 (H0: o conjunto de correlações de ordem 6 é igual zero) ou seja possui memória de mais de 6 meses
Box.test(tsEmp, lag = 12, type="Ljung-Box") #p-value = 2.2e-16 - ha evidencias para rejeitar H0 (H0: o conjunto de correlações de ordem 12 é igual zero) ou seja possui memória de mais de 12 meses
Box.test(tsEmp, lag = 24, type="Ljung-Box") #p-value = 2.2e-16 - ha evidencias para rejeitar H0 (H0: o conjunto de correlações de ordem 24 é igual zero) ou seja possui memória de mais de 24 meses
Box.test(tsEmp, lag = 36, type="Ljung-Box") #p-value = 2.2e-16 - ha evidencias para rejeitar H0 (H0: o conjunto de correlações de ordem 36 é igual zero) ou seja possui memória de mais de 36 meses


#Inicio da Modelagem

t = c(1:nEmp)

modelo1 <- lm(tsEmp ~ t)
summary(modelo1)
AIC(modelo1) #[1] 281.6922

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.2653 -1.4309 -0.3491  1.0658  5.9786 
#Coefficients:
#           Estimate  Std. Error t value Pr(>|t|)    
#(Intercept) 89.37481    0.51424  173.80   <2e-16 ***
#  t          0.21149    0.01355   15.61   <2e-16 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 2.049 on 63 degrees of freedom
#Multiple R-squared:  0.7946,	Adjusted R-squared:  0.7913 
#F-statistic: 243.7 on 1 and 63 DF,  p-value: < 2.2e-16

plot(modelo1)
shapiro.test(modelo1$residuals) 
#Shapiro-Wilk normality test
#data:  modelo1$residuals
#W = 0.95311, p-value = 0.01517 - Ha evidencias para rejeitar H0 (H0: os residuos sao normalmente distribuidos)

durbinWatsonTest(modelo1)
#lag Autocorrelation D-W Statistic p-value
# 1       0.8390991     0.1780147       0
#Alternative hypothesis: rho != 0
#Há evidencias para rejeitar-se H0 - H0: nao ha auto-correlacao de ordem 1
acf(modelo1$residuals) # ratifica-se o valor observado no teste de Durbin-Watson - ve-se autocorrelacoes significantes 

#Modelo 2 - Ajuste para Tendencia
t2=t^2
modelo2 <- lm(tsEmp ~ t + t2)
summary(modelo2)
AIC(modelo2) #[1] 243.615 - reduzido em relacao ao modelo1 

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.42808 -1.23632 -0.04589  1.18878  3.05655 

#Coefficients:
#             Estimate  Std. Error t value Pr(>|t|)    
# (Intercept) 92.5795009  0.5825264 158.928  < 2e-16 ***
# t           -0.0755014  0.0407283  -1.854   0.0685 .  
# t2           0.0043483  0.0005981   7.270 7.31e-10 ***
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 1.518 on 62 degrees of freedom
#Multiple R-squared:  0.8891,	Adjusted R-squared:  0.8856 
#F-statistic: 248.6 on 2 and 62 DF,  p-value: < 2.2e-16

plot(modelo2)
shapiro.test(modelo2$residuals) 
#Shapiro-Wilk normality test
#data:  modelo1$residuals
#W = 0.96047, p-value = 0.03629 - Ha evidencias para rejeitar H0 (H0: os residuos sao normalmente distribuidos)

durbinWatsonTest(modelo2)
#lag Autocorrelation D-W Statistic p-value
#  1       0.8044943     0.3116911       0
#Alternative hypothesis: rho != 0
#Há evidencias para rejeitar-se H0 - H0: nao ha auto-correlacao de ordem 1
acf(modelo2$residuals) # ratifica-se o valor observado no teste de Durbin-Watson - ve-se autocorrelacoes significantes 

#Modelo 3 - Inclusao do componente  de sazonalidade
sazon1=rep.int(4:12,1)
sazon2=rep.int(1:12,4)
sazon3=rep.int(1:8,1)
sazon = append(sazon1, sazon2)
sazon = append(sazon, sazon3)
modelo3 <- lm(tsEmp ~ t + t2 + factor(sazon))
summary(modelo3)
AIC(modelo3) #[1] 128.5813 - reduzido em relacao ao modelo1 

#Residuals:
#      Min       1Q   Median       3Q      Max 
#-1.21429 -0.36498 -0.04533  0.33957  1.69858 

#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#. (Intercept)     90.3224275  0.3473353 260.044  < 2e-16 ***
#  t               -0.0352069  0.0159769  -2.204 0.032093 *  
#  t2               0.0037497  0.0002349  15.966  < 2e-16 ***
#  factor(sazon)2   0.1964757  0.3688266   0.533 0.596552    
#. factor(sazon)3   0.2054519  0.3688916   0.557 0.580002    
#  factor(sazon)4   0.8072789  0.3547153   2.276 0.027086 *  
#  factor(sazon)5   2.1062529  0.3545473   5.941 2.54e-07 ***
#  factor(sazon)6   3.5810608  0.3544634  10.103 9.09e-14 ***
#  factor(sazon)7   3.8483693  0.3544627  10.857 7.26e-15 ***
#  factor(sazon)8   3.5248450  0.3545462   9.942 1.57e-13 ***
#  factor(sazon)9   2.5991027  0.3691354   7.041 4.69e-09 ***
#  factor(sazon)10  1.9455762  0.3689891   5.273 2.75e-06 ***
#  factor(sazon)11  1.5445503  0.3688869   4.187 0.000112 ***
#  factor(sazon)12  0.1360249  0.3688260   0.369 0.713800  
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.5831 on 51 degrees of freedom
#Multiple R-squared:  0.9865,	Adjusted R-squared:  0.9831 
#F-statistic: 287.4 on 13 and 51 DF,  p-value: < 2.2e-16

plot(modelo3)
shapiro.test(modelo3$residuals) 
#Shapiro-Wilk normality test
#data:  modelo1$residuals
#W = 0.97801, p-value = 0.2995 - nao ha evidencias para rejeitar H0 (H0: os residuos sao normalmente distribuidos)

durbinWatsonTest(modelo3)
#lag Autocorrelation D-W Statistic p-value
#  1       0.7918974      0.249839       0
#Alternative hypothesis: rho != 0
#Há evidencias para rejeitar-se H0 - H0: nao ha auto-correlacao de ordem 1
acf(modelo3$residuals) # ratifica-se o valor observado no teste de Durbin-Watson - ve-se autocorrelacoes significantes 

#Modelo 4 - Inclusao do termo autoregressivo de ordem 1

resid_1=rep(0,nEmp) 
for(i in 2:nEmp)
  resid_1[i]=modelo3$residuals[i-1]
modelo4= lm(tsEmp ~ t + t2 + factor(sazon) + resid_1)
summary(modelo4)
AIC(modelo4) #[1] 22.53605 - reduzido em relacao ao modelo1 

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.06493 -0.15777  0.03491  0.16182  0.52970 

#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#. (Intercept)     92.8718702  0.2987762 310.841  < 2e-16 ***
#  t               -0.0737479  0.0093833  -7.859 2.75e-10 ***
#  t2               0.0043836  0.0001403  31.240  < 2e-16 ***
#  factor(sazon)2  -1.0988220  0.2328220  -4.720 1.94e-05 ***
#  factor(sazon)3  -1.1417858  0.2352627  -4.853 1.23e-05 ***
#  factor(sazon)4  -0.6375804  0.2335407  -2.730  0.00872 ** 
#  factor(sazon)5   0.4788691  0.2431360   1.970  0.05444 .  
#  factor(sazon)6   0.9155860  0.3093981   2.959  0.00470 ** 
#  factor(sazon)7  -0.0126520  0.3998281  -0.032  0.97488    
#. factor(sazon)8  -0.4513772  0.4090247  -1.104  0.27507    
#. factor(sazon)9  -0.6263657  0.3549605  -1.765  0.08374 .  
#. factor(sazon)10 -0.6310521  0.3082548  -2.047  0.04591 *  
#  factor(sazon)11 -0.3229598  0.2628596  -1.229  0.22496    
#. factor(sazon)12 -1.2484233  0.2370013  -5.268 2.93e-06 ***
#  resid_1          0.8947958  0.0811241  11.030 5.36e-15 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.3178 on 50 degrees of freedom
#Multiple R-squared:  0.9961,	Adjusted R-squared:  0.995 
#F-statistic:   907 on 14 and 50 DF,  p-value: < 2.2e-16

plot(modelo4)
shapiro.test(modelo4$residuals) 
#Shapiro-Wilk normality test
#data:  modelo1$residuals
#W = 0.95824, p-value = 0.02778 - Ha evidencias para rejeitar H0 (H0: os residuos sao normalmente distribuidos)

durbinWatsonTest(modelo4)
#lag Autocorrelation D-W Statistic p-value
#  1       0.2929212      1.134103       0
#Alternative hypothesis: rho != 0
#Há evidencias para rejeitar-se H0 - H0: nao ha auto-correlacao de ordem 1
acf(modelo4$residuals) # ratifica-se o valor observado no teste de Durbin-Watson - ve-se autocorrelacoes significantes

#modelo5 - serie Transformada
lnEmp = log(tsEmp)

modelo5= lm(lnEmp ~ t + t2 + factor(sazon))
summary(modelo5)
AIC(modelo5) #[1] -476.7235 - reduzido em relacao ao modelo1 

#Residuals:
#Min        1Q    Median        3Q       Max 
#-0.011754 -0.003275 -0.001043  0.003426  0.013831 

#Coefficients:
#                   Estimate Std. Error  t value Pr(>|t|)    
#. (Intercept)      4.503e+00  3.301e-03 1364.326  < 2e-16 ***
#  t               -2.003e-04  1.518e-04   -1.319   0.1929    
#. t2               3.601e-05  2.232e-06   16.138  < 2e-16 ***
#  factor(sazon)2   2.116e-03  3.505e-03    0.604   0.5488    
#. factor(sazon)3   2.232e-03  3.505e-03    0.637   0.5271    
#. factor(sazon)4   8.636e-03  3.371e-03    2.562   0.0134 *  
#  factor(sazon)5   2.209e-02  3.369e-03    6.555 2.74e-08 ***
#  factor(sazon)6   3.711e-02  3.368e-03   11.017 4.29e-15 ***
#  factor(sazon)7   3.968e-02  3.368e-03   11.779 3.63e-16 ***
#  factor(sazon)8   3.621e-02  3.369e-03   10.749 1.04e-14 ***
#  factor(sazon)9   2.727e-02  3.508e-03    7.773 3.30e-10 ***
#  factor(sazon)10  2.040e-02  3.506e-03    5.818 3.96e-07 ***
#  factor(sazon)11  1.620e-02  3.505e-03    4.621 2.63e-05 ***
#  factor(sazon)12  1.421e-03  3.505e-03    0.405   0.6869    
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.005541 on 51 degrees of freedom
#Multiple R-squared:  0.9883,	Adjusted R-squared:  0.9853 
#F-statistic: 331.4 on 13 and 51 DF,  p-value: < 2.2e-16

plot(modelo5)
shapiro.test(modelo5$residuals) 
#Shapiro-Wilk normality test
#data:  modelo1$residuals
#W = 0.98798, p-value = 0.7831 - Ha evidencias para rejeitar H0 (H0: os residuos sao normalmente distribuidos)

durbinWatsonTest(modelo5)
#lag Autocorrelation D-W Statistic p-value
#  1       0.2929212      1.134103       0
#Alternative hypothesis: rho != 0
#Há evidencias para rejeitar-se H0 - H0: nao ha auto-correlacao de ordem 1
acf(modelo5$residuals) # ratifica-se o valor observado no teste de Durbin-Watson - ve-se autocorrelacoes significantes


#Modelo 6 - Termo autoregressivo na serie transformada

resid_1=rep(0,nEmp) 
for(i in 2:nEmp)
  resid_1[i]=modelo5$residuals[i-1]
modelo6= lm(lnEmp ~ t + t2 + factor(sazon) + resid_1)
summary(modelo6)
AIC(modelo6) #[1] -575.4465 - reduzido em relacao ao modelo1 

#Residuals:
#Call:
#  lm(formula = lnEmp ~ t + t2 + factor(sazon) + resid_1)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.005222 -0.001443  0.000167  0.001492  0.004962 

#Coefficients:
#                 Estimate Std. Error  t value Pr(>|t|)    
#. (Intercept)      4.504e+00  1.537e-03 2930.595  < 2e-16 ***
#  t               -2.929e-04  7.098e-05   -4.127 0.000139 ***
#  t2               3.774e-05  1.046e-06   36.071  < 2e-16 ***
#  factor(sazon)2   2.089e-03  1.631e-03    1.281 0.206195    
#. factor(sazon)3   2.176e-03  1.631e-03    1.334 0.188363    
#. factor(sazon)4   8.466e-03  1.569e-03    5.397 1.86e-06 ***
#  factor(sazon)5   2.190e-02  1.568e-03   13.967  < 2e-16 ***
#  factor(sazon)6   3.690e-02  1.568e-03   23.540  < 2e-16 ***
#  factor(sazon)7   3.945e-02  1.568e-03   25.164  < 2e-16 ***
#  factor(sazon)8   3.596e-02  1.568e-03   22.933  < 2e-16 ***
#  factor(sazon)9   3.002e-02  1.645e-03   18.252  < 2e-16 ***
#  factor(sazon)10  2.046e-02  1.632e-03   12.536  < 2e-16 ***
#  factor(sazon)11  1.624e-02  1.631e-03    9.956 1.87e-13 ***
#  factor(sazon)12  1.444e-03  1.631e-03    0.885 0.380261    
#. resid_1          9.718e-01  7.136e-02   13.619  < 2e-16 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.002579 on 50 degrees of freedom
#Multiple R-squared:  0.9975,	Adjusted R-squared:  0.9968 
#F-statistic:  1434 on 14 and 50 DF,  p-value: < 2.2e-16

plot(modelo6)
shapiro.test(modelo6$residuals) 
#Shapiro-Wilk normality test
#data:  modelo1$residuals
#W  = 0.98736, p-value = 0.7498 - nao ha evidencias para rejeitar H0 (H0: os residuos sao normalmente distribuidos)

durbinWatsonTest(modelo6)
#lag Autocorrelation D-W Statistic p-value
#  1       0.2929212      1.134103       0
#Alternative hypothesis: rho != 0
#Há evidencias para rejeitar-se H0 - H0: nao ha auto-correlacao de ordem 1
acf(modelo6$residuals) # ratifica-se o valor observado no teste de Durbin-Watson - ve-se autocorrelacoes significantes

# Ajuste do modelo sem as 9 ultimas observacoes
real=lnEmp[57:65]

n6=length(lnEmp)-9 # Coloca em n o tamanho da serie menos 12 observacoes 
serie=rep(0,n6) # Cria a variavel serie
for(i in 1:n6)
  serie[i]=lnEmp[i] # Coloca na variavel serie a serie transformada por LN
serie=ts(serie,start=c(1999,04),frequency=12)
t=c(1:n6)
t2=t^2

sazon1=rep.int(4:12,1)
sazon2=rep.int(1:12,3)
sazon3=rep.int(1:11,1)
sazon6 = append(sazon1, sazon2)
sazon6 = append(sazon6, sazon3)

M5=lm(serie ~ t + t2+ factor(sazon6)) 
resid_m5=rep(0,n6) 
for(i in 2:n6)
  resid_m5[i]=M5$res[i-1] 
M6= lm(serie ~ t + t2 + factor(sazon6) + resid_m5)
summary(M6)

# Previsao
previsao = rep(1,9) 
previsao[1] = M6$coef[1] + M6$coef[2]*(n6+1) + M6$coef[3]*(n6+1)^2 + M6$coef[14] + M6$coef[15]*M5$res[n6]
previsao[2] = M6$coef[1] + M6$coef[2]*(n6+2) + M6$coef[3]*(n6+2)^2 + M6$coef[15]*M5$res[n6]


for(i in 3:9)
  previsao[i] = M6$coef[1] + M6$coef[2]*(n6+i) + M6$coef[3]*(n6+i)^2 + M6$coef[i+1] + (M6$coef[15])^i*M6$res[n6]


previsao
exp(previsao)
previsao=ts(previsao,start = c(2003,12), frequency = 12)
plot(exp(previsao))
real=ts(real,start = c(2003,12), frequency = 12)
plot(exp(real))

previsao
real

# Intervalos de previsao
li=rep(1,9)
ls=rep(1,9)
previsaofinal=rep(1,9)
p=length(M6$coef)

PHI=1
for(i in 1:9)
{
  li[i]=previsao[i] - qt(0.975,n6-p)*sd(M6$res)*sqrt(PHI)
  ls[i]=previsao[i] + qt(0.975,n6-p)*sd(M6$res)*sqrt(PHI)
  PHI=PHI+(M6$coef[15])^2*i
}
li
for(i in 1:9)
{
  previsaofinal[i] = exp(previsao[i])
  li[i]=exp(li[i])
  ls[i]=exp(ls[i])
}
length(previsaofinal)
length(real)
vetor_prev = cbind(exp(real),previsaofinal, li, ls)
vetor_prev

# Calculo do Erro Quadratico Medio de Previsao

for(i in 1:9)
  SQP=(exp(real[i]) - previsaofinal[i])^2/9
SQP ##[1] 0.6325209

## Fazer o MSE para comparaçao

#Tecnica ARIMA(p, q, d)

#Teste de Raiz Unitaria
pp.test(lnEmp) # p-value = 0.5413 nao tem evidencias para rejeitar a hipotese nula H0 (H0: a série tem raiz unitária, logo é estacionária)
adf.test(lnEmp)  #p-value = 0.03811 rejeita-se a hipótese nula H0 (H0: a série não é estacionaria)
#seguindo o teste Aumentado de Dickey-Fuller, A serie é estacionaria, entao necessita da componente Diferencial do ARIMA.


pacf(lnEmp) #observando o pacf, observa-se que ultrapassa-se a banda em ordem 1 - ou seja, sera necessario um componente auto-regressivo 1

Box.test(lnEmp,lag=1,type="Ljung-Box")
Box.test(lnEmp,lag=6,type="Ljung-Box")
Box.test(lnEmp,lag=12,type="Ljung-Box")
Box.test(lnEmp,lag=24,type="Ljung-Box")
Box.test(lnEmp,lag=36,type="Ljung-Box")


fit1 <- sarima(lnEmp,1,1,1,1,1,1,6)
fit1

#Coefficients:
#  ar1      ma1     sar1     sma1
#0.7912  -0.2890  -0.9040  -0.5544
#s.e.  0.1807   0.2567   0.0516   0.2138

#sigma^2 estimated as 1.246e-05:  log likelihood = 236.62,  aic = -463.24

#$ttable
#     Estimate   SE    t.value p.value
#ar1    0.7912 0.1807   4.3797  0.0001
#ma1   -0.2890 0.2567  -1.1258  0.2652
#sar1  -0.9040 0.0516 -17.5229  0.0000
#sma1  -0.5544 0.2138  -2.5935  0.0122

#ma1 nao representativo, demais componentes respeitam as condicoes e sao representativos


#Mantendo a mesma condicao de sazonalidade e removendo o componente media movel 1 (ma1) do modelo - indicado pelos resultados acima
fit2 <- sarima(lnEmp,1,1,0,1,1,1,6)
fit2
shapiro.test(fit2$fit$residuals)#p-value = 0.06214 - nao ha evidencias para rejeitar a hipotese nula de que os residuos sao normalmente distribuidos
#Coefficients:
#  ar1     sar1     sma1
#0.5956  -0.8863  -0.5848
#s.e.  0.1249   0.0539   0.2012

#sigma^2 estimated as 1.271e-05:  log likelihood = 236.1,  aic = -464.2

#$ttable
#Estimate     SE  t.value p.value
#ar1    0.5956 0.1249   4.7683  0.0000
#sar1  -0.8863 0.0539 -16.4451  0.0000
#sma1  -0.5848 0.2012  -2.9066  0.0053

#observa-se melhora no AIC, aumento dos graus de liberdade e menor complexidade do modelo - este deve ser usado para simulacao
#melhor modelo 

#Previsao 
serie
serieSarima <- sarima.for(serie, 9, 1, 1, 0, 1, 1, 1, 6)
serieSarima$pred
exp(serieSarima$pred)

#Forecasting MSE:
for(i in 1:9)
  SQP=(exp(real[i]) - exp(serieSarima$pred[i]))^2/9
SQP ##[1] 2.61274

#Tecnica de Alisamento Exponencial
#Dado tudo que foi visto ate o momento, pode-se afirmar que a serie contem uma componente de Nivel, de Tendencia e Sazonalidade, logo


fitHT <- HoltWinters(lnEmp)
plot(fitted(fitHT))
plot(fitHT)

fitHT2 <- HoltWinters(serie)
plot(fitted(fitHT2))
plot(fitHT2) 

predHT2 = predict(fitHT2,9, prediction.interval = TRUE)
exp(predHT2)
exp(real)
plot(exp(predHT2))
plot(exp(real))


for(i in 1:9)
  SQP=(exp(real[i]) - exp(predHT2[i,1]))^2/9
SQP ##[1]1.77749

#FIM TECNICA DE SUAVIZACAO EXPONENCIAL

#Tecnica de Espaço-Estados 
lnEmp
model<-function(u){
  mod<-dlmModSeas(frequency=12,dV=0,dW=c(exp(u[4]),rep(0,10)))+
    dlmModPoly(2,dV=exp(u[1]),dW=(exp(u[2:3])))
}
outmle=dlmMLE(tsEmp,parm=rep(0,4),model)
exp(outmle$par)
mod=model(outmle$par)
outmodFil=dlmFilter(tsEmp,mod)

outF<-dlmFilter(tsEmp,mod)    #Filtering
outS<-dlmSmooth(tsEmp,mod)    #smoothing

par(mfrow=c(4,1))
ts.plot((outF$m[10:nEmp,1]))
title("Sazonality: Filtering")
ts.plot((outF$m[10:nEmp,12]))
title("Slope: Filtering")
ts.plot((outF$m[10:nEmp,13]))
title("Level: Filtering")
ts.plot((outF$f[10:nEmp]),ylab="Yt^",xlab="t")
title("Preditos")

par(mfrow=c(1,1))
myt=matrix(NA,nEmp,2)
myt[,1]=tsEmp
myt[,2]=outF$f[1:nEmp]
ts.plot(myt,ylab="Yt^",xlab="t",col=c("black","red"))
#observa-se um descasamento nos preditos em momentos iniciais, depois o modelo ajusta para as observações

ts.plot((outS$s[10:nEmp,1]))
title("Sazonality: Smoothing")
ts.plot((outS$s[10:nEmp,12]))
title("Slope: Smoothing")
ts.plot((outS$s[10:nEmp,13]))
title("Level: Smoothing")

#Análise dos Resíduos
par(mfrow=c(1,1))
qqnorm(residuals(outF,sd=FALSE))
qqline(residuals(outF,sd=FALSE))
 
tsdiag(outF)
shapiro.test(residuals(outF,sd=FALSE)) # p-value = 0.009997 - Ha evidencias para rejeitar H0 (H0: os residuos do modelo são normalmente distribuidos)
ArchTest(residuals(outF,sd=FALSE))

# Previsao

seriePura = exp(serie)
seriePura

outmle2=dlmMLE(seriePura,parm=rep(0,4),model)
exp(outmle2$par)
mod2=model(outmle2$par)
outmodFil2=dlmFilter(seriePura,mod2)

outF2<-dlmFilter(seriePura,mod2)    #Filtering
outS2<-dlmSmooth(seriePura,mod2)    #smoothing

prev2=dlmForecast(outmodFil2,n=9)
prev2$f
previsaoMEB=as.numeric(prev2$f)
lsMEB=previsaoMEB+1.96*sqrt(as.numeric(prev2$Q[1:9]))
liMEB=previsaoMEB-1.96*sqrt(as.numeric(prev2$Q[1:9]))
#dfPrev <- data.frame(exp(real),predp,predli,predls)
vetorPrevMEB = cbind(exp(real),previsaoMEB, liMEB, lsMEB)

#Forecasting MSE:
for(i in 1:9)
  SQP=(exp(real[i]) - previsaoMEB[i])^2/9
SQP  ##[1]0.7514231
## A previsão obteve bons resultados com a Media do erro quadratico menor que 1
