
# =====================================
# Código: funções auxiliares
# Especialização em Estatística, 2022/2
# Essa versão: 09/11/2022      
# =====================================


## cores customizadas para uso com o corrplot
mat.colors = 
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))


## classificando dados em quantis (equidistantes)
quant.class = function(x, c){
  # x = vetor de dados
  # c = número de classes
  
  ## declarações
  n = length(x)
  q.obj = quantile(x, probs = seq(0, 1, 1/c))
  x.cat = rep(NA, n)
  x.sort = sort(x)
  
  ## classificando
  k = 1
  x.cat[1] = paste0('(', format(round(q.obj[1], 2), nsmall = 2),
                    ', ', format(round(q.obj[2], 2), nsmall = 2), ']')
  ## loop principal
  for(i in 2:n){
    if( (x.sort[i] > q.obj[k]) & (x.sort[i] <= q.obj[k+1]) ){
      x.cat[i] = paste0('(', format(round(q.obj[k], 2), nsmall = 2),
                        ', ', format(round(q.obj[k+1], 2), nsmall = 2), ']')
    } else {
      k = k + 1
      x.cat[i] = paste0('(', format(round(q.obj[k], 2), nsmall = 2),
                        ', ', format(round(q.obj[k+1], 2), nsmall = 2), ']')
    }
  }
  
  ## reordenando valores e ordenando categorias
  x.out = factor(x.cat[order(order(x))], levels = unique(x.cat))
  
  ## saída da função
  return(x.out)
}

