---
title: 'Aluguel de casas: fazendo predições sobre o valor de aluguel decasas.'
output:
  pdf_document: default
  html_notebook: default

---

# Importação dos dados

```r
setwd("D:\\07 04 2020\\Documents\\Edgar\\Projetos\\Data_sciece\\DataScience_projects\\Vendas_casa")
library("tidyverse")
```

```
## -- Attaching packages --------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts ------------------------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
dados<- read.csv("houses_to_rent_v2.csv", header = T)
ndados<-nrow(dados) 
View(dados)
```

# Limpeza e Preparação dos dados

\n Organização do cabeçalho para que seja possível manusear as variáveis com mais facilidade.

```r
# Exlcuindo os caracteres ..R.. que estão depois do nome da variavel.
cab<-names(dados)
cab<-cab%>% str_remove_all("..R..")
colnames(dados)<- cab
```
\n Os dados são separados em treino e teste, sendo 80% para treinar e 20%
para testar o modelo.

```r
n_treino<- (70/100)*ndados
set.seed(100)
ua<- sample(n_treino)
treino<- dados[ua,]; View(treino)
teste<- dados[-ua,]; View(teste)
```


# Treinamento do modelo

```r
model<- lm(total~.,data= treino)
```

# Avaliação da performance do modelo
\n Para a avaliação da performance do modelo foi utilizado três métricas, sendo elas erro médio absoluto, percentual médio do erro absoluto e coeficiente de determinação (R²). Também foi avaliado a distribuição dos erros dentro de quartis.

```r
predito<-predict(model,teste)
per<- teste %>% select("city", "total")%>%
  mutate(predito) %>% mutate(erro= total-predito )%>%
  mutate(erro_abs= abs(erro))%>%mutate(erro_perc= erro/total)%>% 
  mutate(erro_percabs= abs(erro_perc))
per[,c(4:7)]<-round(per[,c(4:7)], 5)

# Calculando o erro medio absoluto e percentual medio
erro_medio<- mean(per$erro_abs); erro_medio
```

```
## [1] 0.7203906
```

```r
erro_percmed<- mean(per$erro_percabs); erro_percmed
```

```
## [1] 0.000202048
```

```r
summary(per$erro_percabs)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000000 0.0000300 0.0000600 0.0002021 0.0001500 0.2589400
```


\n Cálculo do coeficiente de determinação (R²), ele varia entre 0 e 1 e indica o quão bem ajustado o modelo está, quanto mais préximo de 1 melhor a performance do modelo.

```r
# Calculando o coeficiente de determinacao
resumo<- summary(model)
r2<- resumo$adj.r.squared
performance<- data.frame(erro_medio, erro_percmed,r2)
performance
```

```
##   erro_medio erro_percmed        r2
## 1  0.7203906  0.000202048 0.9999999
```


