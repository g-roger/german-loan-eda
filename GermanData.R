---
title: "GermanData"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
---

German Data Analysis

Neste documento temos um exemplo de análise exploratória usando R 
voltado a análise bancaria.

Contexto de Negócio

No momento da realização de um empréstimo, é necessário que o banco
possa antes de realiza-lo, considerar o histórico da mesma para que
ela não se torne futuramente uma inadimplemente, cabendo assim a instituição de, em cada solicitação, tomar a decisão se esse empréstimo será concedido ou não.

Nesta análise, temos uma base de 1000 aplicações para emprestimo
que informam se a pessoa que esta solicitando possui histórico
bom ou ruim, com base em seus dados sócio-economicos.

Instalação da Bibliotecas

```{r}
#install.packages('dplyr')
library('dplyr')

#install.packages('ggplot')
library('ggplot2')

#install.packages('RColorBrewer')
library('RColorBrewer')

#install.packages("randomForest")
library("randomForest")
```

Paleta de Cores

```{r}
cores <- brewer.pal(7,"Greens")
cores
#https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
```


Leitura Base
```{r}
german_data <- read.csv("german_data.csv",sep = ';')
```

Conhecendo Estrutura
```{r}
str(german_data)
```

Top Registros
```{r}
head(german_data,10)
```


#Factor - Forma I
```{r}
german_data$Job_Factor <- as.factor(german_data$Job)
german_data$Housing_Factor <- as.factor(german_data$Housing)
german_data$Purpose_Factor <- as.factor(german_data$Purpose)
german_data$Sex_Factor <- as.factor(german_data$Sex)

str(german_data)
```

Factor - Forma II

```{r}
german_data <- german_data %>% mutate(Checking_Factor=factor(Checking.account)) %>%
  mutate(Saving_Factor=factor(Saving.accounts))

str(german_data)
```


Valores NA

Desobrindo Colunas

```{r}
german_data_null <- data.frame(colSums(is.na(german_data)))
german_data_null
colnames(german_data_null) <- c('valor_na')
german_data_null
german_data_null <- german_data_null %>% 
                      arrange(desc(valor_na)) %>%
                      filter(valor_na>0)

german_data_null
```

  
Substituindo Valores

```{r}
german_data <- german_data %>% 
  mutate(Saving.accounts=replace(Saving.accounts,is.na(Saving.accounts),'not defined')) %>%
  mutate(Credit.amount=replace(Credit.amount,is.na(Credit.amount),0)) %>%
  mutate(Checking.account=replace(Checking.account ,is.na(Checking.account),'not defined')) 

#backlog - validacao

```



Agrupamento Idades

Iremos nessa etapa atribuir labels para cada aplicação baseado na
idade do mesmo.

-Jovem - 19 -24
-Adulto - 25 - 40
-Senior - > 40

```{r}
german_data$AgeGroup <- cut(german_data$Age,breaks = c(19,25,40,75),labels=c('Young','Adult','Senior'))

german_data$AgeGroup_Factor <- as.factor(german_data$AgeGroup)

table(german_data$AgeGroup_Factor)

str(german_data$AgeGroup)
```

Estatisticas

Filtrando e avaliando as estatisticas da base, com o cuidado no detalhe de remoção do ID, dado numérico, mais não relevante para esse cenário de análise.

```{r}
german_data_num <- german_data %>% select_if(is.numeric) %>% 
  select(Age,Credit.amount,Duration)

summary(german_data_num)
```


Exploratório

Uma vez realizado o tratamento na nossa base de crédito, iremos agora fazer alguas analises visuais de modo a compreender alguns aspectos sobre a base.

Age Group

```{r}
ggplot(german_data,aes(Credit.amount)) +
  geom_histogram( fill=cores[6], bins = 30) +
  facet_grid(~AgeGroup) +
  theme_bw() +
  labs(x='Credit',y='Value') + 
  ggtitle('Credit Amount Distribution', subtitle = 'German Data Credit')
```

Sex

Tabulando Dados - Sex
```{r}
table(german_data$Sex)
```

Sex Distribution

```{r}
ggplot(german_data,aes(Sex, fill=Sex)) +
  geom_bar() +
  theme_bw() +
  labs(x='Sex',y='Value') +
  ggtitle('Sex Distribution', subtitle = 'German Data Credit')
```


Amount by Sex - Age

```{r}
ggplot(german_data,aes(Credit.amount,fill=Sex)) +
  geom_histogram(bins = 30) +
  facet_grid(~Sex) +
  theme_bw() +
  labs(x="Credit Amount",y="Density",title = "Amount by Sex")
```

Age by Sex - Age

```{r}
ggplot(german_data,aes(Age,fill=Sex)) +
  geom_histogram(bins = 30) +
  facet_grid(~Sex) +
  theme_bw() +
  labs(x="Age",y="Density",title = "Age by Sex")
```


Job

Aqui criamos um vetor de nomes dos empregos para que essa informação possa ser 
inclusa na base, que até esse momento possui apenas o ID.

```{r}
desc <- data.frame(
  c(
    'unemployed/unskilled - non-resident',
    'unskilled/resident',
    'skilled employee/official',
    'management/self-employed')
)
```


Cria Data Frame (id,desc job)

```{r}
id <- data.frame(c(1:4))

jobs_desc <- data.frame(cbind(id,desc))

names(jobs_desc)

colnames(jobs_desc) <- c('Job','Job_Desc')

names(jobs_desc)
```


Join Job

```{r}
german_data <- left_join(jobs_desc,german_data,by='Job')
unique(german_data$Job_Desc)
```

Representação Job

```{r}
german_job <- data.frame(german_data %>% group_by(Job_Desc) %>% 
                           summarise(Total=n())) %>%
              mutate(Percent=round(Total/sum(Total),3)*100) %>%
              arrange(desc(Total))
  
german_job
```

Housing

```{r}
ggplot(german_data,aes(Housing)) +
  geom_bar(fill=cores[4]) +
  theme_bw() +
  labs(x='Housing',y='Value', 
       title = 'Housing Distribution',subtitle = 'German Credit Data')
```

German Data - Correlations

Podemos também a partir da base somente com features numericas, usar os comandos
abaixo para compreender a correlação entre elas

```{r}
cor(german_data_num)
plot(german_data_num)
```


Dynamic Save

Imaginem um cenário onde durante a nossa analise desejamos ter salvos na maquina varias imagens de um mesmo plot a partir de diferente filtros aplicados, é o que
podemos fazer abaixo por meio dos comandos do ggplot.

Verificando os diferentes tipos de Housing

```{r}
Housing <- unique(german_data$Housing)
```

Usando o loop for para salvar o plot a partir dos diferentes tipos de housing.

```{r}
for(h in Housing){
  
  slice_data <- german_data %>% filter(Housing==h)
  cores <- brewer.pal(7,"Greens")
  
  plot <- ggplot(slice_data,aes(Default)) +
    geom_bar(fill=cores[4]) +
    theme_bw() +
    ggtitle('Default by Purpose ',h) +
    labs(x='Default',y='Value')
  
  
  ggsave(plot, file=paste0("plot_", h,".png"), width = 14, height = 10, units = "cm")
  
  
}
```

Implementação Clusterting

Visando aplicar técnicas de aprendizado não supervisionado, iremos aplicar em nossa base de análise de crédito a analise de cluster visando identificar padrões em comum entre as diferentes aplicações para empréstimo.

```{r}
german_data_cluster <- read.csv("german_data.csv",sep = ';')
cluster <- kmeans(german_data_cluster[,c("Age", "Credit.amount")], 3)
```


Adiciona Coluna do Centroid a Base

```{r}
german_data_cluster$cluster <- cluster$cluster
validacao <- german_data_cluster %>% select(ID,Age,cluster,Credit.amount)
head(validacao,5)
```

Gráfico Cluster

```{r}
plot(Age~Credit.amount,data=german_data_cluster,pch=19,
     col=cluster,
     main="K-Means - Age x Credit Amount",
     xlab="Credit Amount",
     ylab="Age")
```


Propiedades do Cluster

Centros
```{r}
head(cluster$cluster,20)
```


Tamanhos 
```{r}
table(cluster$cluster)
```

Classificador

Por fim, vamos agora aplicar um algoritmo de classificador sobre a nossa base de crédito de modo a determinar de uma determinada aplicação deve ter o seu crédito 
aprovado ou não.



Treino e Teste

```{r}
index <- sample.int(nrow(german_data), 0.8 * nrow(german_data))

train_data <- german_data[index,]
test_data <- german_data[-index,]
```


Linhas Treino

```{r}
nrow(train_data)
```


Linhas Teste
```{r}
nrow(test_data)
```

Modelo (Random Forest)

)
```{r}
mod <- randomForest(Default~Housing_Factor+Job_Factor+Purpose_Factor+AgeGroup_Factor,
                    data=train_data,na.action = na.roughfix)

head(predict(mod, test_data, type = "class"),5)
```

Validação em Teste (Predict)

```{r}
Scored <- predict(mod,test_data)
head(Scored,5)
```

Adaptação Validação - 0 ou 1

```{r}
Scored <- ifelse(predict(mod,test_data)<0.7,0,1)
head(Scored,5)
```

Matriz de Confusão
```{r}
table(Scored,test_data$Default)
```


Validação Resultado
```{r}
resultado_rf <- data.frame(cbind(test_data$Default,Scored))
colnames(resultado_rf) <- c('Default','Scored')
head(resultado_rf,5)
```

Calculando os Indices - Matriz de Confusão

```{r}
tp <- nrow(data.frame(resultado_rf %>% filter(Default==0) %>% filter(Scored==0)))
tn <- nrow(data.frame(resultado_rf %>% filter(Default==1) %>% filter(Scored==1)))
fp <- nrow(data.frame(resultado_rf %>% filter(Default==0) %>% filter(Scored==1)))
fn <- nrow(data.frame(resultado_rf %>% filter(Default==1) %>% filter(Scored==0)))
```

#Calculando Métricas

Accuracy (Acerto Total - TP+TN/Total)
```{r}
round((tp+tn)/nrow(test_data),2)
```


Precision (Acerto das Corretas - TP/TP+FP)
```{r}
round(tp/(tp+fp),2)
```


Sensitivity (Recall - TP / (TP + FN).
```{r}
round(tp/(tp+fn),2)
```