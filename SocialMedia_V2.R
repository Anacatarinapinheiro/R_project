library(ggplot2) 
library(dplyr)    
library(factoextra) 
#install.packages("mice")
library(mice)
library(leaps)
library(C50)
library(pROC)
library(stats)
#install.packages("corrgram")
library(corrplot)
library(corrgram)
#install.packages("psych")
library(psych)
#install.packages("GPArotation")
library(GPArotation)
#install.packages("coin")
library(coin)
#install.packages("rpart.plot")
library(rpart.plot)



social.o <-read.csv("C:/Users/ana_c/Documentos PC/Análise de Dados/PROGRAMAÇÃO/PR/R/Projeto Final/socialmedia_data.csv")


#dataset em factorial
social.f <- social.o
social.f$gender<- as.factor(social.o$gender)
social.f$platform<- as.factor(social.o$platform)
social.f$interests<- as.factor(social.o$interests)
social.f$location<- as.factor(social.o$location)
social.f$demographics<- as.factor(social.o$demographics)
social.f$profession<- as.factor(social.o$profession)
social.f$indebt<- as.factor(social.o$indebt)
social.f$isHomeOwner<- as.factor(social.o$isHomeOwner)
social.f$Owns_Car<- as.factor(social.o$Owns_Car)

#dataset em numerico
social.n <- social.f
social.n$gender<- as.numeric(social.f$gender)
social.n$platform<- as.numeric(social.f$platform)
social.n$interests<- as.numeric(social.f$interests)
social.n$location<- as.numeric(social.f$location)
social.n$demographics<- as.numeric(social.f$demographics)
social.n$profession<- as.numeric(social.f$profession)
social.n$indebt<- as.numeric(social.f$indebt)
social.n$isHomeOwner<- as.numeric(social.f$isHomeOwner)
social.n$Owns_Car<- as.numeric(social.f$Owns_Car)

#summary
summary (social.f)

#correlação
correlation2d <- cor(social.n)
corrgram(correlation2d, upper.panel = panel.pie, lower.panel = panel.shade, diag.panel = panel.density)

#------------------------------------------------------------------------------------------------------
#Plataformas - Faixa Etárias e Tempo Gasto

# Criar intervalos
idades <- c(social.o$age)
limites_intervalos <- seq(from = 18, to = 70, by = 5) 
intervalos_idade <- cut(idades, breaks = limites_intervalos, right = FALSE)

#tabela dos intervalos das idades
tabela_intervalos_idade<- table(intervalos_idade)
df_tabela_intervalos_idade <- as.data.frame(tabela_intervalos_idade)
df_tabela_intervalos_idade <- df_tabela_intervalos_idade[order(-df_tabela_intervalos_idade$Freq), ]
print(as.data.frame(df_tabela_intervalos_idade))

# Criar uma tabela de contagem por plataforma e intervalo de idade
tabela_contagem <- table(intervalos_idade, social.o$platform)
print(tabela_contagem)



#Gráfico de barras
tabela_contagem_transposta <- t(tabela_contagem)
cores <- c("#f8766d", "#00ba38", "#619cff")
barplot(tabela_contagem_transposta, beside = TRUE, legend = TRUE, col = cores,
        main = "Preferências de Plataforma por Faixa Etária", xlab = "Faixa Etária", ylab = "Contagem")


media_PorPlataforma <- aggregate(time_spent ~ platform, data = social.o, FUN = mean)

# Convert mean time to hours and minutes
hours <- floor(media_PorPlataforma$time_spent)
minutes <- round((media_PorPlataforma$time_spent - hours) * 60)
legenda_com_valores <- paste(media_PorPlataforma$platform, ": ", hours, "h", minutes, "m")

# grafico de pizza tempo médio por plataforma
pie(media_PorPlataforma$time_spent, labels = legenda_com_valores, col =cores,
    main = "Tempo Médio Gasto por Plataforma")

# Em tabela e com varivel idades
media_TimeSpent <- aggregate(time_spent ~ platform + intervalos_idade, data = social.o, FUN = function(x) {
  mean_time <- mean(x)  
})
media_TimeSpent <- media_TimeSpent[order(media_TimeSpent$time_spent, decreasing = TRUE), ]
print(media_TimeSpent)

convert_to_hours_minutes <- function(decimal_hours) {
  hours <- floor(decimal_hours)
  remaining_minutes <- round((decimal_hours - hours) * 60)
  return(paste(hours, "h", remaining_minutes, "m"))
}

media_TimeSpent$time_spent_convertido <- sapply(media_TimeSpent$time_spent, convert_to_hours_minutes)
print(media_TimeSpent)


#Heatmap para perceber quais as faixas etárias que gastam mais tempo nas plataformas
media_TimeSpent$platform_numeric <- as.numeric(factor(media_TimeSpent$platform))
average_matrix <- reshape2::dcast(media_TimeSpent, platform_numeric ~ intervalos_idade, value.var = "time_spent")

# Trocar as variáveis na matriz
average_matrix <- t(average_matrix)
average_matrix <- average_matrix[-1, ]

# Renomear as linhas e colunas da matriz
colnames(average_matrix) <- c("Facebook", "Instagram", "YouTube")

# Plotar o heatmap
heatmap(average_matrix, col = colorRampPalette(c("white", "#619cff"))(20), 
        xlab = "Plataforma", ylab = "Faixa Etária", 
        main = "Tempo Médio Gasto por Plataforma e Faixa Etária",
        margins = c(8, 5), cexCol = 0.8, cexRow = 0.8, 
        cex.main = 0.8)
#------------------------------------------------------------------------------------------------
#Plataformas - Interesses e Profissão

#tabela com contagem por profissão e interesse
tabela_crosstab <- table(social.o$profession, social.o$interests)
print("Tabela de Contingência:")
print(tabela_crosstab)

df_tabela_longo <- as.data.frame(as.table(tabela_crosstab))
colnames(df_tabela_longo) <- c("Profissão", "Interesse", "Contagem")

# Gráfico de histograma

ggplot(df_tabela_longo, aes(x = Profissão, y = Contagem, fill = Interesse)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Profissão", y = "Contagem", fill = "Interesse") +
  ggtitle("Distribuição de Interesses por Profissão") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table_counts <- table(social.n$profession, social.n$interests, social.n$platform)

array_counts <- array(table_counts, dim = c(dim(table_counts)[1], dim(table_counts)[2], 3))
#print(array_counts)


df_counts <- as.data.frame(as.table(table_counts))
colnames(df_counts) <- c("Profissão", "Interesse", "Plataforma", "Contagem")
profissao_labels <- c("Marketer Manager", "Software Engineer", "Student")
interesse_labels <- c("Lifestyle", "Sports", "Travel")
plataforma_labels <- c("Facebook", "Instagram", "Youtube")


df_counts$Profissão <- profissao_labels[df_counts$Profissão]
df_counts$Interesse <- interesse_labels[df_counts$Interesse]
df_counts$Plataforma <- plataforma_labels[df_counts$Plataforma]

#print(df_counts)

# gráfico de barras empilhadas com os nomes das categorias
ggplot(df_counts, aes(x = Profissão, y = Contagem, fill = Plataforma)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Interesse) +
  labs(title = "Distribuição de Plataforma por Profissão e Interesse",
       x = "Profissão", y = "Contagem", fill = "Plataforma")


profissoes <- unique(df_counts$Profissão)

# Loop para graficos pie sobre cada profissão | plataforma
for (profissao in profissoes) {

  df_profissao <- subset(df_counts, Profissão == profissao)

  pie_chart <- ggplot(df_profissao, aes(x = "", y = Contagem, fill = Plataforma)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("Distribuição de Plataformas para", profissao)) +
    theme_void() +
    theme(legend.position = "bottom") +
    scale_fill_discrete(name = "Plataforma")
  
  print(pie_chart)
}

# Executar o teste qui-quadrado de Mantel-Haenszel
mantelhaen.test(array_counts)

#---------------------------------------------------------------------------------------
#Rendimento & Tempo Gasto

#fazer análise com cluster

# Escala dos dados
data.s <- as.data.frame(scale(social.n[, c("income", "time_spent")]))

# Determinação do número ideal de clusters usando o método de silhueta
library(factoextra)

fviz_nbclust(data.s, kmeans, method = "silhouette")

#obter numero de cluster
nclust<-fviz_nbclust(data.s, kmeans, method = "silhouette")
nclust<-nclust$data
num_clusters<-as.numeric(nclust$clusters[which.max(nclust$y)])

#corre a criação de clusters
km<-kmeans(data.s,centers = num_clusters, iter.max = 100, nstart = 25)

# Visualização dos clusters
fviz_cluster(km, data = data.s)

# Adiciona a coluna 'cluster' ao conjunto de dados
social.n$cluster <- km$cluster

# Plot
ggplot(social.n, aes(x = income, y = time_spent, col = as.factor(cluster))) +
  geom_point() +
  labs(title = "Clusters de Utilizadores", x = "Income", y = "Tempo Gasto")


social.n$platform_name <- ifelse(social.n$platform == 1, "Facebook",
                                 ifelse(social.n$platform == 2, "Instagram", "YouTube"))

#histogramas com plataformas e clusters
social.n %>%
  mutate(cluster = factor(cluster),
         platform_name = factor(platform_name, levels = c("Facebook", "Instagram", "YouTube"))) %>%
  group_by(cluster, platform_name) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = platform_name, y = count, fill = platform_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cluster) +
  labs(title = "Histograma de Plataforma por Cluster", x = "Plataforma", y = "Contagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calcular a correlação entre rendimento e tempo gasto nas redes sociais

correlation_result <- cor.test(social.n$income, social.n$time_spent, method = "pearson")
print(correlation_result)


#-------------------------------------------------------------------------------------
#Previsões com base nas características

dataset<-social.n

#contar o numero de elementos de cada especie
tabela_resumo<-table(dataset$platform)
print(tabela_resumo)

#ordenar por coluna
dataset2<-dataset[order(dataset$age),]


#analise de melhores subsets
lm_subset <- regsubsets(platform ~ age + gender + time_spent + location + demographics + profession +income + indebt + isHomeOwner + Owns_Car + cluster, data = social.n, method = "exhaustive", nbest = 1)
summary(lm_subset)
plot(lm_subset)  

#baralhar dados
dataset.b<-dataset[sample(nrow(dataset)),]

#colunas a analisar
col<-c(3, 11, 12, 13)
x<-dataset.b[,col]
y<-dataset.b[,4]


#divide em teste 1/3 e treino 2/3
DT <- round(1000 * 0.667) # encontrar os 2/3

trainx<- x[1:DT,]
trainy<- y[1:DT]

testx<-x[(DT +1):1000,]
testy<-y[(DT+1):1000]

trainy <- as.factor(trainy)
#Criação do modelo/arvore de decisão
model<-C50::C5.0(trainx,trainy)
plot(model, main='decision tree')

#teste de aderencia do modelo
pred<-predict(model, testx)

resutadoteste<- cbind(testx, pred)
resutadoteste<- cbind(resutadoteste, testy)  

#Analise do modelo 
#teste area under de curve quanto mais perto de 1 melhor

roc_multiclass <- multiclass.roc(as.numeric(testy), as.numeric(pred))

# Plotar as curvas ROC para cada classe
plot(roc_multiclass, main = "Curvas ROC Multiclasse")

# Extrair as curvas ROC para cada classe
class_rocs <- roc_multiclass$rocs

# Plotar cada curva ROC individualmente
par(mfrow = c(1, length(class_rocs)))
for (i in 1:length(class_rocs)) {
  plot(class_rocs[[i]], main = paste("Curva ROC Classe", i))
}

auc_multiclass <- auc(roc_multiclass)
auc_multiclass



# Criação do modelo de árvore de decisão com rpart 
model_rpart <- rpart(platform ~ time_spent + isHomeOwner + Owns_Car + cluster, data = social.n, method = "class")

# Teste de aderência do modelo
pred_rpart <- predict(model_rpart, testx, type = "class")
rpart.plot(model_rpart)

#teste de erro
error_rate <- mean(pred_rpart != testy) * 100
error_rate
