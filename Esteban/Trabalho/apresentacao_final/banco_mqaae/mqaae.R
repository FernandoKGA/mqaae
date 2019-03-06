#Leitura do XLS
library(readxl)
data_estacao <- read_excel("C:/Users/ferna/Desktop/banco_mqaae/data_estacao.xlsx", 
                           col_names = FALSE, skip = 1)
View(data_estacao)

colnames(data_estacao) <- c("Data","Q-E","ZN-E","PH-E","DBO-E","DQO-E","SS-E","SSV-E","SED-E","COND-E","PH-P","DBO-P","SS-P","SSV-P","SED-P","COND-P","PH-D","DBO-D","DQO-D","SS-D","SSV-D","SED-D","COND-D","PH-S","DBO-S","DQO-S","SS-S","SSV-S","SED-S","COND-S","RD-DBO-P","RD-SS-P","RD-SED-P","RD-DBO-S","RD-DQO-S","RD-DBO-G","RD-DQO-G","RD-SS-G","RD-SED-G")
data_estacao

head(data_estacao)
str(data_estacao)
data_estacao[data_estacao == "?"] <- NA
data_estacao

data_estacao$Data <- as.factor(data_estacao$Data)
data_estacao$'Q-E' <- as.integer(data_estacao$'Q-E')
data_estacao$'ZN-E' <- as.double(data_estacao$'ZN-E')
data_estacao$'PH-E' <- as.double(data_estacao$'PH-E')
data_estacao$'DBO-E' <- as.integer(data_estacao$'DBO-E')
data_estacao$'DQO-E' <- as.integer(data_estacao$'DQO-E')
data_estacao$'SS-E' <- as.integer(data_estacao$'SS-E')
data_estacao$'SSV-E' <- as.double(data_estacao$'SSV-E')
data_estacao$'SED-E' <- as.double(data_estacao$'SED-E')
data_estacao$'COND-E' <- as.integer(data_estacao$'COND-E')
data_estacao$'PH-P' <- as.double(data_estacao$'PH-P')
data_estacao$'DBO-P' <- as.integer(data_estacao$'DBO-P')
data_estacao$'SS-P' <- as.integer(data_estacao$'SS-P')
data_estacao$'SSV-P' <- as.double(data_estacao$'SSV-P')
data_estacao$'SED-P' <- as.double(data_estacao$'SED-P')
data_estacao$'COND-P' <- as.integer(data_estacao$'COND-P')
data_estacao$'PH-D' <- as.double(data_estacao$'PH-D')
data_estacao$'DBO-D' <- as.integer(data_estacao$'DBO-D')
data_estacao$'DQO-D' <- as.integer(data_estacao$'DQO-D')
data_estacao$'SS-D' <- as.integer(data_estacao$'SS-D')
data_estacao$'SSV-D' <- as.double(data_estacao$'SSV-D')
data_estacao$'SED-D' <- as.double(data_estacao$'SED-D')
data_estacao$'COND-D' <- as.integer(data_estacao$'COND-D')
data_estacao$'PH-S' <- as.double(data_estacao$'PH-S')
data_estacao$'DBO-S' <- as.integer(data_estacao$'DBO-S')
data_estacao$'DQO-S' <- as.integer(data_estacao$'DQO-S')
data_estacao$'SS-S' <- as.integer(data_estacao$'SS-S')
data_estacao$'SSV-S' <- as.double(data_estacao$'SSV-S')
data_estacao$'SED-S' <- as.double(data_estacao$'SED-S')
data_estacao$'COND-S' <- as.integer(data_estacao$'COND-S')
data_estacao$'RD-DBO-P' <- as.double(data_estacao$'RD-DBO-P')
data_estacao$'RD-SS-P' <- as.double(data_estacao$'RD-SS-P')
data_estacao$'RD-SED-P' <- as.double(data_estacao$'RD-SED-P')
data_estacao$'RD-DBO-S' <- as.double(data_estacao$'RD-DBO-S')
data_estacao$'RD-DQO-S' <- as.double(data_estacao$'RD-DQO-S')
data_estacao$'RD-DBO-G' <- as.double(data_estacao$'RD-DBO-G')
data_estacao$'RD-DQO-G' <- as.double(data_estacao$'RD-DQO-G')
data_estacao$'RD-SS-G' <- as.double(data_estacao$'RD-SS-G')
data_estacao$'RD-SED-G' <- as.double(data_estacao$'RD-SED-G')

#Omitindo os NA's
data_est <- na.omit(data_estacao)
data_est

#1 - ANOVA
"
(H0) Todas as etapas de tratamento tem o mesmo PH.

(H1) Existe diferença entre os 4 PH's existentes desde a entrada,
até a saída da estação de tratamento de água 
"

#Dados dos PHs juntos
phs <- data_est[c("PH-E","PH-P","PH-D","PH-S")]

#Pega as unidades de cada um
phs_e <- phs$'PH-E'
phs_p <- phs$'PH-P'
phs_d <- phs$'PH-D'
phs_s <- phs$'PH-S'

#Separa os dados

Niveis <- c(phs_e)
Tipos <- c("PH-E")
phs_e_data <- data.frame(Niveis,Tipos)

Niveis <- c(phs_p)
Tipos <- c("PH-P")
phs_p_data <- data.frame(Niveis,Tipos)

Niveis <- c(phs_d)
Tipos <- c("PH-D")
phs_d_data <- data.frame(Niveis,Tipos)

Niveis <- c(phs_s)
Tipos <- c("PH-S")
phs_s_data <- data.frame(Niveis,Tipos)

phs_half_data1 <- rbind(phs_e_data,phs_p_data)

phs_half_data2 <- rbind(phs_half_data1,phs_d_data)

phs_dps <- rbind(phs_half_data2,phs_s_data)

library("ggpubr")
ggboxplot(phs_dps, y = "Niveis", x = "Tipos", 
          color = "Tipos", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#ff0000"),
          order = c("PH-E", "PH-P", "PH-D", "PH-S"),
          ylab = "Níveis", xlab = "PHs")

ggline(phs_dps, x = "Tipos", y = "Niveis", 
       add = c("mean_se", "jitter"), 
       order = c("PH-E", "PH-P", "PH-D", "PH-S"),
       ylab = "Níveis", xlab = "PHs")

anova <- aov(Niveis~Tipos,data=phs_dps)
anova

#Sumário
summary(anova)
"
O valor encontrado no baoifewifewfpiesfnesi
"

TukeyHSD(anova)

#Fazendo a analise com DBO

"
(H0) Todas as etapas do tratamento tem o mesmo nível de DBO.

(H1) Existe diferença entre os 4 DBO's existentes desde a entrada,
até a saída da estação de tratamento de água.
"

#Dados dos PHs juntos
dbo <- data_est[c("DBO-E","DBO-P","DBO-D","DBO-S")]

#Pega as unidades de cada um
dbo_e <- dbo$'DBO-E'
dbo_p <- dbo$'DBO-P'
dbo_d <- dbo$'DBO-D'
dbo_s <- dbo$'DBO-S'

#Separa os dados

Niveis <- c(dbo_e)
Tipos <- c("DBO-E")
dbo_e_data <- data.frame(Niveis,Tipos)


Niveis <- c(dbo_p)
Tipos <- c("DBO-P")
dbo_p_data <- data.frame(Niveis,Tipos)


Niveis <- c(dbo_d)
Tipos <- c("DBO-D")
dbo_d_data <- data.frame(Niveis,Tipos)


Niveis <- c(dbo_s)
Tipos <- c("DBO-S")
dbo_s_data <- data.frame(Niveis,Tipos)


dbo_half_data1 <- rbind(dbo_e_data,dbo_p_data)


dbo_half_data2 <- rbind(dbo_half_data1,dbo_d_data)


dbo_dps <- rbind(dbo_half_data2,dbo_s_data)

library("ggpubr")
ggboxplot(dbo_dps, y = "Niveis", x = "Tipos", 
          color = "Tipos", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#ff0000"),
          order = c("DBO-E","DBO-P","DBO-D","DBO-S"),
          ylab = "Níveis", xlab = "DBOs")

ggline(dbo_dps, x = "Tipos", y = "Niveis", 
       add = c("mean_se", "jitter"), 
       order = c("DBO-E","DBO-P","DBO-D","DBO-S"),
       ylab = "Níveis", xlab = "DBOs")

anova <- aov(Niveis~Tipos,data=dbo_dps)
anova

#Sumário
summary(anova)
"
O valor encontrado bj0adwqidnwaíd´dosd
"

TukeyHSD(anova)


#2 - Análise de Regressão Simples

summary(data_est$'SS-P')
var(data_est$'SS-P')
sd(data_est$'SS-P')

summary(data_est$'SED-P')
var(data_est$'SED-P')
sd(data_est$'SED-P')

plot(data_est$'SS-P',data_est$'SED-P')

cor(data_est$'SS-P',data_est$'SED-P')
cor.test(data_est$'SS-P',data_est$'SED-P')

#Ajuste para a regressao, lm(y ~ x) onde y eh a variavel resposta
#e x a variavel preditora
ajuste <- lm(data_est$'SED-P' ~ data_est$'SS-P')

#plota um resumo do ajuste
summary(ajuste)

#Plota a tabela de analise de variancia
anova(ajuste)

#Plota a linha para a funcao
windows()
plot(data_est$'SS-P',data_est$'SED-P')
abline(lm(data_est$'SED-P' ~ data_est$'SS-P'))

#constroi um intervalo de confianca
confint(ajuste)

#Analise dos residuos
windows()
plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Resíduos")
abline(h=0)
windows()
plot(data_est$'SS-P',residuals(ajuste),xlab="SS-P",ylab="Resíduos")
abline(h=0)

ajuste$residuals
ajuste$fitted.values

#mediana de SS-P
median(data_est$'SS-P')

#teste para comparacao das duas variancias
var.test(residuals(ajuste)[data_est$'SS-P'>median(data_est$'SS-P')],
         residuals(ajuste)[data_est$'SS-P'<median(data_est$'SS-P')])

"
Falar qual o resultado ud9uiwqdh9iwqd
"


#3 - Análise de Regressão Múltipla
data_est

#Nao sei fazer esta bosta


#4 - Análise de Componentes Principais

#Carregamento de biblioteca
library(psych)

#Dados
X <- subset(data_est, select = -Data)  #Vamos retirar a Data do dataset

#descricoes estatisticas
summary(X)

#matriz de correlacao
cor(X)

#PCA
pca1 <- princomp(X, scores=TRUE, cor=TRUE)

#descricoes estatisticas do PCA
summary(pca1)

#Componentes Principais
loadings(pca1)

#Imprimir os autovalores
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

#Imprimir os escores
biplot(pca1)

#Rotacao VARIMAX
pca2 <- principal(X, nfactors = 2, scores=TRUE)
summary(pca2)
biplot(pca2)

#A que conclusão chegamos???
"
Mostra a correlação de certas variáveis e alguns agrupamentos entre elas.
Os scores de perfomances estão mais agrupados. Algumas variáveis de entrada estão agrupadas
com outras variáveis de entrada e alguams mais sobre etapas intermediárias.
As de saída estão agrupadas em outra parte abaixo do gráfico.
"


#5 - Análise Fatorial

# EXPLORATÓRIA

summary(data_est)

data_est_num <- data_est[,-1]
data_est_num
matriz_cor_data <- cor(data_est_num)
print(matriz_cor_data,digits=3)

library(corrplot)
corrplot(matriz_cor_data)

#Teste de esfericidade de Bartlett

Bartlett.sphericity.test <- function(x)
{
  method <- "Teste de esfericidade de Bartlett"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omitindo valores faltantes
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}
Bartlett.sphericity.test(data_est_num)

#KMO
kmo = function(x)
{
  x = subset(x, complete.cases(x))
  r = cor(x)
  r2 = r^2 
  i = solve(r) 
  d = diag(i) 
  p2 = (-i/sqrt(outer(d, d)))^2 
  diag(r2) <- diag(p2) <- 0 
  KMO = sum(r2)/(sum(r2)+sum(p2))
  MSA = colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

kmo_data <- kmo(data_est_num)
print(kmo_data)

#Matriz de correlacao parcial
partial.cor <- function (x)
{
  R <- cor(x)
  RI <- solve(R)
  D <- 1/sqrt(diag(RI))
  Rp <- -RI * (D %o% D)
  diag(Rp) <- 0
  rownames(Rp) <- colnames(Rp) <- colnames(x)
  Rp
}
mat_anti_imagem <- -partial.cor(data_est_num)
mat_anti_imagem

#Método dos componentes principais com a matriz de correlação amostral
acpcor <- prcomp(data_est_num, scale = TRUE)
summary(acpcor)

plot(1:ncol(data_est_num), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Variância", pch = 20, cex.axis = 1.3, cex.lab = 1.3)

#fatores
k <- 4
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")
carfat

#estimação das comunalidades e das variâncias específicas
comum <- rowSums(carfat^2)
vespec <- diag(matriz_cor_data) - comum
estimat <- cbind(comum, vespec, diag(matriz_cor_data))
rownames(estimat) <- colnames(data_est_num)
colnames(estimat) <- c("Comunalidade", "Variância única", "Variância")
estimat

resid <- matriz_cor_data - (carfat %*% t(carfat) + diag(vespec))
resid

#Visando auxiliar na interpretação dos fatores, realizamos uma rotação
#pelo método varimax.
#A função varimax encontra-se no pacote stats.

carfatr <- varimax(carfat)
carfatr

#Plot sem rotação varimax
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)

#Plot com rotação varimax
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)

"
Que conclusão chegamos???
"

#CONFIRMATÓRIA

library(psych)
library(lavaan)
library(mirt)

#explorar

data_est_num_renamed <- data_est_num
names(data_est_num_renamed) <- c(
  "QE","ZNE","PHE","DBOE","DQOE",
  "SSE","SSVE","SEDE","CONDE",
  "PHP","DBOP","SSP","SSVP","SEDP","CONDP",
  "PHD","DBOD","DQOD","SSD","SSVD","SEDD","CONDD",
  "PHS","DBOS","DQOS","SSS","SSVS","SEDS","CONDS",
  "RDDBOP","RDSSP","RDSEDP",
  "RDDBOS","RDDQOS",
  "RDDBOG","RDDQOG","RDSSG","RDSEDG"
)
data_est_num_renamed
scree(data_est_num_renamed)
ma_cor <- cor(data_est_num_renamed)
fa(ma_cor, cor = 'poly') #analise fatorial com relacao policorica?
fa.parallel(ma_cor, cor = 'poly')
factor.plot(fa(ma_cor, cor = 'poly'), cut=.2)
#fa.diagram(fa(ma_cor, cor = 'poly'), cut=.2)
irt.fa(data_est_num_renamed, plot = TRUE)

#criar um SEM
library(semPlot)
sum(is.na(data_est_num_renamed[1:38]))

modelo_ceri <- 'Performance =~ RDDBOS + RDDQOS + RDDQOG + RDDBOG + RDSEDG + RDSSG
Saida =~ DBOS + DQOS + SEDS + SSS
Intermedio =~ SSVE + SSVS + SSVP + SSVD + 
SSE + SSP + SSD + 
SEDE + SEDP + 
CONDE + CONDP + CONDS + SEDD +
PHP + PHD + PHE + PHS +
ZNE + QE +
RDSSP + RDSEDP + RDDBOP
Demanda =~ DQOD + DQOE +
  DBOD + DBOE + DBOP
'

ceri_fit <- lavaan(modelo_ceri, data = data_est_num_renamed, auto.var = TRUE,
                   auto.fix.first = TRUE, auto.cov.lv.x = TRUE, estimator = "ML")
summary(ceri_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
semPaths(ceri_fit, what = "std", residuals = FALSE, nCharNodes = 4, edge.label.cex = 0.7, legend = FALSE, title.cex = 0.5)
standardizedsolution(ceri_fit)


#6 - Escalonamento Multidimensional
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/
data_est_esc_mult <- data_est[,-1]

# Load required packages
library(magrittr)
library(dplyr)
library(ggpubr)
# Cmpute MDS
mds <- data_est_esc_mult %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          size = 1,
          repel = TRUE)

# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)


#7 - Análise discriminante

#TODAS as nossas variáveis são métricas!!!
#Necessidade de adicionar mais uma variável para definir a qualitativa!
#Risco de corromper o dataset colocando-a

#8 - Correlação Canônica
data_cor_cc <- data_est[,-1]

data_ent_itr <- data_cor_cc[,1:29]
data_perf <- data_cor_cc[,30:38]

library(CCA)

ma_correl <- matcor(data_ent_itr,data_perf)
img.matcor(ma_correl,type = 2)

cc <- cc(data_ent_itr,data_perf)

cc$cor

par(mfrow = c(1,2))
barplot(cc$cor, main = "Canonical correlations ", col = "gray")

cc$xcoef
cc$ycoef
plt.cc(cc, var.label = TRUE, ind.names = data[,1])

library(vegan)

cc2 <- cca(data_ent_itr,data_perf)
plot(cc2, scaling=1)

#9 - Testes de Aderência (Análise de Dados Categorizados)
data_est

#nao sei fazer essa porra

#10 - Co-clustering
library(biclust)

data_est_num <- data_est[,-1]
data_est_num


#Tratamento dos dados
data_est_biclust <- data.matrix(data_est_num)
data_est_biclust
data_est_biclust_bccc <- biclust(data_est_biclust,method="BCCC")
data_est_biclust_bccc

hc <- hclust(dist(data_est_biclust))
plot(hc)

#Plots
drawHeatmap2(data_est_biclust,data_est_biclust_bccc,1)
drawHeatmap2(data_est_biclust,data_est_biclust_bccc,2)
drawHeatmap2(data_est_biclust,data_est_biclust_bccc,3)
heatmapBC(data_est_biclust,data_est_biclust_bccc)

#11 - Random Forest
data_est
"
O que queremos verificar?
É ou não é?
Boa qualidade ou não?
https://www.researchgate.net/publication/316647427_Wastewater_Effluent_Prediction_Based_on_Decision_Tree
"

#NAs omitidos
data_est_num <- data_est[,-1]
data_est_num

data_est_num_renamed <- data_est_num
names(data_est_num_renamed) <- c(
  "QE","ZNE","PHE","DBOE","DQOE",
  "SSE","SSVE","SEDE","CONDE",
  "PHP","DBOP","SSP","SSVP","SEDP","CONDP",
  "PHD","DBOD","DQOD","SSD","SSVD","SEDD","CONDD",
  "PHS","DBOS","DQOS","SSS","SSVS","SEDS","CONDS",
  "RDDBOP","RDSSP","RDSEDP",
  "RDDBOS","RDDQOS",
  "RDDBOG","RDDQOG","RDSSG","RDSEDG"
)
data_est_num_renamed

#Necessaria a transformacao para fatores das expressoes

data_est_num_renamed$'QE' <- as.factor(data_est_num_renamed$'QE')
data_est_num_renamed$'ZNE' <- as.factor(data_est_num_renamed$'ZNE')
data_est_num_renamed$'PHE' <- as.factor(data_est_num_renamed$'PHE')
data_est_num_renamed$'DBOE' <- as.factor(data_est_num_renamed$'DBOE')
data_est_num_renamed$'DQOE' <- as.factor(data_est_num_renamed$'DQOE')
data_est_num_renamed$'SSE' <- as.factor(data_est_num_renamed$'SSE')
data_est_num_renamed$'SSVE' <- as.factor(data_est_num_renamed$'SSVE')
data_est_num_renamed$'SEDE' <- as.factor(data_est_num_renamed$'SEDE')
data_est_num_renamed$'CONDE' <- as.factor(data_est_num_renamed$'CONDE')
data_est_num_renamed$'PHP' <- as.factor(data_est_num_renamed$'PHP')
data_est_num_renamed$'DBOP' <- as.factor(data_est_num_renamed$'DBOP')
data_est_num_renamed$'SSP' <- as.factor(data_est_num_renamed$'SSP')
data_est_num_renamed$'SSVP' <- as.factor(data_est_num_renamed$'SSVP')
data_est_num_renamed$'SEDP' <- as.factor(data_est_num_renamed$'SEDP')
data_est_num_renamed$'CONDP' <- as.factor(data_est_num_renamed$'CONDP')
data_est_num_renamed$'PHD' <- as.factor(data_est_num_renamed$'PHD')
data_est_num_renamed$'DBOD' <- as.factor(data_est_num_renamed$'DBOD')
data_est_num_renamed$'DQOD' <- as.factor(data_est_num_renamed$'DQOD')
data_est_num_renamed$'SSD' <- as.factor(data_est_num_renamed$'SSD')
data_est_num_renamed$'SSVD' <- as.factor(data_est_num_renamed$'SSVD')
data_est_num_renamed$'SEDD' <- as.factor(data_est_num_renamed$'SEDD')
data_est_num_renamed$'CONDD' <- as.factor(data_est_num_renamed$'CONDD')
data_est_num_renamed$'PHS' <- as.factor(data_est_num_renamed$'PHS')
data_est_num_renamed$'DBOS' <- as.factor(data_est_num_renamed$'DBOS')
data_est_num_renamed$'DQOS' <- as.factor(data_est_num_renamed$'DQOS')
data_est_num_renamed$'SSS' <- as.factor(data_est_num_renamed$'SSS')
data_est_num_renamed$'SSVS' <- as.factor(data_est_num_renamed$'SSVS')
data_est_num_renamed$'SEDS' <- as.factor(data_est_num_renamed$'SEDS')
data_est_num_renamed$'CONDS' <- as.factor(data_est_num_renamed$'CONDS')
data_est_num_renamed$'RDDBOP' <- as.factor(data_est_num_renamed$'RDDBOP')
data_est_num_renamed$'RDSSP' <- as.factor(data_est_num_renamed$'RDSSP')
data_est_num_renamed$'RDSEDP' <- as.factor(data_est_num_renamed$'RDSEDP')
data_est_num_renamed$'RDDBOS' <- as.factor(data_est_num_renamed$'RDDBOS')
data_est_num_renamed$'RDDQOS' <- as.factor(data_est_num_renamed$'RDDQOS')
data_est_num_renamed$'RDDBOG' <- as.factor(data_est_num_renamed$'RDDBOG')
data_est_num_renamed$'RDDQOG' <- as.factor(data_est_num_renamed$'RDDQOG')
data_est_num_renamed$'RDSSG' <- as.factor(data_est_num_renamed$'RDSSG')
data_est_num_renamed$'RDSEDG' <- as.factor(data_est_num_renamed$'RDSEDG')
data_est_num_renamed

#Carrega as bibliotecas

library(ggplot2)
library(cowplot)
library(randomForest)
library(missForest)
library(mice)

set.seed(42)

## impute any missing values in the training set using proximities
data.imputed <- rfImpute(DBOS ~ PHS+DQOS+SSS+SEDS+SSVS, data = data_est_num_renamed, iter=6)

#Tirar as 10 ultimas colunas
#data.imputed <- missForest(matrix(data_est_num_renamed),maxiter=10,ntree=300,verbose=TRUE)

## Queremos prever a quantidade de DBO dada uma entrada, the thing we are trying to predict,
## there are 13 variables. So by default, randomForest() will set
## mtry = sqrt(13) = 3.6 rounded down = 3
## Also, by default random forest generates 500 trees (NOTE: rfImpute() only
## generates 300 tress by default)
model <- randomForest(DBOS ~ ., data=data_est_num_renamed, proximity=TRUE)

model

model$err.rate

#PROBLEMA COM OS DADOS FALTANTES!!! NAO TEMOS TAXA DE ERRO

#INPUT DEVE ACEITAR CATEGORIAS, NAO NUMERICOS
#DEVE SE DIVIDIR OS NUMERICOS NAS CATEGORIAS

## Now check to see if the random forest is actually big enough...
## Up to a point, the more trees in the forest, the better. You can tell when
## you've made enough when the OOB no longer improves.
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=2),
  Type=rep(c("OOB", "DBOS"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"DBOS"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

## NOTE: After building a random forest with 500 tress, the graph does not make
## it clear that the OOB-error has settled on a value or, if we added more
## trees, it would continue to decrease.
## So we do the whole thing again, but this time add more trees.
model <- randomForest(hd ~ ., data=data.imputed, ntree=1000, proximity=TRUE)
model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "?", "?"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"?"],
          model$err.rate[,"?"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

#Nao tem variavel qualitativa para rodar!!!!!!


#12 - Regressão Logística

#Nao tem variáveis binárias para fazer a regressão