library(psych)
library(lavaan)
library(mirt)

#explorar
dados_cfe <- X02_RTE_CEREAL_V2
scree(dados_cfe)
ma_cor <- cor(dados_cfe)
fa(ma_cor, cor = 'poly') #analise fatorial com relacao policorica?
fa.parallel(ma_cor, cor = 'poly')
factor.plot(fa(ma_cor, cor = 'poly'), cut=.2)
fa.diagram(fa(ma_cor, cor = 'poly'), cut=.2)
irt.fa(dados_cfe, plot = TRUE)

#criar um SEM
library(semPlot)
sum(is.na(dados_cfe[1:24]))
modelo_ceri <- 'Resultado =~ Gratificante + Natural + Energia + Nutritivo + Saúde + Fibra + Qualidade + Regular
+ Satisfaz + Chato + Prazer + Simples + Fruta + Crocante + Fácil + Processo + Família + Sal + Encharcado + Divertido
+ Crianças + Doce + Calorias + Açúcar + Econômico'
ceri_fit <- lavaan(modelo_ceri, data = dados_cfe, auto.var = TRUE,
                   auto.fix.first = TRUE, auto.cov.lv.x = TRUE, estimator = "ML")
summary(ceri_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
semPaths(ceri_fit, what = "std", residuals = FALSE, nCharNodes = 4, edge.label.cex = 0.7, legend = FALSE, title.cex = 0.5)
standardizedsolution(ceri_fit)