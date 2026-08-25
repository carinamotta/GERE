# Capítulo 07 - Análises Ecológicas no R
# https://analises-ecologicas.com/


# Pacotes---------------------------------------------------------------------

devtools::install_github("paternogbc/ecodados")

package.list <- c("here", 
                  "ecodados", 
                  "car", 
                  "ggpubr", 
                  "ggforce",
                  "lsmeans",
                  "lmtest",
                  "sjPlot",
                  "nlme",
                  "ape",
                  "fields",
                  "tidyverse",
                  "vegan",
                  "rdist"
)


#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]

if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}

# Dados-----------------------------------------------------------------------
CRC_PN_macho <- ecodados::teste_t_var_igual
CRC_LP_femea <- ecodados::teste_t_var_diferente
Pareado <- ecodados::teste_t_pareado
correlacao_arbustos <- ecodados::correlacao
dados_regressao <- ecodados::regressoes
dados_regressao_mul <- ecodados::regressoes
dados_anova_simples <- ecodados::anova_simples
dados_dois_fatores <- ecodados::anova_dois_fatores
dados_dois_fatores_interacao <- ecodados::anova_dois_fatores
dados_dois_fatores_interacao2 <- ecodados::anova_dois_fatores_interacao2
dados_bloco <- ecodados::anova_bloco
dados_ancova <- ecodados::ancova
data("mite")
data("mite.xy")
coords <- mite.xy
colnames(coords) <- c("long", "lat")
data("mite.env")


# 7.1 Teste T (de Student) para duas amostras independentes--------------------

## Exemplo prático 1---------------- 
# Teste T para duas amostras com variâncias iguais

## Cabeçalho dos dados
head(CRC_PN_macho) 

## Teste de normalidade
residuos <- lm(CRC ~ Estacao, data = CRC_PN_macho)
qqPlot(residuos)


## Teste de Shapiro-Wilk
residuos_modelo <- residuals(residuos)
shapiro.test(residuos_modelo)


## Teste de homogeneidade de variância
leveneTest(CRC ~ as.factor(Estacao), data = CRC_PN_macho)


## Análise Teste T 
t.test(CRC ~ Estacao, data = CRC_PN_macho, var.equal = TRUE)
        

## Gráfico
ggplot(data = CRC_PN_macho, aes(x = Estacao, y = CRC, color = Estacao)) + 
  labs(x = "Estações", 
       y = expression(paste("CRC (mm) - ", italic("P. nattereri")))) +
  geom_boxplot(fill = c("darkorange", "cyan4"), color = "black", 
               outlier.shape = NA) +
  geom_jitter(shape = 16, position = position_jitter(0.1), 
              cex = 5, alpha = 0.7) +
  scale_color_manual(values = c("black", "black")) +
  tema_livro() +
  theme(legend.position = "none")


## Análise Teste T Pareado

t.test(Riqueza ~ Estado, paired = TRUE, data = Pareado)


## Exemplo prático 2-------- 
# Teste T para duas amostras independentes com variâncias diferentes

## Cabeçalho dos dados
head(CRC_LP_femea) 

## Teste de normalidade usando QQ-plot
residuos_LP <- lm(CRC ~ Estacao, data = CRC_LP_femea)
qqPlot(residuos_LP)

## Teste de Shapiro-Wilk
residuos_modelo_LP <- residuals(residuos_LP)
shapiro.test(residuos_modelo_LP)

## Teste de homogeneidade da variância
leveneTest(CRC ~ as.factor(Estacao), data = CRC_LP_femea)

## Teste T
t.test(CRC ~ Estacao, data = CRC_LP_femea, var.equal = FALSE)

## Gráfico
ggplot(data = CRC_LP_femea, aes(x = Estacao, y = CRC, color = Estacao)) + 
  geom_boxplot(fill = c("darkorange", "cyan4"), width = 0.5, 
               color = "black", outlier.shape = NA, alpha = 0.7) +
  geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
  scale_color_manual(values = c("darkorange", "cyan4")) +
  labs(x = "Estações", 
       y = expression(paste("CRC (mm) - ", italic("L. podicipinus"))), size = 15) +
  tema_livro() +
  theme(legend.position = "none")


# 7.2 Teste T para amostras pareadas--------------------------------------------

## Cabeçalho dos dados
head(Pareado) 

## Análise Teste T Pareado
t.test(Riqueza ~ Estado, paired = TRUE, data = Pareado)

## Gráfico
ggpaired(Pareado, x = "Estado", y = "Riqueza",
         color = "Estado", line.color = "gray", line.size = 0.8, 
         palette = c("darkorange", "cyan4"), width = 0.5, 
         point.size = 4, xlab = "Estado das localidades", 
         ylab = "Riqueza de Espécies") +
  expand_limits(y = c(0, 150)) +
  tema_livro() 

# 7.3 Correlação de Pearson---------------------------------------------------

## Cabeçalho dos dados
head(correlacao_arbustos) 

## Correlação de Pearson
cor.test(correlacao_arbustos$Tamanho_raiz, correlacao_arbustos$Tamanho_tronco, method = "pearson")

## Alternativamente
cor.test(~ Tamanho_tronco + Tamanho_raiz, data = correlacao_arbustos, method = "pearson")

## Gráfico
ggplot(data = correlacao_arbustos, aes(x = Tamanho_raiz, y = Tamanho_tronco)) + 
  labs(x = "Tamanho da raiz (m)", y = "Altura do tronco (m)") +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_text(x = 14, y = 14, label = "r = 0.89, P < 0.001", 
            color = "black", size = 5) +
  geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dashed") +
  tema_livro() +
  theme(legend.position = "none")
