# 17 de março 2025

# pacotes

# a vector listing package names needed 

#devtools::install_github("paternogbc/ecodados")

package.list <- c("here", #so I don't have to deal with setting a WD
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

## Dados
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


# t-test

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


# 7.2 ----
# 
# ## Cabeçalho dos dados
head(Pareado) 
#>   Areas Riqueza       Estado
#> 1     1      92 Pre-Queimada
#> 2     2      74 Pre-Queimada
#> 3     3      96 Pre-Queimada
#> 4     4      89 Pre-Queimada
#> 5     5      76 Pre-Queimada
#> 6     6      80 Pre-Queimada

t.test(Riqueza ~ Estado, paired = TRUE, data = Pareado)

## Gráfico
ggpaired(Pareado, x = "Estado", y = "Riqueza",
         color = "Estado", line.color = "gray", line.size = 0.8, 
         palette = c("darkorange", "cyan4"), width = 0.5, 
         point.size = 4, xlab = "Estado das localidades", 
         ylab = "Riqueza de Espécies") +
  expand_limits(y = c(0, 150)) +
  tema_livro() 

# 7.3 - Pearson's Correlation ----

head(correlacao_arbustos) 


cor.test(correlacao_arbustos$Tamanho_raiz, correlacao_arbustos$Tamanho_tronco, method = "pearson")


cor.test(~ Tamanho_tronco + Tamanho_raiz, data = correlacao_arbustos, method = "pearson")

## Gráfico
ggplot(data = correlacao_arbustos, aes(x = Tamanho_raiz, y = Tamanho_tronco)) + 
  labs(x = "Tamanho da raiz (m)", y = "Altura do tronco (m)") +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_text(x = 14, y = 14, label = "r = 0.89, P < 0.001", 
            color = "black", size = 5) +
  geom_smooth(method = lm, se = T, color = "black", linetype = "dashed") +
  tema_livro() +
  theme(legend.position = "none")

#7.4 Regressão Linear ------
#
#
head(dados_regressao) 

## regressão simples
modelo_regressao <- lm(CRC ~ Temperatura, data = dados_regressao)

## Verificar as premissas do teste
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modelo_regressao)
dev.off() # volta a configuração dos gráficos para o formato padrão 

anova(modelo_regressao)

summary(modelo_regressao)

## Gráfico
ggplot(data = dados_regressao, aes(x = Temperatura, y = CRC)) + 
  labs(x = "Temperatura média anual (°C)", 
       y = "Comprimento rostro-cloacal (mm)") +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  tema_livro() +
  theme(legend.position = "none")


coef(modelo_regressao)

## Regressão múltipla
modelo_regressao_mul <- lm(CRC ~ Temperatura + Precipitacao,
                           data = dados_regressao_mul)
modelo_regressao_mul 

# positive relationship with temperature and negative relationship with precip.

# Multicolinearidade
vif(modelo_regressao_mul)

# VIF values are < 2, not correlated 

## Normalidade e homogeneidade das variâncias
plot_grid(plot_model(modelo_regressao_mul , type = "diag"))

## regressão múltipla
summary(modelo_regressao_mul)

## Criando os modelos aninhados
modelo_regressao_mul <- lm(CRC ~ Temperatura + Precipitacao, 
                           data = dados_regressao_mul)
modelo_regressao <- lm(CRC ~ Temperatura, data = dados_regressao_mul)

## Likelihood-ratio test (LRT)
lrtest(modelo_regressao_mul, modelo_regressao)


## Cabeçalho dos dados
head(dados_anova_simples) 

## Análise ANOVA de um fator
Modelo_anova <- aov(Crescimento ~ Tratamento, data = dados_anova_simples) 

Modelo_anova

## Normalidade
shapiro.test(residuals(Modelo_anova))
#distribtution is normal, > 0.05

## Homogeneidade da variância
bartlett.test(Crescimento ~ Tratamento, data = dados_anova_simples)
# variance homogeneous, Bartlett already assumes that data is normally distributed

anova(Modelo_anova)

TukeyHSD(Modelo_anova)

## Reorganizando a ordem que os grupos irão aparecer no gráfico
dados_anova_simples$Tratamento <- factor(dados_anova_simples$Tratamento,
                                         levels = c("Controle", "Adubo_Tradicional", "Adubo_X-2020"))

## Gráfico
ggplot(data = dados_anova_simples, 
       aes(x = Tratamento, y = Crescimento, color = Tratamento)) + 
  geom_boxplot(fill = c("darkorange", "darkorchid", "cyan4"), 
               color = "black", show.legend = FALSE, alpha = 0.4) +
  geom_jitter(shape = 16, position = position_jitter(0.1), 
              cex = 4, alpha = 0.7) +
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  scale_y_continuous(limits = c(0, 20), breaks = c(0, 5, 10, 15, 20)) +
  geom_text(x = 1, y = 12, label = "ab", color = "black", size = 5) +
  geom_text(x = 2, y = 17, label = "a", color = "black", size = 5) +
  geom_text(x = 3, y = 17, label = "b", color = "black", size = 5) +
  scale_x_discrete(labels = c("Sem adubo", "Tradicional", "X-2020")) +
  labs(x = "Adubação", y = "Crescimento Coffea arabica (cm)", size = 20) +
  tema_livro() +
  theme(legend.position = "none")  


head(dados_dois_fatores) 

## Análise Anova de dois fatores 
# A interação entre os fatores é representada por *
Modelo1 <- aov(Tempo ~ Pessoas * Idade, data = dados_dois_fatores) 

# Olhando os resultados
anova(Modelo1) #P > 0.05, likelihood test to see if the simpler model is better

# Criando modelo sem interação.
Modelo2 <- aov(Tempo ~ Pessoas + Idade, data = dados_dois_fatores) 

## LRT
lrtest(Modelo1, Modelo2) #null hypothesis is that the simpler model is better, so if P > 0.05, then we fail to reject the null hypothesis

# Verificando as premissas do teste.
plot_grid(plot_model(Modelo2, type = "diag")) # distribution if not normal, checkout chapter 8

## Gráfico
ggplot(data = dados_dois_fatores_interacao, 
       aes(y = Tempo, x = Pessoas, color = Idade)) + 
  geom_boxplot() +
  stat_summary(fun = mean, geom ="point", aes(group = Idade, x = Pessoas),
               color = "black",
               position = position_dodge(0.7), size  = 4) +
  geom_link(aes(x = 0.8, y = 31, xend = 1.8, yend = 40), color = "darkorange", 
            lwd  = 1.3, linetype = 2) + 
  geom_link(aes(x = 1.2, y = 19, xend = 2.2, yend = 26.5), 
            color = "cyan4", lwd  = 1.3, linetype = 2) + 
  labs(x = "Sistema XY de determinação do sexo", 
       y = "Tempo (horas) para eliminar a droga") +
  scale_color_manual(values = c("darkorange", "cyan4", "darkorange", "cyan4")) +
  scale_y_continuous(limits = c(10, 50), breaks = c(10, 20, 30, 40, 50)) +
  tema_livro()  


# agora com efeito de interação

## Olhando os dados
head(dados_dois_fatores_interacao2)

## Análise anova de dois fatores 
Modelo_interacao2 <- aov(Tempo ~ Pessoas * Idade, 
                         data = dados_dois_fatores_interacao2)

anova(Modelo_interacao2)

## Gráfico
ggplot(data = dados_dois_fatores_interacao2, 
       aes(y = Tempo, x = Pessoas, color = Idade)) + 
  geom_boxplot() +
  stat_summary(fun = mean, geom ="point", aes(group = Idade, x = Pessoas), 
               color = "black", position = position_dodge(0.7), size  = 4) +
  geom_link(aes(x = 0.8, y = 31, xend = 1.8, yend = 27), color = "darkorange", 
            lwd  = 1.3, linetype = 2) + 
  geom_link(aes(x = 1.2, y = 19, xend = 2.2, yend = 41), color = "cyan4", 
            lwd  = 1.3, linetype = 2) + 
  labs(x = "Sistema XY de determinação do sexo", 
       y = "Tempo (horas) para eliminar a droga") +
  scale_color_manual(values = c("darkorange", "cyan4", "darkorange", "cyan4")) +
  scale_y_continuous(limits = c(10, 50), breaks = c(10, 20, 30, 40, 50)) +
  tema_livro() 

##Exemplo prático 1 - ANOVA em blocos aleatorizados 

## Cabeçalho dos dados
head(dados_bloco) 

## Análise Anova em blocos aleatorizados
model_bloco <- aov(Riqueza ~ Pocas + Error(Blocos), data = dados_bloco)
summary(model_bloco)

## Teste de Tuckey's honest significant difference
pairs(lsmeans(model_bloco, "Pocas"), adjust = "tukey")

# Reordenando a ordem que os grupos irão aparecer no gráfico.
dados_bloco$Pocas <- factor(dados_bloco$Pocas, 
                            levels = c("Int-100m", "Int-50m", "Borda", "Mat-50m", "Mat-100m"))

## Gráfico
ggplot(data = dados_bloco, aes(x = Pocas, y = Riqueza)) + 
  labs(x = "Poças artificiais", y = "Riqueza de espécies de anuros") +
  geom_boxplot(color = "black", show.legend = FALSE, alpha = 0.4) +
  geom_jitter(shape = 16, position = position_jitter(0.1), cex = 4, alpha = 0.7) +
  scale_x_discrete(labels = c("-100m","-50m","Borda", "50m", "100m")) +
  tema_livro() +
  theme(legend.position = "none") 


## ANCOVA 
## 
head(dados_ancova) 

## Ancova
modelo_ancova <- lm(Biomassa ~ Herbivoria * Raiz, data = dados_ancova)

# Verificando as premissas da Ancova
plot_grid(plot_model(modelo_ancova, type = "diag"))

## Resultados do modelo
anova(modelo_ancova)

## Criando modelo sem interação
modelo_ancova2 <- lm(Biomassa ~ Herbivoria + Raiz, data = dados_ancova)

## Likelihood-ratio test
lrtest(modelo_ancova, modelo_ancova2) #fail to reject null hypothesis, simpler model is better

## Gráfico
ggplot(data = dados_ancova, aes(x = Raiz, y = Biomassa, fill = Herbivoria)) + 
  labs(x = "Tamanho da raiz (cm)", y = "Biomassa dos frutos (g)") +
  geom_point(size = 4, shape = 21, alpha = 0.7) +
  scale_colour_manual(values = c("darkorange", "cyan4")) +
  scale_fill_manual(values = c("darkorange", "cyan4"),
                    labels = c("Com herbivoria", "Sem herbivoria")) +
  geom_smooth(aes(color = Herbivoria), method = "lm", show.legend = FALSE) +
  tema_livro()


## GLS -----
## Calcular a riqueza de espécies em cada comunidade
riqueza <- specnumber(mite) 

## Selecionar a variável ambiental - quantidade de água no substrato
agua <- mite.env[,2]

## Criar um data.frame com riqueza, quantidade de água no substrato e coordenadas geográficas
mite_dat <- data.frame(riqueza, agua, coords)

## Modelo
linear_model <- lm(riqueza ~ agua, mite_dat) 

## Resíduos
par(mfrow = c(2, 2)) 
plot(linear_model, which = 1:4)

## Resultados do modelo
res_lm <- summary(linear_model)

## Coeficiente de determinação e coeficientes
res_lm$adj.r.squared

res_lm$coefficients

# 7.1 Avalie se os indivíduos machos de uma espécie de aranha são maiores do que as fêmeas. Qual a sua interpretação sobre o dimorfismo sexual nesta espécie? Faça um gráfico boxplot usando também a função geom_jitter(). Use os dados Cap7_exercicio1 disponível no pacote ecodados.

# visualize data 
head(Cap7_exercicio1)

#create a linear model (lm) to test whether the residuals are normal (shapiro wilk's) and if there is homogeneity of variance (levene)
res_cap7 <- lm(Tamanho ~ Sexo, Cap7_exercicio1)

shapiro.test(residuals(res_cap7)) # normal

leveneTest(Tamanho ~ Sexo, Cap7_exercicio1) # homogeneous

## Análise Teste T 
t.test(Tamanho ~ Sexo, data = Cap7_exercicio1, var.equal = T)

## Gráfico
c7_e1 <- ggplot(data = Cap7_exercicio1, aes(x = Sexo, y = Tamanho)) + 
  labs(x = "Sexo", 
       y = "Tamanho") +
  geom_boxplot(color = "black", 
               outlier.shape = NA) +
  geom_jitter(shape = 16, position = position_jitter(0.1), 
              cex = 5, alpha = 0.7) +
  scale_color_manual(values = c("black", "black")) +
  tema_livro() +
  theme(legend.position = "none")

c7_e1

#sexual dimorphism exists in this species and the females are significantly larger than the males


# 7.2 Avalie se o número de polinizadores visitando uma determinada espécie de planta é dependente da presença ou ausência de predadores. A mesma planta, em tempos diferentes, foi utilizada como unidade amostral para os tratamentos com e sem predadores. Qual a sua interpretação sobre os resultados? Faça um gráfico boxplot ligando os resultados da mesma planta com e sem a presença do predador. Use os dados Cap7_exercicio2 disponível no pacote ecodados.

head(Cap7_exercicio2)

#create a linear model (lm) to test whether the residuals are normal (shapiro wilk's) and if there is homogeneity of variance (levene)
res_cap7_ex2 <- lm(Polinizadores ~ Predadores, Cap7_exercicio2)

shapiro.test(residuals(res_cap7_ex2)) # normal

leveneTest(Polinizadores ~ Predadores, Cap7_exercicio2) # not homogeneous

class(Cap7_exercicio2$Polinizadores)

class(Cap7_exercicio2$Predadores)

t.test(Polinizadores ~ Predadores, paired = TRUE, data = Cap7_exercicio2, var.equal = F)

## Gráfico
ggpaired(Cap7_exercicio2, x = "Predadores", y = "Polinizadores",
         color = "Predadores", line.color = "gray", line.size = 0.8, 
         palette = c("darkorange", "cyan4"), width = 0.5, 
         point.size = 4, xlab = "Predadores", 
         ylab = "Polinizadores") +
  #expand_limits(y = c(20, 40)) +
  tema_livro() 

# 7.3 Avalie se existe correlação entre o número de filhotes nos ninhos de uma espécie de ave com o tamanho do fragmento florestal. Qual a sua interpretação dos resultados? Faça um gráfico mostrando a relação entre as variáveis. Use os dados Cap7_exercicio3 disponível no pacote ecodados.


head(Cap7_exercicio3) 

cor.test(Cap7_exercicio3$Filhotes, Cap7_exercicio3$Fragmentos, method = "kendall")

shapiro.test(Cap7_exercicio3$Filhotes)

hist(Cap7_exercicio3$Filhotes)

shapiro.test(Cap7_exercicio3$Fragmentos)

hist(Cap7_exercicio3$Fragmentos)

## Gráfico
ggplot(data = Cap7_exercicio3, aes(x = Fragmentos, y = Filhotes)) + 
  labs(x = "Número de Filhotes", y = "Fragmentos") +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  #geom_smooth(method = lm, se = T, color = "black", linetype = "dashed") +
  tema_livro() +
  theme(legend.position = "none")


# 7.4 Avalie se a relação entre o tamanho da área de diferentes ilhas e a riqueza de espécies de lagartos. Qual a sua interpretação dos resultados? Faça um gráfico mostrando a relação predita pelo modelo. Use os dados Cap7_exercicio4 disponível no pacote ecodados.
 
head(Cap7_exercicio4)

## regressão simples
modelo_regressao <- lm(Riqueza ~ Area_ilhas, data = Cap7_exercicio4)

## Verificar as premissas do teste
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modelo_regressao)
dev.off() # volta a configuração dos gráficos para o formato padrão 

shapiro.test(residuals(modelo_regressao)) # normal

## Gráfico
ggplot(data = Cap7_exercicio4, aes(x = Area_ilhas, y = Riqueza)) + 
  labs(x = "Area das ilhas", 
       y = "Riqueza de especies") +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  tema_livro() +
  theme(legend.position = "none")

summary(modelo_regressao)

# 7.5 Avalie se existe relação entre a abundância de uma espécie de roedor com o tamanho da área dos fragmentos florestais e/ou a altitude. Faça uma regressão múltipla. Em seguida, crie diferentes modelos e selecione o mais parcimonioso com base nos valores do teste de Likelihood-ratio test (LRT) e Akaike information criterion (AIC). Qual a sua interpretação? Use os dados Cap7_exercicio5 disponível no pacote ecodados.
# 

head(Cap7_exercicio5)

modelo_regressao_mult_sem_interacao <- lm(Abundancia ~ Area_fragmento + 
                                            Altitude,
                                          data = Cap7_exercicio5)
## Regressão múltipla
modelo_regressao_mult <- lm(Abundancia ~ Area_fragmento*Altitude,
                            data = Cap7_exercicio5)

lrtest(modelo_regressao_mult, modelo_regressao_mult_sem_interacao)

vif(modelo_regressao_mult)

## Vamos verificar o modelo só com a altitude 
modelo_regressao_mult_sem_fragmento <- lm(Abundancia ~ Altitude, data = Cap7_exercicio5)

lrtest(modelo_regressao_mult_sem_interacao, modelo_regressao_mult_sem_fragmento)

## Vamos verificar o modelo só com o intercepto
modelo_regressao_mult_nulo <- lm(Abundancia ~ 1, data = Cap7_exercicio5)

lrtest(modelo_regressao_mult_sem_fragmento, modelo_regressao_mult_nulo)

## modelo só com a área do fragmento
modelo_regressao_mult_sem_altitude <- lm(Abundancia ~ Area_fragmento, data = Cap7_exercicio5)

AICc <- ICtab(modelo_regressao_mult, modelo_regressao_mult_sem_interacao, 
              modelo_regressao_mult_sem_fragmento,modelo_regressao_mult_nulo, 
              modelo_regressao_mult_sem_altitude,
              type = c("AIC"), weights = TRUE, 
              delta = TRUE, sort = TRUE)
AICc
