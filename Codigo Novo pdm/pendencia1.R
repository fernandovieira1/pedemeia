# Instalar pacotes necessários (se não instalados)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("fixest")) install.packages("fixest")  # Regressões robustas
if (!require("did")) install.packages("did")  # Modelo de diferenças-em-diferenças
if (!require("forecast")) install.packages("forecast")  # Projeções
if (!require("glmnet")) install.packages("glmnet")  # Modelos de regularização

# Carregar pacotes
library(tidyverse)
library(fixest)
library(did)
library(forecast)
library(glmnet)

cat('\014')

# 0. ====================================================================== ####
# Criar variável indicadora para participação no programa PDM (já filtrado nas bases)
base_evasao_filtrada2 <- base_evasao_filtrada %>%
  mutate(PDM = ifelse(Ano >= 2023, 1, 0))

base_abandono_filtrada2 <- base_abandono_filtrada %>%
  mutate(PDM = ifelse(Ano >= 2023, 1, 0))
base_abandono_filtrada2 <- base_abandono_filtrada2 %>% filter(!is.na(abandono))


# Verificar estrutura das variáveis dependentes
table(base_evasao_filtrada2$evasao, useNA = "ifany")
table(base_abandono_filtrada2$abandono, useNA = "ifany")

# 1. ====================================================================== ####
# 1. Modelo de Diferenças-em-Diferenças para Evasão e Abandono Escolar (s(P))
modelo_evasao <- feglm(evasao ~ PDM + Ano + PDM * Ano | ID_DOMICILIO, 
                       data = base_evasao_filtrada2, family = binomial)
summary(modelo_evasao)

modelo_abandono <- feglm(abandono ~ PDM + Ano + PDM * Ano | ID_DOMICILIO, 
                         data = base_abandono_filtrada2, family = binomial)
summary(modelo_abandono)

# 2. ====================================================================== ####
## 2. Modelo de Salário para Evasão e abandono Escolar
if ("RD" %in% names(base_evasao_filtrada2)) {
  if (!all(is.na(base_evasao_filtrada2$RD))) {
    modelo_salario_evasao <- feols(
      log(RD) ~ evasao + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
      data = base_evasao_filtrada2)
  } else {
    modelo_salario_evasao <- NULL
    print("RD em base_evasao_filtrada2 está vazia.")
  }
} else {
  print("RD não existe em base_evasao_filtrada2.")
}

# Modelo de Salário para evasão e Abandono Escolar
if ("RD" %in% names(base_abandono_filtrada2)) {
  if (!all(is.na(base_abandono_filtrada2$RD))) {
    modelo_salario_abandono <- feols(
      log(RD) ~ abandono + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
      data = base_abandono_filtrada2)
  } else {
    modelo_salario_abandono <- NULL
    print("RD em base_abandono_filtrada2 está vazia.")
  }
} else {
  print("RD não existe em base_abandono_filtrada2.")
}

# Exibir os resultados
if (!is.null(modelo_salario_evasao)) summary(modelo_salario_evasao)
if (!is.null(modelo_salario_abandono)) summary(modelo_salario_abandono)

# Como muitos NAs foram removidos, vamos investigar o que houve
# Contar quantos NA existem por variável em evasão
na_evasao <- colSums(is.na(base_evasao_filtrada2))

# Contar quantos NA existem por variável em abandono
na_abandono <- colSums(is.na(base_abandono_filtrada2))

# Exibir o total de NA por variável
na_evasao[na_evasao > 0]
na_abandono[na_abandono > 0]

# Filtrar observações onde RD está ausente na base de evasão
na_rd_evasao <- base_evasao_filtrada2 %>% filter(is.na(RD))

# Filtrar observações onde RD está ausente na base de abandono
na_rd_abandono <- base_abandono_filtrada2 %>% filter(is.na(RD))

# Contar quantos NA por UF (Estado) para verificar padrão geográfico
table(na_rd_evasao$regiao)
table(na_rd_abandono$regiao)

# Contar quantos NA por Regiões (caso tenha essa variável)
table(na_rd_evasao$regiao)
table(na_rd_abandono$regiao)

# Verificar se os NA ocorrem mais em famílias de baixa escolaridade dos pais
summary(na_rd_evasao$educacao_mae)
summary(na_rd_abandono$educacao_mae)

summary(na_rd_evasao$educacao_pai)
summary(na_rd_abandono$educacao_pai)

# Verificar distribuição de evasão e abandono entre os NA
table(na_rd_evasao$evasao)
table(na_rd_abandono$abandono)

# Verificar se as observações com NA em RD também têm NA em VD4020
table(is.na(na_rd_evasao$VD4020))
table(is.na(na_rd_abandono$VD4020))

# Os NA em RD estão fortemente correlacionados com VD4020 (renda do trabalho), sugerindo que muitas dessas famílias não tinham renda formal declarada.

# Criar variável indicadora de famílias sem renda declarada
base_evasao_filtrada2 <- base_evasao_filtrada2 %>%
  mutate(sem_renda_dec = ifelse(is.na(RD), 1, 0))

base_abandono_filtrada2 <- base_abandono_filtrada2 %>%
  mutate(sem_renda_dec = ifelse(is.na(RD), 1, 0))

# Modelo de Salário para Evasão Escolar
if ("RD" %in% names(base_evasao_filtrada2)) {
  if (!all(is.na(base_evasao_filtrada2$RD))) {
    modelo_salario_evasao <- feols(
      log(RD) ~ evasao + PDM + sem_renda_dec + VD4020 + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
      data = base_evasao_filtrada2)
  } else {
    modelo_salario_evasao <- NULL
    print("RD em base_evasao_filtrada2 está vazia.")
  }
} else {
  print("RD não existe em base_evasao_filtrada2.")
}

# Modelo de Salário para Abandono Escolar
if ("RD" %in% names(base_abandono_filtrada2)) {
  if (!all(is.na(base_abandono_filtrada2$RD))) {
    modelo_salario_abandono <- feols(
      log(RD) ~ abandono + PDM + sem_renda_dec + VD4020 + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
      data = base_abandono_filtrada2)
  } else {
    modelo_salario_abandono <- NULL
    print("RD em base_abandono_filtrada2 está vazia.")
  }
} else {
  print("RD não existe em base_abandono_filtrada2.")
}

# Exibir os resultados
if (!is.null(modelo_salario_evasao)) summary(modelo_salario_evasao)
if (!is.null(modelo_salario_abandono)) summary(modelo_salario_abandono)

# podemos testar sem_renda_dec interagindo com PDM, para ver se o programa ajuda mais as famílias sem renda declarada.
# Modelo de Salário para Evasão Escolar com Interação
if ("RD" %in% names(base_evasao_filtrada2)) {
  if (!all(is.na(base_evasao_filtrada2$RD))) {
    modelo_salario_evasao_interacao <- feols(
      log(RD) ~ evasao + PDM + sem_renda_dec + PDM * sem_renda_dec + VD4020 + 
        educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
      data = base_evasao_filtrada2)
  } else {
    modelo_salario_evasao_interacao <- NULL
    print("RD em base_evasao_filtrada2 está vazia.")
  }
} else {
  print("RD não existe em base_evasao_filtrada2.")
}

# Modelo de Salário para Abandono Escolar com Interação
if ("RD" %in% names(base_abandono_filtrada2)) {
  if (!all(is.na(base_abandono_filtrada2$RD))) {
    modelo_salario_abandono_interacao <- feols(
      log(RD) ~ abandono + PDM + sem_renda_dec + PDM * sem_renda_dec + VD4020 + 
        educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
      data = base_abandono_filtrada2)
  } else {
    modelo_salario_abandono_interacao <- NULL
    print("RD em base_abandono_filtrada2 está vazia.")
  }
} else {
  print("RD não existe em base_abandono_filtrada2.")
}

# Exibir os resultados
if (!is.null(modelo_salario_evasao_interacao)) summary(modelo_salario_evasao_interacao)
if (!is.null(modelo_salario_abandono_interacao)) summary(modelo_salario_abandono_interacao)

# Diagnosticar casusas da colinearidade
# Verificar colinearidade dos termos no modelo
summary(modelo_salario_evasao_interacao)$collin.var
summary(modelo_salario_abandono_interacao)$collin.var

# Correlação entre PDM e sem_renda_dec
# cor(base_evasao_filtrada2$PDM, base_evasao_filtrada2$sem_renda_dec, use = "complete.obs")

# Correlação entre PDM e PDM * sem_renda_dec
# cor(base_evasao_filtrada2$PDM, base_evasao_filtrada2$PDM * base_evasao_filtrada2$sem_renda_dec, use = "complete.obs")

# O problema não é colinearidade, mas falta de variação!
table(base_evasao_filtrada2$PDM, base_evasao_filtrada2$sem_renda_dec, useNA = "ifany")
table(base_abandono_filtrada2$PDM, base_abandono_filtrada2$sem_renda_dec, useNA = "ifany")

#Rodar Modelos Separados para Famílias com e Sem Renda Declarada
# Criar bases separadas
base_evasao_com_renda <- base_evasao_filtrada2 %>% filter(sem_renda_dec == 0)
base_evasao_sem_renda <- base_evasao_filtrada2 %>% filter(sem_renda_dec == 1)

base_abandono_com_renda <- base_abandono_filtrada2 %>% filter(sem_renda_dec == 0)
base_abandono_sem_renda <- base_abandono_filtrada2 %>% filter(sem_renda_dec == 1)

# Modelo para quem tem renda declarada
modelo_salario_evasao_com_renda <- feols(
  log(RD) ~ evasao + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_evasao_com_renda
)

modelo_salario_abandono_com_renda <- feols(
  log(RD) ~ abandono + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_abandono_com_renda
)

# Modelo para quem não tem renda declarada
modelo_salario_evasao_sem_renda <- feols(
  log(RD) ~ evasao + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao,
  data = base_evasao_sem_renda
)

modelo_salario_abandono_sem_renda <- feols(
  log(RD) ~ abandono + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_abandono_sem_renda
)

# Exibir os resultados
summary(modelo_salario_evasao_com_renda)
summary(modelo_salario_evasao_sem_renda)

summary(modelo_salario_abandono_com_renda)
summary(modelo_salario_abandono_sem_renda)

#Diagnóstico: Modelos para Famílias Sem Renda Declarada Não Rodaram
dim(base_evasao_sem_renda)  # Deve mostrar número de observações
dim(base_abandono_sem_renda)

sum(is.na(base_evasao_sem_renda$RD))  # Deve ser igual ao número de linhas da base
sum(is.na(base_abandono_sem_renda$RD))

#Manter sem_renda_dec no modelo geral para todas as observações:
modelo_salario_evasao <- feols(
  log(RD) ~ evasao + PDM + sem_renda_dec + VD4020 + 
    educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_evasao_filtrada2
)
summary(modelo_salario_evasao)

modelo_salario_abandono <- feols(
  log(RD) ~ abandono + PDM + sem_renda_dec + VD4020 + 
    educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_abandono_filtrada2
)
summary(modelo_salario_abandono)

#Conferir se sem_renda_dec foi corretamente incluído no modelo
summary(modelo_salario_evasao)$coeftable
summary(modelo_salario_abandono)$coeftable

modelo_salario_evasao <- feols(
  log(RD) ~ evasao + PDM + sem_renda_dec + VD4020 + 
    educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_evasao_filtrada2
)
summary(modelo_salario_evasao)

modelo_salario_evasao_interacao <- feols(
  log(RD) ~ evasao + PDM + sem_renda_dec + PDM * sem_renda_dec + VD4020 + 
    educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_evasao_filtrada2
)
summary(modelo_salario_evasao_interacao)

modelo_salario_abandono_interacao <- feols(
  log(RD) ~ abandono + PDM + sem_renda_dec + PDM * sem_renda_dec + VD4020 + 
    educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_abandono_filtrada2
)
summary(modelo_salario_abandono_interacao)

#Modelo Logístico para Probabilidade de Ter Renda Declarada
modelo_prob_renda <- feglm(sem_renda_dec ~ PDM + evasao + 
                             educacao_mae + educacao_pai + salario_minimo + VD4020, 
                           data = base_evasao_filtrada2, family = binomial)

summary(modelo_prob_renda)

#Criar um Novo Modelo de Salário Apenas para Quem Tem Renda Declarada
modelo_salario_evasao_ajustado <- feols(
  log(RD) ~ evasao + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_evasao_filtrada2 %>% filter(RD > 0)
)

summary(modelo_salario_evasao_ajustado)

#Testar um Modelo de Sobrevivência para o Tempo até Declarar Renda
library(survival)

modelo_sobrevivencia <- coxph(Surv(Ano, sem_renda_dec) ~ PDM + evasao + educacao_mae + educacao_pai, 
                              data = base_evasao_filtrada2)

summary(modelo_sobrevivencia)

##Modelo Final: Salário para Evasão Escolar
modelo_salario_evasao_ajustado <- feols(
  log(RD) ~ evasao + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_evasao_filtrada2 %>% filter(!is.na(RD) & RD > 0)
)

summary(modelo_salario_evasao_ajustado)

##Modelo Final: Salário para Abandono Escolar
modelo_salario_abandono_ajustado <- feols(
  log(RD) ~ abandono + PDM + RD + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_abandono_filtrada2 %>% filter(!is.na(RD) & RD > 0)
)

summary(modelo_salario_abandono_ajustado)

##Análise Adicional: Impacto do Programa nas Famílias com Baixa Renda
#Para entender se o Pé de Meia teve maior impacto entre os mais vulneráveis, incluímos a variável baixo_rendimento (indivíduos com RDPC abaixo do salário mínimo).
base_evasao_filtrada2 <- base_evasao_filtrada2 %>%
  mutate(baixo_rendimento = ifelse(RDPC < salario_minimo, 1, 0))

modelo_salario_evasao_interacao <- feols(
  log(RD) ~ evasao + PDM + baixo_rendimento + PDM * baixo_rendimento + 
    VD4020 + educacao_mae + educacao_pai + salario_minimo + V2001 | regiao, 
  data = base_evasao_filtrada2 %>% filter(!is.na(RD) & RD > 0)
)

summary(modelo_salario_evasao_interacao)


# 3. ====================================================================== ####
# 3. Modelo de Empregabilidade (D(P)) usando Regressão Logística

# 🔹 Criar variável binária de empregabilidade com base em RD (Renda)
if ("RD" %in% names(base_evasao_filtrada2)) {
  base_evasao_filtrada2 <- base_evasao_filtrada2 %>%
    mutate(empregado = ifelse(RD > 0, 1, 0))
} else {
  stop("Erro: A variável RD não está presente na base de dados.")
}

# 🔹 Criar variável de baixa renda (caso ainda não exista)
if (!"baixo_rendimento" %in% names(base_evasao_filtrada2)) {
  base_evasao_filtrada2 <- base_evasao_filtrada2 %>%
    mutate(baixo_rendimento = ifelse(RDPC < salario_minimo, 1, 0))
}

# 🔹 Modelo de Empregabilidade Ajustado (D(P)) usando Regressão Logística
modelo_emprego_evasao <- glm(
  empregado ~ evasao + PDM + evasao:PDM + RD + salario_minimo + 
    educacao_mae + educacao_pai + baixo_rendimento,
  data = base_evasao_filtrada2,
  family = binomial
)

# 🔹 Fazer projeções de salários futuros usando séries temporais (ARIMA)
if (nrow(base_evasao_filtrada2) > 0) {
  serie_salario_evasao <- ts(base_evasao_filtrada2$RD, 
                             start = min(base_evasao_filtrada2$Ano, na.rm = TRUE), 
                             frequency = 1)
  
  modelo_arima_evasao <- auto.arima(serie_salario_evasao)
  previsao_salario_evasao <- forecast(modelo_arima_evasao, h = 5)  # Previsão para 5 anos
} else {
  previsao_salario_evasao <- NULL
  print("RD em base_evasao_filtrada2 está vazia. Sem projeção possível.")
}

# 🔹 Exibir resultados
print("📌 Resultados do Modelo de Empregabilidade:")
summary(modelo_emprego_evasao)

print("📌 Resultados do Modelo ARIMA de Salário:")
if (!is.null(previsao_salario_evasao)) {
  plot(previsao_salario_evasao, main = "Projeção de Salário Futuro - Evasão")
}
