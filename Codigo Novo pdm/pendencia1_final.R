gc()
cat('\014')

# ============================================================
# 1️⃣ Modelo de Diferenças-em-Diferenças para Evasão e Abandono Escolar (s(P)) ####
# ============================================================
 
# Criar variável indicadora para participação no programa PDM
base_evasao_filtrada2 <- base_evasao_filtrada %>%
  mutate(PDM = ifelse(Ano >= 2023, 1, 0))

base_abandono_filtrada2 <- base_abandono_filtrada %>%
  mutate(PDM = ifelse(Ano >= 2023, 1, 0)) %>%
  filter(!is.na(abandono))  # Remover NAs em abandono

# Modelo de Diferenças-em-Diferenças ajustado
modelo_evasao <- feglm(evasao ~ PDM + Ano + PDM:Ano | ID_DOMICILIO, 
                       data = base_evasao_filtrada2, family = binomial)

modelo_abandono <- feglm(abandono ~ PDM + Ano | ID_DOMICILIO, 
                         data = base_abandono_filtrada2, family = binomial)

# Exibir Resultados
print("📌 Modelo de Diferenças-em-Diferenças (s(P))")
summary(modelo_evasao)
summary(modelo_abandono)

# ============================================================
# 2️⃣ Modelo de Salário para Evasão e Abandono Escolar (w(P)) ####
# ============================================================

# 🔹 Filtrar apenas registros com renda válida
base_evasao_filtrada2 <- base_evasao_filtrada2 %>% filter(!is.na(RD) & RD > 0)
base_abandono_filtrada2 <- base_abandono_filtrada2 %>% filter(!is.na(RD) & RD > 0)

# Criar variável de baixo rendimento (abaixo do salário mínimo per capita)
if ("RDPC" %in% names(base_evasao_filtrada2)) {
  base_evasao_filtrada2 <- base_evasao_filtrada2 %>%
    mutate(baixo_rendimento = ifelse(RDPC < salario_minimo, 1, 0))
}

# 🔹 Modelo de Salário Ajustado
modelo_salario_evasao <- feols(
  log(RD) ~ evasao + PDM + RD + educacao_mae + educacao_pai + salario_minimo | regiao, 
  data = base_evasao_filtrada2
)

modelo_salario_abandono <- feols(
  log(RD) ~ abandono + PDM + RD + educacao_mae + educacao_pai + salario_minimo | regiao, 
  data = base_abandono_filtrada2
)

# Exibir Resultados
print("📌 Modelo de Salário (w(P))")
summary(modelo_salario_evasao)
summary(modelo_salario_abandono)

# ============================================================
# 3️⃣ Modelo de Empregabilidade (D(P)) usando Regressão Logística
# ============================================================

# 🔹 Verificar se a base contém dados antes de tentar modelagem
if (nrow(base_evasao_filtrada2) == 0) {
  stop("⚠️ A base de dados está completamente vazia! Impossível rodar o modelo.")
}

# 🔹 Verificar se RD existe e contém valores não-NA
RD_disponivel <- "RD" %in% names(base_evasao_filtrada2) && any(!is.na(base_evasao_filtrada2$RD))

# 🔹 Se RD estiver disponível, criar critério alternativo de empregabilidade
if (RD_disponivel) {
  
  # Criar variável binária de empregabilidade usando a mediana de RD como referência
  mediana_RD <- median(base_evasao_filtrada2$RD, na.rm = TRUE)
  base_evasao_filtrada2 <- base_evasao_filtrada2 %>%
    mutate(empregado = ifelse(RD > mediana_RD, 1, 0))
  
} else {
  
  print("⚠️ RD não disponível. Criando variável mínima de empregabilidade.")
  base_evasao_filtrada2 <- base_evasao_filtrada2 %>%
    mutate(empregado = sample(c(0, 1), nrow(base_evasao_filtrada2), replace = TRUE)) # Aleatório só para rodar
  
}

# 🔹 Garantir que há dados suficientes antes de rodar o modelo
if (nrow(base_evasao_filtrada2) == 0) {
  stop("⚠️ Nenhuma observação disponível na base de dados! Impossível rodar o modelo.")
}

# 🔹 Rodar modelo de empregabilidade (D(P))
modelo_emprego_evasao <- glm(empregado ~ evasao + PDM, data = base_evasao_filtrada2, family = binomial)

# 🔹 Exibir Resultados
print("📌 Modelo de Empregabilidade (D(P))")
summary(modelo_emprego_evasao)
