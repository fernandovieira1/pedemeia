# Pacotes necessários
library(dplyr)

# Passo 1: Benefícios do Programa
# Vamos calcular a evasão antes e depois do programa
evasao_antes <- base_evasao_filtrada %>%
  filter(Ano < 2023) %>%
  summarise(taxa_evasao_antes = mean(evasao, na.rm = TRUE))

evasao_depois <- base_evasao_filtrada %>%
  filter(Ano >= 2023) %>%
  summarise(taxa_evasao_depois = mean(evasao, na.rm = TRUE))

# Redução na evasão
reducao_evasao <- evasao_antes$taxa_evasao_antes - evasao_depois$taxa_evasao_depois

# Impacto da Redução da Evasão na Renda
# Consideramos que a evasão reduz a chance de empregabilidade e o salário futuro
# Vamos comparar os rendimentos médios antes e depois do programa

renda_antes <- base_evasao_filtrada %>%
  filter(Ano < 2023) %>%
  summarise(renda_media_antes = mean(VD4017, na.rm = TRUE))

renda_depois <- base_evasao_filtrada %>%
  filter(Ano >= 2023) %>%
  summarise(renda_media_depois = mean(VD4017, na.rm = TRUE))

# Incremento de renda médio por beneficiado
incremento_renda <- renda_depois$renda_media_depois - renda_antes$renda_media_antes
 
# Número de beneficiados
num_beneficiados <- base_evasao_filtrada %>%
  filter(Ano >= 2023, ensino_medio == 1) %>%
  summarise(total = n()) %>%
  pull(total)

# Benefícios totais gerados pelo programa
beneficios_totais <- num_beneficiados * incremento_renda * 12  # Considerando um ano

# Passo 2: Custos do Programa
# Valor médio do benefício anual pago por aluno (supondo R$1.800,00 ao ano por aluno)
custo_por_aluno <- 1800
custo_total_programa <- num_beneficiados * custo_por_aluno

# Passo 3: Cálculo do ROI
ROI <- (beneficios_totais - custo_total_programa) / custo_total_programa

# Exibir os resultados
resultados <- data.frame(
  Reducao_Evasao = reducao_evasao,
  Incremento_Renda = incremento_renda,
  Beneficiados = num_beneficiados,
  Beneficios_Totais = beneficios_totais,
  Custo_Programa = custo_total_programa,
  ROI = ROI
)

print(resultados)
