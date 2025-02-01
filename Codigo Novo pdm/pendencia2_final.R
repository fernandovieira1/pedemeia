gc()
cat('\014')

# Definir a estimativa de estudantes do ensino médio na rede pública no Brasil
total_estudantes_brasil <- 6967905  # https://www.gov.br/inep/pt-br/assuntos/noticias/censo-escolar/escolas-publicas-atendem-45-milhoes-de-alunos-no-brasil#:~:text=No%20ensino%20m%C3%A9dio%2C%20dos%206%2C9%20milh%C3%B5es%20de%20alunos,alunos%20matriculados%2C%2076%25%20se%20concentram%20na%20rede%20p%C3%BAblica.

# Passo 1: Benefícios do Programa

# Calcular a evasão antes e depois do programa
evasao_antes <- base_evasao_filtrada %>%
  filter(Ano < 2023) %>%
  summarise(taxa_evasao_antes = mean(evasao, na.rm = TRUE)) %>%
  pull(taxa_evasao_antes)

evasao_depois <- base_evasao_filtrada %>%
  filter(Ano >= 2023) %>%
  summarise(taxa_evasao_depois = mean(evasao, na.rm = TRUE)) %>%
  pull(taxa_evasao_depois)

# Redução na evasão
reducao_evasao <- evasao_antes - evasao_depois

# Calcular a proporção de alunos do ensino médio público elegíveis ao programa
proporcao_beneficiados <- base_evasao_filtrada %>%
  filter(ensino_medio == 1, RDPC_menor_meio_sm == 1) %>%
  summarise(proporcao = n() / nrow(base_evasao_filtrada)) %>%
  pull(proporcao)

# Estimar o número total de beneficiados no Brasil
total_beneficiados_br <- round(total_estudantes_brasil * proporcao_beneficiados)

# Impacto da Redução da Evasão na Renda

renda_antes <- base_evasao_filtrada %>%
  filter(Ano < 2023) %>%
  summarise(renda_media_antes = mean(VD4017, na.rm = TRUE)) %>%
  pull(renda_media_antes)

renda_depois <- base_evasao_filtrada %>%
  filter(Ano >= 2023) %>%
  summarise(renda_media_depois = mean(VD4017, na.rm = TRUE)) %>%
  pull(renda_media_depois)

# Incremento de renda médio por beneficiado
incremento_renda <- renda_depois - renda_antes

# Benefícios totais gerados pelo programa ajustados para a população-alvo no Brasil
beneficios_totais_br <- total_beneficiados_br * incremento_renda * 12  # Considerando um ano

# Passo 2: Custos do Programa
custo_por_aluno <- 1800  # Valor médio do benefício anual por aluno
custo_total_programa_br <- total_beneficiados_br * custo_por_aluno

# Passo 3: Cálculo do ROI Ajustado
ROI_br <- (beneficios_totais_br - custo_total_programa_br) / custo_total_programa_br

# Exibir os resultados ajustados para a população brasileira
resultados_ajustados <- data.frame(
  Reducao_Evasao = reducao_evasao,
  Incremento_Renda = incremento_renda,
  Beneficiados_Estimados = total_beneficiados_br,
  Beneficios_Totais_Ajustados = beneficios_totais_br,
  Custo_Programa_Ajustado = custo_total_programa_br,
  ROI_Ajustado = ROI_br
)

print(resultados_ajustados)
