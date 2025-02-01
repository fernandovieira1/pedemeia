gc()
cat('\014')

# Parâmetros baseados nos resultados ajustados
total_estudantes_brasil <- 6967905  # Total de alunos do ensino médio público
proporcao_beneficiados <- 675446 / total_estudantes_brasil  # Proporção estimada de beneficiados
custo_por_aluno <- 1800  # Benefício anual por aluno

# Função para calcular ROI em diferentes cenários
calcular_ROI <- function(reducao_evasao, incremento_renda) {
  
  # Número estimado de beneficiados no Brasil
  total_beneficiados_br <- round(total_estudantes_brasil * proporcao_beneficiados)
  
  # Benefícios totais gerados
  beneficios_totais_br <- total_beneficiados_br * incremento_renda * 12  # Renda anualizada
  
  # Custo total do programa
  custo_total_programa_br <- total_beneficiados_br * custo_por_aluno
  
  # Cálculo do ROI
  ROI_br <- (beneficios_totais_br - custo_total_programa_br) / custo_total_programa_br
  
  return(ROI_br)
}

# Simulações para diferentes cenários
cenarios <- data.frame(
  Cenário = c("Base", "Cenário Pessimista", "Cenário Otimista"),
  Reducao_Evasao = c(-0.0503, -0.0300, -0.0700),  # Redução da evasão menor e maior
  Incremento_Renda = c(466.16, 350, 600)  # Diferentes incrementos de renda mensal
)

# Aplicar a função para calcular o ROI em cada cenário
cenarios$ROI_Ajustado <- mapply(calcular_ROI, cenarios$Reducao_Evasao, cenarios$Incremento_Renda)

# Exibir resultados
print(cenarios)
