# Instalar e carregar pacotes necessários
library(sampleSelection)

# Limpar o ambiente
gc(); cat('\014')

# Filtrar a base de dados para manter apenas o público-alvo
base_heckit <- base_evasao_probit %>%
  na.omit()  # Remover NAs

# Etapa 1: Modelo Probit para seleção
# Definir a variável dependente da seleção (participação no público-alvo)
base_heckit$RDPC_menor_meio_sm <- as.numeric(base_heckit$RDPC_menor_meio_sm)
 
# Ajustar o modelo Probit
modelo_selecao <- glm(evasao ~ V2010 + V2007 + V1022 + V3002A + VD4013 +
                        educ_max_pais + V2009 + V2001 + RDPC,
                      data = base_heckit, family = binomial(link = 'probit'))

# Estimar o inverso da razão de Mills (lambda)
base_heckit$lambda <- dnorm(predict(modelo_selecao, type = 'link')) /
  pnorm(predict(modelo_selecao, type = 'link'))

# Etapa 2: Modelo de resultado ajustado
# Definir a variável dependente do modelo de resultado (por exemplo, escolaridade final)
modelo_resultado <- lm(RDPC ~ V2010 + V2007 + V1022 + VD2004 + educ_media_pais + 
                         V2009 + regiao + VD4013 + lambda,
                       data = base_heckit)

# Resumo dos resultados
summary(modelo_selecao)
summary(modelo_resultado)
