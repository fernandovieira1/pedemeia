rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM
gc()

library(PNADcIBGE) # Dados PNADc
library(tidyverse) # Manipulação do df e Gráficos

anos <- c(2015, 2023)     # Pode especificar um ou mais anos
trimestres <- c(1, 2, 3, 4) # Pode especificar um ou mais trimestres

## Função para carregar dados de múltiplos anos e trimestres
carregar_pnadc_multiplos <- function(anos, trimestres) {
  # Lista para armazenar dados de cada combinação de ano e trimestre
  dados_completos <- list()
  
  # Loop para cada ano e trimestre
  for (ano in anos) {
    for (trimestre in trimestres) {
      # Tenta carregar os dados do ano e trimestre especificados
      dados_trimestre <- tryCatch({
        # Baixa o objeto de amostra da PNADc
        pnadc_data <- get_pnadc(
          year = ano, 
          quarter = trimestre
        )
        # Extrai os dados do objeto de amostra
        as.data.frame(pnadc_data$variables)
      }, error = function(e) NULL)  # Retorna NULL em caso de erro
      
      # Verifica se o carregamento foi bem-sucedido
      if (!is.null(dados_trimestre)) {
        dados_trimestre <- dados_trimestre %>% mutate(Ano = ano, Trimestre = trimestre)
        dados_completos[[paste(ano, trimestre, sep = '_')]] <- dados_trimestre
      } else {
        warning(paste('Dados não disponíveis para o ano', ano, 'e trimestre', trimestre))
      }
    }
  }
  
  # Combina todos os dados em um único df
  dados_final <- bind_rows(dados_completos)
  
  return(dados_final)
}

## Importar dados
dados_pnad <- carregar_pnadc_multiplos(anos, trimestres)
table(dados_pnad$Ano)
table(dados_pnad$Trimestre)