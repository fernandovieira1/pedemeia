### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
# Código Curti: https://github.com/freitascurti/desafio-pe-de-meia/blob/main/probit
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

#### 0. CONFIGURAR AMBIENTE ####
# Carregar pacotes
# Definir colunas pnad
# Criar função pnad

### 0.1 Local de trabalho ####
# local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pé-de-Meia\\BDs Pé-de-Meia'
# setwd(local)

### 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados --> cálculo dos pesos
library(convey) # Cálculo de medidas de desigualdade
library(tidyverse) # Manipulação do df e Gráficos
library(gt)  # Criar Tabelas
library(scales)  # Formatação de gráficos

### 0.3 Notas metodológicas colunas PNADc (IBGE) ####
## - [CHAVE]
# Descrição: Chaves PNADc (domicílio e pessoas)
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Chaves_PNADC.pdf

## - Variáveis derivadas
# Descrição: Códigos dos factors (categorias) das colunas PNADc 
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Definicao_variaveis_derivadas_PNADC_20200211.pdf

## - Deflatores
# Descrição: Deflatores para as variáveis de renda. 
#    * Habitual: rendas ordinárias: salário, alugueis etc.
#    * Efetivo: rendas extraordinárias: 13º salário, férias etc.
# Fontes: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/PNADcIBGE_Deflator_Trimestral.pdf
#         https://www.ibge.gov.br/estatisticas/sociais/trabalho/2511-np-pnad-continua/9173-pesquisa-nacional-por-amostra-de-domicilios-continua-trimestral.html?edicao=38405&t=downloads
#    * Trimestral >>> Microdados >>> Documentacao >>> Deflatores.zip

## - Variáveis PNADc 
# Descrição: Códigos das variáveis PNADc e trimestres e ano em que foram utilizadas
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/PNADcIBGE_Deflator_Trimestral.pdf
#    * Trimestral >>> Microdados >>> Documentacao >> Variaveis_PNADC_Trimestral.xls

## - Dicionários PNADc
# Descrição: Dicionários de variáveis PNADc
# Fonte: 2016: https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fftp.ibge.gov.br%2FTrabalho_e_Rendimento%2FPesquisa_Nacional_por_Amostra_de_Domicilios_continua%2FAnual%2FMicrodados%2FVisita%2FVisita_1%2FDocumentacao%2Fdicionario_PNADC_microdados_2016_visita1_20220224.xls&wdOrigin=BROWSELINK
#        2023: https://1drv.ms/x/s!AqlEsL9Wt3_5ku5rpuaUeN_JjVksCw?e=cgLGME

# Função para carregar dados da PNAD para múltiplos anos e trimestres
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
        dados_completos[[paste(ano, trimestre, sep = "_")]] <- dados_trimestre
      } else {
        warning(paste("Dados não disponíveis para o ano", ano, "e trimestre", trimestre))
      }
    }
  }
  
  # Combina todos os dados em um único df
  dados_final <- bind_rows(dados_completos)
  
  return(dados_final)
}

# Definir anos e trimestres para carregar
anos <- c(2022, 2023)     # Pode especificar um ou mais anos
trimestres <- c(1, 2, 3, 4) # Pode especificar um ou mais trimestres

# Carregar os dados
dados_pnad <- carregar_pnadc_multiplos(anos, trimestres)
table(dados_pnad$Ano)
table(dados_pnad$Trimestre)

variaveis_interesse <- c( # Variáveis de interesse Curti
  "UPA", "V1008", "V1014", "V2003", "V2008", "V20081", "V20082", "V2009",
  "V3003A", "VD2004", "V3002A", "ID_DOMICILIO", "V2001", "VD4016",
  "VD4017", "Trimestre", "VD2003", "VD4019", "V2007", "V3009A","VD2002", "V3006","VD3005", "V2010")

# Filtrar a base de dados
publico_alvo_filtrado <- dados_pnad %>%
  select(all_of(variaveis_interesse))
