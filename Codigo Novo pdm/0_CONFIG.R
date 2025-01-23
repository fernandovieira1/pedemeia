# Limpar o ambiente
rm(list = ls(all = TRUE)); gc(); cat('\014')

#### INÍCIO ####
# Marcar o início do processamento
ini <- Sys.time()

###################################### I. DEFINIR PERÍODOS ######################################

### Definir período ####
## AVISO: Definir anos inicial e final
## AVISO: A análise da evasão escolar sempre requer mais de um ano e a inclusão do 1º semestre.

## Digite o ano inicial e o final
# Anos disponíveis: de 2015 até 2024.
anos <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

## Digite o trimestre inicial e final
# - P. ex.: todos os trimestres (1, 2, 3, 4); apenas o 3º e 4º trimestres (3, 4) etc.
# - Se a análise for de evasão e não envolver mais de dois anos, recomendada a inclusão 
# apenas do 1º trimestre (1) p/ economizar RAM.
trimestres <- c(1)

################################### II. CONFIGURAR PARÂMETROS ###################################
## Critérios que interferem na qtde. de informações analisadas e no processamento dos scripts.

### Tipo da análise ####
# 'amostra' ou 'censo' (recomendado)
# amostra com 10 mil observações.
tipo_analise <- 'censo' 

### Tipo de processamento ####
# 'original' (recomendado) ou 'compactado' (otimiza o bd sql. DEMORA!).
tipo_processamento <- 'original' 

### Local do banco de dados ####
# Defina aqui: 'salvo' (arquivo feather previamente salvo) ou 'processar' (carregar os dados rds e processá-los).
local_bd <- 'salvo'

### Formato dos arquivos da base de dados ####
# No caso de banco de dados do tipo 'processar'.
# Defina aqui: 'rds' (recomendado) ou 'sql'.
formato_arquivo <- 'rds'

##################################### III. EXECUTAR SCRIPTS #####################################

### Carregar Bibliotecas ####
## AVISO: Não mexer (apenas execute).
source('Codigo Novo pdm\\Script pdm\\0_bibliotecas.R')

### Local de trabalho ####
## AVISO: Verifique abaixo o caminho do arquivo e altere-o (se ainda não o fez).
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'

### Carregar Dados ####
if (local_bd == 'processar') {
  if (tipo_processamento == 'compactado') {
    source('Codigo Novo pdm\\Script pdm\\Docs Acessorios pdm\\0_config_teste.R') # testes
  } else {
    source('Codigo Novo pdm\\Script pdm\\0_config.R')
  }
  source('Codigo Novo pdm\\Script pdm\\0_local.R')
  # Otimizar publico_alvo_filtrado
  source('Codigo Novo pdm\\Script pdm\\0_otimizar.R')
  gc()
} else {
  # Use o caminho completo para o arquivo Feather
  caminho_feather <- file.path(local, '0_publico_alvo_filtrado_2015-2023.feather')
  
  if (!file.exists(caminho_feather)) {
    stop('Arquivo Feather não encontrado: ', caminho_feather)
    gc()
  }
  
  publico_alvo_filtrado <- read_feather(caminho_feather)
}


######################################## IV. RESULTADOS ########################################

## Verificar dados ####
# glimpse(publico_alvo_filtrado)
# table(publico_alvo_filtrado$Ano)
# table(publico_alvo_filtrado$Trimestre)
Sys.sleep(5)
gc()

#### FIM ####
# Marcar o final do processamento
final <- Sys.time()

# Calcular o tempo total de execução
tempo_execucao <- final - ini
print(paste('Tempo de execução (minutos):', round(tempo_execucao, 2)))
